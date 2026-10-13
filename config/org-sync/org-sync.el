;;; org-sync.el --- Record ~/org in git while Syncthing syncs its files -*- lexical-binding: t; -*-

;; Generated from org-sync.org -- edit that file, not this one.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar jf/org-directory)
(defvar jf/data-profile)
(defvar jf/emacs-dir)

(declare-function magit-mode-get-buffers "magit-mode")
(declare-function magit-refresh-buffer "magit-mode")

(defgroup jf/org-sync nil
  "Record the org directory in git while Syncthing syncs its files."
  :group 'org)

(defcustom jf/org-sync-remote "origin"
  "Remote that holds the canonical history of the org directory."
  :type 'string)

(defcustom jf/org-sync-branch "main"
  "Branch the org directory must be on, locally and on the remote."
  :type 'string)

(defcustom jf/org-sync-idle-seconds 300
  "Seconds of idleness after which `jf/org-sync-mode' records the org directory."
  :type 'integer)

(defcustom jf/org-sync-min-interval 900
  "Fewest seconds between two runs started by `jf/org-sync-mode'."
  :type 'integer)

(defcustom jf/org-sync-max-attempts 3
  "Rounds of fetch, reset, commit and push one run tries before giving up."
  :type 'integer)

(defcustom jf/org-sync-retry-delay 2
  "Seconds to wait before retrying a round."
  :type 'number)

(defvar jf/org-sync-directory nil
  "Directory to record, or nil for `jf/org-directory'.")

(defvar jf/org-sync--running nil
  "Non-nil while a run is in flight.")

(defvar jf/org-sync--last-run nil
  "`float-time' at which the last run was asked for.")

(defvar jf/org-sync--timer nil
  "Idle timer installed by `jf/org-sync-mode'.")

(defvar jf/org-sync-last-result nil
  "Plist describing the last finished run: :time, :status and :message.
:status is one of `committed', `pushed', `nothing', `refused', `busy'
or `failed'.")

(defun jf/org-sync--directory ()
  "Return the directory to record, as a directory name."
  (file-name-as-directory
   (expand-file-name (or jf/org-sync-directory jf/org-directory))))

(defun jf/org-sync--upstream ()
  "Return the remote-tracking ref of the canonical branch, e.g. origin/main."
  (concat jf/org-sync-remote "/" jf/org-sync-branch))

(defun jf/org-sync--log (format-string &rest args)
  "Append FORMAT-STRING applied to ARGS to the *org-sync* buffer; return it."
  (let ((text (apply #'format format-string args)))
    (with-current-buffer (get-buffer-create "*org-sync*")
      (goto-char (point-max))
      (insert (format-time-string "%F %T  ") text "\n"))
    text))

(defun jf/org-sync--git-string (dir &rest args)
  "Run git ARGS in DIR and return its trimmed output, or nil if it fails.
Only for fast local queries; anything touching the network goes
through `jf/org-sync--git'."
  (with-temp-buffer
    (let ((default-directory dir))
      (when (eql 0 (apply #'process-file "git" nil '(t nil) nil args))
        (string-trim (buffer-string))))))

(defun jf/org-sync--git (dir args callback)
  "Run git with ARGS in DIR asynchronously.
When git exits, call CALLBACK with its exit status and its trimmed
combined output."
  (let ((buffer (generate-new-buffer " *org-sync-git*"))
        (default-directory dir)
        (process-environment
         (append '("GIT_TERMINAL_PROMPT=0"
                   "GIT_SSH_COMMAND=ssh -o BatchMode=yes")
                 process-environment)))
    (make-process
     :name "org-sync-git"
     :buffer buffer
     :command (cons "git" args)
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let ((output (with-current-buffer buffer
                         (string-trim (buffer-string)))))
           (kill-buffer buffer)
           (funcall callback (process-exit-status process) output)))))))

(defun jf/org-sync--operation-in-progress-p (dir)
  "Non-nil when the repository at DIR is mid merge, rebase, cherry-pick or revert."
  (let ((git-dir (jf/org-sync--git-string dir "rev-parse" "--absolute-git-dir")))
    (and git-dir
         (cl-some (lambda (name) (file-exists-p (expand-file-name name git-dir)))
                  '("MERGE_HEAD" "rebase-merge" "rebase-apply"
                    "CHERRY_PICK_HEAD" "REVERT_HEAD")))))

(defun jf/org-sync--refusal (dir)
  "Return why DIR must not be recorded, as a string, or nil when it may be."
  (let ((kind (plist-get (bound-and-true-p jf/data-profile) :kind)))
    (cond
     ((and kind (not (eq kind 'live)))
      (format "data profile is %s, not live" kind))
     ((not (file-directory-p dir))
      (format "%s does not exist" dir))
     ((not (equal (jf/org-sync--git-string dir "rev-parse" "--show-toplevel")
                  (directory-file-name (file-truename dir))))
      (format "%s is not the top of a git repository" dir))
     ((not (equal (jf/org-sync--git-string dir "symbolic-ref" "--short" "HEAD")
                  jf/org-sync-branch))
      (format "%s is not on branch %s" dir jf/org-sync-branch))
     ((jf/org-sync--operation-in-progress-p dir)
      (format "%s has a merge, rebase, cherry-pick or revert in progress" dir)))))

(defun jf/org-sync--fetch (ctx)
  "Start a round for CTX by fetching the canonical branch."
  (jf/org-sync--git
   (plist-get ctx :dir)
   (list "fetch" "--quiet" jf/org-sync-remote jf/org-sync-branch)
   (lambda (status output)
     (if (zerop status)
         (jf/org-sync--align ctx)
       (jf/org-sync--finish
        ctx 'failed (format "fetch failed, nothing recorded: %s" output))))))

(defun jf/org-sync--align (ctx)
  "Base HEAD on the upstream for CTX without touching the working tree.
Keep HEAD when it already contains the upstream; otherwise reset."
  (jf/org-sync--git
   (plist-get ctx :dir)
   (list "merge-base" "--is-ancestor" (jf/org-sync--upstream) "HEAD")
   (lambda (status _output)
     (if (zerop status)
         (jf/org-sync--stage ctx)
       (jf/org-sync--reset ctx)))))

(defun jf/org-sync--reset (ctx)
  "Move the branch and index for CTX to the upstream, leaving files alone."
  (jf/org-sync--git
   (plist-get ctx :dir)
   (list "reset" "--quiet" "--mixed" (jf/org-sync--upstream))
   (lambda (status output)
     (if (zerop status)
         (jf/org-sync--stage ctx)
       (jf/org-sync--retry-or-fail ctx (format "reset failed: %s" output))))))

(defun jf/org-sync--stage (ctx)
  "Stage every change in the working tree for CTX."
  (jf/org-sync--git
   (plist-get ctx :dir)
   '("add" "--all")
   (lambda (status output)
     (if (zerop status)
         (jf/org-sync--changes ctx)
       (jf/org-sync--retry-or-fail ctx (format "add failed: %s" output))))))

(defun jf/org-sync--changes (ctx)
  "Commit the staged changes for CTX, or go straight to the ahead check."
  (jf/org-sync--git
   (plist-get ctx :dir)
   '("diff" "--cached" "--name-status")
   (lambda (status output)
     (cond
      ((not (zerop status))
       (jf/org-sync--retry-or-fail ctx (format "diff failed: %s" output)))
      ((string-empty-p output) (jf/org-sync--ahead ctx))
      (t (jf/org-sync--commit ctx output))))))

(defun jf/org-sync--message-body (changes)
  "Return a commit body listing CHANGES, git's name-status output, capped at 50."
  (let* ((lines (split-string changes "\n" t))
         (shown (seq-take lines 50))
         (more (- (length lines) (length shown))))
    (concat (string-join shown "\n")
            (if (> more 0) (format "\n... and %d more" more) ""))))

(defun jf/org-sync--host ()
  "Return this machine's short host name."
  (car (split-string (system-name) "\\.")))

(defun jf/org-sync--commit (ctx changes)
  "Commit the staged CHANGES for CTX."
  (jf/org-sync--git
   (plist-get ctx :dir)
   (list "commit" "--quiet"
         "-m" (format "Sync notes from %s" (jf/org-sync--host))
         "-m" (jf/org-sync--message-body changes))
   (lambda (status output)
     (if (zerop status)
         (jf/org-sync--push
          (plist-put (copy-sequence ctx) :committed
                     (length (split-string changes "\n" t))))
       (jf/org-sync--retry-or-fail ctx (format "commit failed: %s" output))))))

(defun jf/org-sync--ahead (ctx)
  "Push for CTX when HEAD has commits the upstream lacks; else finish."
  (jf/org-sync--git
   (plist-get ctx :dir)
   (list "rev-list" "--count" (concat (jf/org-sync--upstream) "..HEAD"))
   (lambda (status output)
     (cond
      ((not (zerop status))
       (jf/org-sync--retry-or-fail ctx (format "rev-list failed: %s" output)))
      ((equal output "0")
       (jf/org-sync--finish ctx 'nothing "nothing to record"))
      (t (jf/org-sync--push ctx))))))

(defun jf/org-sync--push (ctx)
  "Push HEAD for CTX to the canonical branch."
  (jf/org-sync--git
   (plist-get ctx :dir)
   (list "push" "--quiet" jf/org-sync-remote
         (concat "HEAD:refs/heads/" jf/org-sync-branch))
   (lambda (status output)
     (let ((files (plist-get ctx :committed)))
       (cond
        ((not (zerop status))
         (jf/org-sync--retry-or-fail ctx (format "push failed: %s" output)))
        (files
         (jf/org-sync--finish
          ctx 'committed (format "recorded %d file%s and pushed"
                                 files (if (= files 1) "" "s"))))
        (t (jf/org-sync--finish ctx 'pushed "pushed earlier local commits")))))))

(defun jf/org-sync--retry-or-fail (ctx reason)
  "Start another round for CTX after REASON, or fail when out of attempts."
  (let ((attempt (plist-get ctx :attempt)))
    (if (>= attempt jf/org-sync-max-attempts)
        (jf/org-sync--finish
         ctx 'failed (format "gave up after %d attempts: %s" attempt reason))
      (jf/org-sync--log "attempt %d: %s; retrying" attempt reason)
      ;; :committed survives the retry: the next round either keeps this
      ;; round's commit (the remote did not move) or redoes it.
      (run-at-time jf/org-sync-retry-delay nil #'jf/org-sync--fetch
                   (plist-put (copy-sequence ctx) :attempt (1+ attempt))))))

(defun jf/org-sync--refresh-magit (dir)
  "Refresh the Magit buffers of the repository at DIR."
  (when (fboundp 'magit-mode-get-buffers)
    (let ((default-directory dir))
      (dolist (buffer (ignore-errors (magit-mode-get-buffers)))
        (with-current-buffer buffer
          (ignore-errors (magit-refresh-buffer)))))))

(defun jf/org-sync--finish (ctx status text)
  "End the run for CTX with STATUS and TEXT: record, log, report, call back."
  (setq jf/org-sync--running nil
        jf/org-sync-last-result (list :time (current-time)
                                      :status status :message text))
  (jf/org-sync--log "%s: %s" status text)
  (when (or (memq status '(committed pushed failed))
            (plist-get ctx :verbose))
    (message "org-sync: %s" text))
  (when (memq status '(committed pushed))
    (jf/org-sync--refresh-magit (plist-get ctx :dir)))
  (when-let ((callback (plist-get ctx :callback)))
    (funcall callback status text)))

(defun jf/org-sync-run (&optional verbose callback)
  "Record the org directory in git and push it, asynchronously.
With VERBOSE, report every outcome in the echo area, not only commits
and failures.  When the run ends, call CALLBACK with its status
symbol and a message; see `jf/org-sync-last-result'."
  (let* ((dir (jf/org-sync--directory))
         (ctx (list :dir dir :attempt 1 :verbose verbose :callback callback))
         (refusal (and (not jf/org-sync--running) (jf/org-sync--refusal dir))))
    (setq jf/org-sync--last-run (float-time))
    (cond
     (jf/org-sync--running
      (when verbose (message "org-sync: a run is already in progress"))
      (when callback (funcall callback 'busy "a run is already in progress")))
     (refusal (jf/org-sync--finish ctx 'refused refusal))
     (t (setq jf/org-sync--running t)
        (jf/org-sync--fetch ctx)))))

(defun jf/org-sync-now ()
  "Record the org directory in git and push it now."
  (interactive)
  (jf/org-sync-run t))

(defun jf/org-sync--idle-run ()
  "Run from the idle timer, unless the last run was too recent."
  (when (or (null jf/org-sync--last-run)
            (>= (- (float-time) jf/org-sync--last-run) jf/org-sync-min-interval))
    (jf/org-sync-run)))

(define-minor-mode jf/org-sync-mode
  "Record the org directory in git whenever Emacs has been idle a while.
See `jf/org-sync-idle-seconds' and `jf/org-sync-min-interval'."
  :global t
  :group 'jf/org-sync
  (when jf/org-sync--timer
    (cancel-timer jf/org-sync--timer)
    (setq jf/org-sync--timer nil))
  (when jf/org-sync-mode
    (setq jf/org-sync--timer
          (run-with-idle-timer jf/org-sync-idle-seconds t #'jf/org-sync--idle-run))))

(defun jf/org-sync--production-p ()
  "Non-nil when this Emacs runs from the production tree, ~/.emacs.d."
  (and (boundp 'jf/emacs-dir) jf/emacs-dir
       (file-equal-p jf/emacs-dir (expand-file-name "~/.emacs.d/"))))

(when (and (jf/org-sync--production-p)
           (memq (plist-get (bound-and-true-p jf/data-profile) :kind) '(nil live)))
  (jf/org-sync-mode 1))

(provide 'org-sync)
;;; org-sync.el ends here
