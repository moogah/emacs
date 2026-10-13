;;; org-sync-spec.el --- jf/org-sync: git records, Syncthing moves files -*- lexical-binding: t; -*-

;; Every spec builds throwaway repositories under `temporary-file-directory':
;; a bare "remote" standing in for GitHub and one or two clones standing in
;; for laptops.  Syncthing is simulated by writing the same file into both
;; clones.  The real ~/org is never touched: `jf/org-sync-directory' is
;; always bound to a clone.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'org-sync)

(defvar jf/data-profile)
(defvar jf/emacs-dir)

;;;; Fixture

(defvar org-sync-spec--root nil
  "Temporary directory holding the current spec's repositories.")

(defun org-sync-spec--git (dir &rest args)
  "Run git ARGS in DIR synchronously; return trimmed stdout, signal on failure."
  (with-temp-buffer
    (let* ((default-directory dir)
           (status (apply #'process-file "git" nil t nil args)))
      (unless (eql status 0)
        (error "git %s in %s failed: %s" (string-join args " ") dir (buffer-string)))
      (string-trim (buffer-string)))))

(defun org-sync-spec--write (dir file content)
  "Write CONTENT to FILE under DIR, creating directories."
  (let ((path (expand-file-name file dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))))

(defun org-sync-spec--read (dir file)
  "Return the contents of FILE under DIR."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file dir))
    (buffer-string)))

(defun org-sync-spec--configure (dir)
  "Give the repository at DIR a committer identity and no signing."
  (org-sync-spec--git dir "config" "user.name" "Spec")
  (org-sync-spec--git dir "config" "user.email" "spec@example.invalid")
  (org-sync-spec--git dir "config" "commit.gpgsign" "false"))

(defun org-sync-spec--remote ()
  "Path of the bare remote."
  (expand-file-name "remote.git" org-sync-spec--root))

(defun org-sync-spec--setup ()
  "Create a bare remote and laptop A with one pushed commit; return A."
  (setq org-sync-spec--root (file-name-as-directory (make-temp-file "org-sync-spec" t)))
  (let ((a (file-name-as-directory (expand-file-name "a" org-sync-spec--root))))
    (org-sync-spec--git org-sync-spec--root "init" "--quiet" "--bare" "-b" "main"
                        (org-sync-spec--remote))
    (make-directory a)
    (org-sync-spec--git a "init" "--quiet" "-b" "main")
    (org-sync-spec--configure a)
    (org-sync-spec--write a "note.org" "* first\n")
    (org-sync-spec--git a "add" "--all")
    (org-sync-spec--git a "commit" "--quiet" "-m" "initial")
    (org-sync-spec--git a "remote" "add" "origin" (org-sync-spec--remote))
    (org-sync-spec--git a "push" "--quiet" "-u" "origin" "main")
    a))

(defun org-sync-spec--clone (name)
  "Clone the remote as laptop NAME and return its directory."
  (let ((dir (file-name-as-directory (expand-file-name name org-sync-spec--root))))
    (org-sync-spec--git org-sync-spec--root "clone" "--quiet" (org-sync-spec--remote) dir)
    (org-sync-spec--configure dir)
    dir))

(defun org-sync-spec--remote-head (&optional format)
  "Return the remote main commit, shown with git log FORMAT (default the hash)."
  (org-sync-spec--git (org-sync-spec--remote)
                      "log" "-1" (concat "--format=" (or format "%H")) "main"))

(defun org-sync-spec--remote-count ()
  "Number of commits on the remote main."
  (string-to-number (org-sync-spec--git (org-sync-spec--remote) "rev-list" "--count" "main")))

(defun org-sync-spec--run (dir)
  "Run `jf/org-sync-run' on DIR and wait for it; return (STATUS . MESSAGE)."
  (let ((jf/org-sync-directory dir)
        (result nil)
        (deadline (+ (float-time) 30)))
    (jf/org-sync-run nil (lambda (status text) (setq result (cons status text))))
    (while (and (null result) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (or result (error "org-sync run on %s did not finish" dir))))

(defmacro org-sync-spec--with-env (&rest body)
  "Run BODY with live settings, fast retries and quiet echo area."
  (declare (indent 0))
  `(let ((jf/data-profile nil)
         (jf/org-sync-retry-delay 0.1)
         (jf/org-sync-max-attempts 3)
         (jf/org-sync--running nil)
         (inhibit-message t))
     ,@body))

;;;; Specs

(describe "jf/org-sync-run"
  :var (a)

  (before-each (setq a (org-sync-spec--setup)))
  (after-each (when org-sync-spec--root
                (delete-directory org-sync-spec--root t)
                (setq org-sync-spec--root nil)))

  (it "commits a new note, pushes it and leaves the file as it was"
    (org-sync-spec--with-env
      (org-sync-spec--write a "new.org" "* new\n")
      (expect (car (org-sync-spec--run a)) :to-be 'committed)
      (expect (org-sync-spec--remote-head "%s") :to-match "^Sync notes from ")
      (expect (org-sync-spec--git (org-sync-spec--remote) "show" "main:new.org")
              :to-equal "* new")
      (expect (org-sync-spec--read a "new.org") :to-equal "* new\n")
      (expect (org-sync-spec--git a "status" "--porcelain") :to-equal "")))

  (it "records deletions too"
    (org-sync-spec--with-env
      (delete-file (expand-file-name "note.org" a))
      (expect (car (org-sync-spec--run a)) :to-be 'committed)
      (expect (org-sync-spec--git (org-sync-spec--remote) "ls-tree" "--name-only" "main")
              :to-equal "")))

  (it "makes no commit when nothing changed"
    (org-sync-spec--with-env
      (let ((before (org-sync-spec--remote-head)))
        (expect (car (org-sync-spec--run a)) :to-be 'nothing)
        (expect (org-sync-spec--remote-head) :to-equal before))))

  (it "does not re-commit a change another laptop already recorded"
    (org-sync-spec--with-env
      (let ((b (org-sync-spec--clone "b")))
        ;; Syncthing delivers the same edit to both laptops; A records it.
        (org-sync-spec--write a "note.org" "* first\nedited on A\n")
        (org-sync-spec--write b "note.org" "* first\nedited on A\n")
        (expect (car (org-sync-spec--run a)) :to-be 'committed)
        (let ((count (org-sync-spec--remote-count)))
          (expect (car (org-sync-spec--run b)) :to-be 'nothing)
          (expect (org-sync-spec--remote-count) :to-equal count)
          (expect (org-sync-spec--git b "rev-parse" "HEAD")
                  :to-equal (org-sync-spec--remote-head))
          (expect (org-sync-spec--read b "note.org") :to-equal "* first\nedited on A\n")))))

  (it "drops a stale draft commit and records only what is new"
    (org-sync-spec--with-env
      (let ((b (org-sync-spec--clone "b")))
        ;; B commits locally (an unpushed draft); meanwhile A records the
        ;; same edit and pushes.  B then gets its own new file.
        (org-sync-spec--write b "note.org" "* first\nshared edit\n")
        (org-sync-spec--git b "commit" "--quiet" "-am" "draft on B")
        (org-sync-spec--write a "note.org" "* first\nshared edit\n")
        (expect (car (org-sync-spec--run a)) :to-be 'committed)
        (org-sync-spec--write b "only-b.org" "* from B\n")
        (expect (car (org-sync-spec--run b)) :to-be 'committed)
        (expect (org-sync-spec--git (org-sync-spec--remote) "log" "--format=%s" "main")
                :not :to-match "draft on B")
        (expect (org-sync-spec--git (org-sync-spec--remote) "diff" "--name-only" "main~1" "main")
                :to-equal "only-b.org")
        (expect (org-sync-spec--read b "note.org") :to-equal "* first\nshared edit\n"))))

  (it "keeps and pushes a local commit that already contains the remote"
    (org-sync-spec--with-env
      (org-sync-spec--write a "careful.org" "* careful\n")
      (org-sync-spec--git a "add" "careful.org")
      (org-sync-spec--git a "commit" "--quiet" "-m" "A careful hand-written message")
      (expect (car (org-sync-spec--run a)) :to-be 'pushed)
      (expect (org-sync-spec--remote-head "%s") :to-equal "A careful hand-written message")))

  (it "retries when the push is rejected and succeeds on the next round"
    (org-sync-spec--with-env
      (let ((hook (expand-file-name "hooks/pre-receive" (org-sync-spec--remote)))
            (marker (expand-file-name "rejected-once" org-sync-spec--root)))
        (org-sync-spec--write (file-name-directory hook) "pre-receive"
                              (format "#!/bin/sh\n[ -e '%s' ] && exit 0\ntouch '%s'\nexit 1\n"
                                      marker marker))
        (set-file-modes hook #o755)
        (org-sync-spec--write a "new.org" "* new\n")
        (expect (car (org-sync-spec--run a)) :to-be 'committed)
        (expect (file-exists-p marker) :to-be-truthy)
        (expect (org-sync-spec--git (org-sync-spec--remote) "show" "main:new.org")
                :to-equal "* new")
        (expect (with-current-buffer "*org-sync*" (buffer-string))
                :to-match "attempt 1: push failed"))))

  (it "fails without committing or touching files when the fetch fails"
    (org-sync-spec--with-env
      (let ((head (org-sync-spec--git a "rev-parse" "HEAD")))
        (org-sync-spec--git a "remote" "set-url" "origin"
                            (expand-file-name "missing.git" org-sync-spec--root))
        (org-sync-spec--write a "new.org" "* new\n")
        (expect (car (org-sync-spec--run a)) :to-be 'failed)
        (expect (org-sync-spec--git a "rev-parse" "HEAD") :to-equal head)
        (expect (org-sync-spec--read a "new.org") :to-equal "* new\n"))))

  (it "answers busy while a run is in flight"
    (org-sync-spec--with-env
      (let ((jf/org-sync-directory a) (second nil) (first nil))
        (jf/org-sync-run nil (lambda (s _) (setq first s)))
        (jf/org-sync-run nil (lambda (s _) (setq second s)))
        (expect second :to-be 'busy)
        (let ((deadline (+ (float-time) 30)))
          (while (and (null first) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        (expect first :to-be 'nothing)))))

(describe "jf/org-sync refusals"
  :var (a)

  (before-each (setq a (org-sync-spec--setup)))
  (after-each (when org-sync-spec--root
                (delete-directory org-sync-spec--root t)
                (setq org-sync-spec--root nil)))

  (it "refuses off the live data profile"
    (org-sync-spec--with-env
      (let ((jf/data-profile '(:kind scratch :dir "/tmp/x/")))
        (org-sync-spec--write a "new.org" "* new\n")
        (expect (org-sync-spec--run a) :to-equal '(refused . "data profile is scratch, not live"))
        (expect (org-sync-spec--git a "status" "--porcelain") :to-equal "?? new.org"))))

  (it "refuses when HEAD is not on the canonical branch"
    (org-sync-spec--with-env
      (org-sync-spec--git a "switch" "--quiet" "-c" "foo7test")
      (org-sync-spec--write a "new.org" "* new\n")
      (expect (car (org-sync-spec--run a)) :to-be 'refused)
      (expect (cdr (org-sync-spec--run a)) :to-match "not on branch main")
      (expect (org-sync-spec--remote-count) :to-equal 1)))

  (it "refuses a directory that is not the top of a repository"
    (org-sync-spec--with-env
      (make-directory (expand-file-name "sub" a))
      (expect (cdr (org-sync-spec--run (file-name-as-directory (expand-file-name "sub" a))))
              :to-match "not the top of a git repository")))

  (it "refuses mid-merge"
    (org-sync-spec--with-env
      (org-sync-spec--write (expand-file-name ".git" a) "MERGE_HEAD"
                            (concat (org-sync-spec--git a "rev-parse" "HEAD") "\n"))
      (expect (cdr (org-sync-spec--run a)) :to-match "in progress"))))

(describe "jf/org-sync--production-p"
  (it "is true only for ~/.emacs.d"
    (let ((jf/emacs-dir (expand-file-name "~/.emacs.d/")))
      (expect (jf/org-sync--production-p) :to-be-truthy))
    (let ((jf/emacs-dir (expand-file-name "~/emacs-org-sync/")))
      (expect (jf/org-sync--production-p) :not :to-be-truthy))))

;;; org-sync-spec.el ends here
