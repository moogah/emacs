;;; buffer-reincarnation-spec.el --- Behavioral tests for the buffer reincarnation chain -*- lexical-binding: t; -*-
;;
;; Covers the four-step fallback chain in `workspace--deserialize-buffer'
;; (bookmark → filename → name → error buffer), the `help-mode'
;; `bug#56643' workaround, the deferred-restore race guard, and a
;; non-file buffer reincarnation via a mocked bookmark handler.

(require 'buttercup)
(require 'cl-lib)
(require 'bookmark)
(require 'map)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar br-spec--parent-dir nil
  "Per-test temp directory used as `workspaces-default-parent-directory'.")

(defun br-spec--cleanup ()
  "Kill every buffer in `br-spec--parent-dir', delete it, leave *scratch* current.
`workspace-new' runs the default home builder, which `find-file's the new
home's home.org and leaves that buffer current.  Deleting the directory
under it would leave the specs after this one, in this file and the next,
running in a buffer whose `default-directory' no longer exists, where
`make-process' and `process-file' fail with `file-missing'."
  (when (and br-spec--parent-dir
             (file-directory-p br-spec--parent-dir))
    (dolist (buffer (buffer-list))
      (when (workspace--path-within-p
             (buffer-local-value 'default-directory buffer)
             br-spec--parent-dir)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))
    (delete-directory br-spec--parent-dir t))
  (set-buffer (get-buffer-create "*scratch*")))

(defun br-spec--make-temp-file (&optional contents)
  "Create a temp file with optional CONTENTS, return its path."
  (let ((path (make-temp-file "ws-br-")))
    (when contents
      (with-temp-file path (insert contents)))
    path))

(describe "workspace--deserialize-buffer fallback chain"
  (describe "step 1: bookmark restore"
    (it "returns the bookmark's buffer when bookmark-jump succeeds"
      (let* ((target (get-buffer-create " *br-spec-target*"))
             (wb (make-workspace-buffer
                  :bookmark '("fake" (handler . ignore))
                  :name " *br-spec-target*")))
        (unwind-protect
            ;; Stub bookmark-jump to switch into TARGET, simulating a
            ;; successful jump.  This drives the temp-buffer probe in
            ;; `workspace--bookmark-buffer'.
            (cl-letf (((symbol-function 'bookmark-jump)
                       (lambda (&rest _)
                         (set-buffer target))))
              (let ((result (workspace--deserialize-buffer wb)))
                (expect result :to-equal target)))
          (when (buffer-live-p target) (kill-buffer target)))))

    (it "falls through to filename when bookmark-jump errors"
      (let* ((path (br-spec--make-temp-file "hello"))
             (wb (make-workspace-buffer
                  :bookmark '("broken" (handler . ignore))
                  :filename path
                  :name (file-name-nondirectory path))))
        (unwind-protect
            (cl-letf (((symbol-function 'bookmark-jump)
                       (lambda (&rest _) (error "bookmark-jump exploded"))))
              (let ((result (workspace--deserialize-buffer wb)))
                (expect (buffer-live-p result) :to-be t)
                (expect (file-truename (buffer-file-name result))
                      :to-equal (file-truename path))))
          (delete-file path)
          (let ((buf (get-file-buffer path)))
            (when buf (kill-buffer buf)))))))

  (describe "step 2: filename fallback"
    (it "returns a buffer visiting the saved filename when no bookmark present"
      (let* ((path (br-spec--make-temp-file "second-step"))
             (wb (make-workspace-buffer
                  :filename path
                  :name (file-name-nondirectory path))))
        (unwind-protect
            (let ((result (workspace--deserialize-buffer wb)))
              (expect (buffer-live-p result) :to-be t)
              (expect (file-truename (buffer-file-name result))
                      :to-equal (file-truename path)))
          (delete-file path)
          (let ((buf (get-file-buffer path)))
            (when buf (kill-buffer buf))))))

    (it "falls through cleanly when the saved filename no longer exists"
      ;; Filename step is inlined inside `workspace--deserialize-buffer'
      ;; (guarded by `ignore-errors').  When the saved path is gone,
      ;; the `or' chain advances; with no name/bookmark/live-buffer
      ;; either, we land on the error-buffer terminator.
      (let* ((tmp (make-temp-file "br-spec-step2-"))
             (wb (make-workspace-buffer :filename tmp
                                        :name "br-spec-step2")))
        (delete-file tmp)
        (let ((result (workspace--deserialize-buffer wb)))
          (unwind-protect
              (progn
                (expect (buffer-live-p result) :to-be t)
                (expect (buffer-name result)
                        :to-match (regexp-quote "br-spec-step2")))
            (when (buffer-live-p result) (kill-buffer result)))))))

  (describe "step 3: name fallback"
    (it "returns the live buffer of that name when no bookmark or filename"
      (let* ((live (get-buffer-create " *br-spec-step3*"))
             (wb (make-workspace-buffer :name " *br-spec-step3*")))
        (unwind-protect
            (let ((result (workspace--deserialize-buffer wb)))
              (expect result :to-equal live))
          (when (buffer-live-p live) (kill-buffer live))))))

  (describe "step 4: error buffer"
    (it "returns a live, named error buffer when all earlier steps fail"
      (let ((wb (make-workspace-buffer
                 :name "vanished-buffer-name")))
        (let ((result (workspace--deserialize-buffer wb)))
          (expect (buffer-live-p result) :to-be t)
          (expect (buffer-name result)
                  :to-equal "*workspace-restore: vanished-buffer-name*")
          (kill-buffer result))))

    (it "renders the explanation text into the error buffer"
      (let* ((wb (make-workspace-buffer :name "explain-test"))
             (result (workspace--deserialize-buffer wb)))
        (unwind-protect
            (with-current-buffer result
              (expect (buffer-string) :to-match
                      "Workspaces could not reincarnate"))
          (when (buffer-live-p result) (kill-buffer result)))))))

(describe "bug#56643 read-back failure shape"
  (it "falls through to filename when the bookmark handler raises a read error"
    (let* ((path (br-spec--make-temp-file "after-56643"))
           (wb (make-workspace-buffer
                :bookmark '("help-mode-bookmark" (handler . help-mode))
                :filename path
                :name "*Help*")))
      (unwind-protect
          (cl-letf (((symbol-function 'bookmark-jump)
                     (lambda (&rest _)
                       ;; Mimic bug#56643: read fails on the natively
                       ;; compiled subr inside the bookmark props.
                       (signal 'invalid-read-syntax (list "#<subr>")))))
            (let ((result (workspace--deserialize-buffer wb)))
              (expect (buffer-live-p result) :to-be t)
              (expect (file-truename (buffer-file-name result))
                      :to-equal (file-truename path))))
        (delete-file path)
        (let ((buf (get-file-buffer path)))
          (when buf (kill-buffer buf)))))))

(describe "deferred restore via run-at-time"
  (before-each
    (clrhash workspace--registry)
    (let ((tabs (frame-parameter nil 'tabs)))
      (when (> (length tabs) 1)
        (dotimes (_ (1- (length tabs)))
          (tab-bar-close-tab 2))))
    ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
    ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
    (spy-on 'workspace-scaffold :and-call-fake
            (lambda (home _name &rest _) (make-directory home t) home))
    (setq br-spec--parent-dir
          (make-temp-file "ws-br-spec-" t)
          workspaces-default-parent-directory br-spec--parent-dir))
  (after-each (br-spec--cleanup))

  (it "schedules window-state-put via run-at-time rather than calling it immediately"
    (let* ((scheduled nil)
           (put-called nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest _args)
                   (push fn scheduled)))
                ((symbol-function 'window-state-put)
                 (lambda (&rest _) (setq put-called t))))
        ;; Drive a restore.
        (workspace-new "alpha")
        (let* ((ws (gethash "alpha" workspace--registry))
               (group (workspace--find-group ws "home"))
               (layout (workspace--group-recent-layout group))
               (state (workspace--layout-effective-state layout)))
          (workspace--restore-frameset state))
        ;; window-state-put MUST NOT have been called yet — the
        ;; closure is sitting on the timer list.
        (expect put-called :to-be nil)
        (expect (length scheduled) :to-be-weakly-greater-than 1)
        ;; Firing the closure runs window-state-put.
        (funcall (car scheduled))
        (expect put-called :to-be t)))))

(describe "restore generation guard"
  (before-each
    (clrhash workspace--registry)
    (setq workspace--restore-generation 0)
    (let ((tabs (frame-parameter nil 'tabs)))
      (when (> (length tabs) 1)
        (dotimes (_ (1- (length tabs)))
          (tab-bar-close-tab 2))))
    ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
    ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
    (spy-on 'workspace-scaffold :and-call-fake
            (lambda (home _name &rest _) (make-directory home t) home))
    (setq br-spec--parent-dir
          (make-temp-file "ws-br-spec-" t)
          workspaces-default-parent-directory br-spec--parent-dir))
  (after-each (br-spec--cleanup))

  (it "stale deferred restore no-ops when a newer one has been queued"
    ;; Two restores in rapid succession.  Capture both closures; fire
    ;; the older one first and assert it noops (window-state-put not
    ;; called); then fire the newer and assert it runs.
    (let ((closures nil)
          (put-count 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest _args)
                   (push fn closures)))
                ((symbol-function 'window-state-put)
                 (lambda (&rest _) (cl-incf put-count))))
        (workspace-new "alpha")
        (let* ((ws (gethash "alpha" workspace--registry))
               (group (workspace--find-group ws "home"))
               (layout (workspace--group-recent-layout group))
               (state (workspace--layout-effective-state layout)))
          (workspace--apply-saved-layout "alpha")
          (workspace--apply-saved-layout "alpha"))
        ;; Two closures queued in LIFO order: (newer older)
        (expect (length closures) :to-be-weakly-greater-than 2)
        ;; Fire the older one (last pushed → last in `closures`'s
        ;; reverse order; we pushed both, so `nth (- N 1)` is the
        ;; oldest).  With LIFO, the LAST element is the oldest.
        (let* ((ordered (nreverse closures))   ; oldest first
               (older (car ordered))
               (newer (car (last ordered))))
          (funcall older)
          (expect put-count :to-equal 0)
          (funcall newer)
          (expect put-count :to-be-weakly-greater-than 0))))))

(defun br-spec--declining-handler (_record)
  "A bookmark handler that DECLINES, as the portable-bookmarks seam does."
  (user-error "Bookmark \"declines\": not on this branch; nothing opened"))

(defvar br-spec--leaf-flag-seen nil
  "What `br-spec--flag-reading-handler' saw, newest first.")

(defun br-spec--flag-reading-handler (_record)
  "A bookmark handler that records whether a layout leaf is being restored.
Reads `workspace--restoring-layout-leaf' as a consumer must: guarded by
`boundp'.  Then lands in the buffer \" *br-spec-flag*\"."
  (push (and (boundp 'workspace--restoring-layout-leaf)
             (symbol-value 'workspace--restoring-layout-leaf))
        br-spec--leaf-flag-seen)
  (set-buffer (get-buffer-create " *br-spec-flag*")))

(describe "the gate: a bookmark handler that declines"
  ;; A `user-error' from the handler is a DECISION for the user to see,
  ;; not a failure to fall through: steps 2 (filename) and 3 (name) are
  ;; skipped and the leaf lands on the error buffer carrying the reason.
  ;; Register: no-silent-wrong-file-open prohibitions 1 and 4 — the
  ;; stored filename is the path the buffer had when captured, which the
  ;; handler has just refused to open.
  (it "skips the filename AND the same-name live buffer, and lands on the error buffer with the reason"
    (let* ((path (br-spec--make-temp-file "wrong copy"))
           (same-name (get-buffer-create "declines.el"))
           (wb (make-workspace-buffer
                :bookmark '("declines" (handler . br-spec--declining-handler))
                :filename path
                :name "declines.el"))
           (collector (list nil))
           (result (let ((workspace--reincarnation-declines collector))
                     (workspace--deserialize-buffer wb))))
      (unwind-protect
          (progn
            (expect (buffer-live-p result) :to-be t)
            (expect (buffer-name result) :to-equal "*workspace-restore: declines.el*")
            (expect result :not :to-be same-name)
            (expect (find-buffer-visiting path) :to-be nil)
            (with-current-buffer result
              (expect (buffer-string) :to-match "declined to restore")
              (expect (buffer-string) :to-match "not on this branch")
              (expect (buffer-string) :to-match "NOT opened"))
            ;; Noted once for the restore's single report.
            (expect (length (car collector)) :to-equal 1)
            (expect (car (car (car collector))) :to-equal "declines.el")
            (expect (cdr (car (car collector))) :to-match "not on this branch"))
        (delete-file path)
        (when (buffer-live-p same-name) (kill-buffer same-name))
        (when (buffer-live-p result) (kill-buffer result)))))

  (it "asks a stock handler's own question exactly once, as on any other visit, and the leaf restores (the large-file prompt via read-multiple-choice)"
    ;; Core binds nothing that suppresses interaction around a leaf.  A
    ;; plain file record over `large-file-warning-threshold' makes the
    ;; stock `find-file-noselect' ask through the REAL
    ;; `read-multiple-choice'.  Only `read-event' — the Emacs primitive
    ;; under it — is stubbed: it answers ?y, honours `inhibit-interaction'
    ;; as the C primitive does (signals `inhibited-interaction', which
    ;; rmc swallows and re-prompts on), and is bounded, so a restore that
    ;; spins ends the spec instead of hanging it.
    (let* ((path (br-spec--make-temp-file (make-string 200 ?x)))
           (wb (make-workspace-buffer
                :bookmark `("big" (filename . ,path) (position . 1))
                :filename path
                :name (file-name-nondirectory path)))
           (asked 0)
           (collector (list nil))
           (result
            (catch 'br-spec--spun
              (cl-letf (((symbol-function 'read-event)
                         (lambda (&rest _)
                           (setq asked (1+ asked))
                           (when (> asked 5) (throw 'br-spec--spun 'spun))
                           (when inhibit-interaction
                             (signal 'inhibited-interaction nil))
                           ?y)))
                (let ((large-file-warning-threshold 10)
                      (inhibit-message t)
                      (workspace--reincarnation-declines collector))
                  (workspace--deserialize-buffer wb))))))
      (unwind-protect
          (progn
            (expect asked :to-equal 1)
            (expect (bufferp result) :to-be t)
            (expect (file-truename (buffer-file-name result))
                    :to-equal (file-truename path))
            (expect (car collector) :to-be nil))
        (delete-file path)
        (when (bufferp result) (kill-buffer result)))))

  (it "binds workspace--restoring-layout-leaf around the leaf's jump, and only there"
    ;; Core's one neutral signal to consumers; a consumer reads it
    ;; boundp-guarded and declines rather than asking per leaf.
    (setq br-spec--leaf-flag-seen nil)
    (let* ((wb (make-workspace-buffer
                :bookmark '("flag" (handler . br-spec--flag-reading-handler))
                :name " *br-spec-flag*"))
           (result (workspace--deserialize-buffer wb)))
      (unwind-protect
          (progn
            (expect br-spec--leaf-flag-seen :to-equal '(t))
            (expect (buffer-name result) :to-equal " *br-spec-flag*")
            (expect (boundp 'workspace--restoring-layout-leaf) :to-be t)
            (expect (symbol-value 'workspace--restoring-layout-leaf) :to-be nil))
        (when (buffer-live-p result) (kill-buffer result)))))

  (it "signals, never opening the saved filename, when step 1 answers anything but a buffer, nil or a decline"
    ;; register/invariant/no-silent-wrong-file-open: a catch-all arm
    ;; errors or does nothing; it never opens.
    (let* ((path (br-spec--make-temp-file "not this one"))
           (wb (make-workspace-buffer
                :bookmark '("odd" (handler . ignore))
                :filename path
                :name (file-name-nondirectory path))))
      (unwind-protect
          (cl-letf (((symbol-function 'workspace--bookmark-buffer)
                     (lambda (_ws-buffer) '(surprise . "unexpected"))))
            (expect (workspace--deserialize-buffer wb) :to-throw 'error)
            (expect (find-buffer-visiting path) :to-be nil))
        (delete-file path))))

  (it "still falls through to the filename when the handler merely FAILS"
    ;; The pre-existing contract, kept: a plain error is not a decision.
    (let* ((path (br-spec--make-temp-file "still here"))
           (wb (make-workspace-buffer
                :bookmark '("fails" (handler . ignore))
                :filename path
                :name (file-name-nondirectory path)))
           (collector (list nil))
           (result (cl-letf (((symbol-function 'bookmark-jump)
                              (lambda (&rest _) (error "handler exploded"))))
                     (let ((workspace--reincarnation-declines collector))
                       (workspace--deserialize-buffer wb)))))
      (unwind-protect
          (progn
            (expect (file-truename (buffer-file-name result))
                    :to-equal (file-truename path))
            (expect (car collector) :to-be nil))
        (delete-file path)
        (when (buffer-live-p result) (kill-buffer result)))))

  (it "reports the collected declines in ONE message, or none when there were none"
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (workspace--report-reincarnation-declines nil)
        (expect messages :to-be nil)
        (workspace--report-reincarnation-declines
         '(("second.el" . "reason two") ("first.el" . "reason one")))
        (expect (length messages) :to-equal 1)
        (expect (car messages) :to-match "2 buffers not restored")
        (expect (car messages) :to-match "first.el (reason one); second.el (reason two)")))))

(describe "reincarnation step closed-set"
  ;; Mirrors the `workspace--valid-state-slots' / `workspace--state-slot-p'
  ;; pair: pins the four step symbols in chain order and asserts the
  ;; predicate accepts every shipped step while rejecting unrelated
  ;; symbols and nil.  Closes the closed-set discipline asymmetry
  ;; noted by cycle-1 architect findings.
  (it "names exactly four steps in chain order"
    (expect workspace--valid-reincarnation-steps
            :to-equal '(bookmark filename name error-buffer)))
  (it "predicate accepts every shipped step"
    (dolist (s workspace--valid-reincarnation-steps)
      (expect (workspace--reincarnation-step-p s) :to-be-truthy)))
  (it "predicate rejects unrelated symbols"
    (expect (workspace--reincarnation-step-p 'mystery) :to-be nil)
    (expect (workspace--reincarnation-step-p nil) :to-be nil)))

(describe "restore generation guard via workspace-switch-layout"
  ;; Sibling to the `restore generation guard' suite above: pins the
  ;; same invariant from the SECOND entry path into
  ;; `workspace--restore-frameset'.  The race-guard cl-incf lives at
  ;; the single restore choke point so every entry path participates
  ;; (register/boundary/buffer-reincarnation-pipeline stage 4).
  (before-each
    (clrhash workspace--registry)
    (setq workspace--restore-generation 0)
    (let ((tabs (frame-parameter nil 'tabs)))
      (when (> (length tabs) 1)
        (dotimes (_ (1- (length tabs)))
          (tab-bar-close-tab 2))))
    ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
    ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
    (spy-on 'workspace-scaffold :and-call-fake
            (lambda (home _name &rest _) (make-directory home t) home))
    (setq br-spec--parent-dir
          (make-temp-file "ws-br-spec-" t)
          workspaces-default-parent-directory br-spec--parent-dir))
  (after-each (br-spec--cleanup))

  (it "stale deferred restore no-ops when triggered by a later workspace-switch-layout"
    ;; Two `workspace-switch-layout' calls back-to-back across two
    ;; layouts (home → beta → home).  Capture both deferred closures;
    ;; fire the older first and assert it noops; then fire the newer
    ;; and assert it runs `window-state-put'.
    (let ((closures nil)
          (put-count 0))
      ;; Seed a second layout `beta' on the alpha workspace before
      ;; installing the run-at-time / window-state-put mocks, so that
      ;; `workspace-save-layout' can do its real frameset capture
      ;; without our counters firing.
      (workspace-new "alpha")
      (workspace-save-layout "beta")
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest _args)
                   (push fn closures)))
                ((symbol-function 'window-state-put)
                 (lambda (&rest _) (cl-incf put-count)))
                ;; `workspace-switch-layout' flushes synchronously via
                ;; `workspace--flush-state' (disk write).  Stub it out for
                ;; the duration of the test so the frameset-capture path
                ;; runs without touching disk.
                ((symbol-function 'workspace--flush-state)
                 (lambda (&rest _) nil)))
        ;; Two switches in rapid succession.  Each one calls
        ;; `workspace--restore-frameset' once, which `cl-incf's the
        ;; generation and pushes a closure onto our `closures' list.
        (workspace-switch-layout "home")
        (workspace-switch-layout "beta")
        ;; At least two closures queued; both came through
        ;; `workspace-switch-layout', not the
        ;; `workspace--apply-saved-layout' entry path.
        (expect (length closures) :to-be-weakly-greater-than 2)
        (let* ((ordered (nreverse closures))   ; oldest first
               (older  (car ordered))
               (newer  (car (last ordered))))
          (funcall older)
          (expect put-count :to-equal 0)
          (funcall newer)
          (expect put-count :to-be-weakly-greater-than 0))))))

(describe "non-file buffer reincarnation (magit-status shape)"
  (it "uses the bookmark handler to materialize a magit-status buffer"
    ;; Mock at the bookmark handler boundary so we do not require
    ;; magit at test time; the contract is: bookmark step returns a
    ;; live buffer named like `magit: ...' via the registered handler.
    (let* ((magit-buf (get-buffer-create "magit: ~/repo/"))
           (wb (make-workspace-buffer
                :bookmark '("magit: ~/repo/" (handler . magit-bookmark-jump))
                :name "magit: ~/repo/")))
      (unwind-protect
          (cl-letf (((symbol-function 'bookmark-jump)
                     (lambda (&rest _)
                       (set-buffer magit-buf))))
            (let ((result (workspace--deserialize-buffer wb)))
              (expect (buffer-live-p result) :to-be t)
              (expect (buffer-name result) :to-equal "magit: ~/repo/")))
        (when (buffer-live-p magit-buf) (kill-buffer magit-buf))))))

(describe "buffer-reincarnation-spec's own cleanup"
  ;; Runs after every describe above that calls `workspace-new'.  Their
  ;; cleanup must not leave the current buffer, or any live buffer, in a
  ;; directory it deleted: a later spec that spawns a subprocess would
  ;; fail with `file-missing' in the full run only.
  (it "leaves the current buffer in a directory that exists"
    (expect (file-directory-p default-directory) :to-be-truthy)
    (expect (process-file "true") :to-equal 0))

  (it "leaves no live buffer in a deleted per-test directory"
    (expect (seq-filter
             (lambda (buffer)
               (let ((dir (buffer-local-value 'default-directory buffer)))
                 (and (stringp dir)
                      (string-match-p "ws-br-spec-" dir)
                      (not (file-directory-p dir)))))
             (buffer-list))
            :to-equal nil)))

(provide 'buffer-reincarnation-spec)
;;; buffer-reincarnation-spec.el ends here
