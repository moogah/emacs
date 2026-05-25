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
          (tab-bar-close-tab 2)))))

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
          (tab-bar-close-tab 2)))))

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
          (tab-bar-close-tab 2)))))

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
                ;; `workspace-switch-layout' calls
                ;; `workspace--autosave-current-layout', which has an
                ;; :after advice that triggers `workspace-save-state'
                ;; (disk write).  Stub it out for the duration of the
                ;; test.
                ((symbol-function 'workspace-save-state)
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

(provide 'buffer-reincarnation-spec)
;;; buffer-reincarnation-spec.el ends here
