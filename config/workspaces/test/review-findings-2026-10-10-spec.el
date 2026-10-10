;;; review-findings-2026-10-10-spec.el --- Validation specs for the post-archive review findings -*- lexical-binding: t; -*-

;; One spec per finding of the 2026-10-10 post-archive review of the
;; portable-bookmarks branch (the nine `.tasks/' files dated that day).
;; Each spec states the behaviour the task asks for, so on HEAD it is RED:
;; a failing spec here is the finding reproduced, a passing one is the
;; finding refuted.  Once validated, the bug specs are the pending tests
;; of their tasks (`xit', the overlay's pending style) until the fix
;; lands, when the implementor moves each into the spec file its task
;; names and promotes it.
;;
;; Loads the same fixtures as the portable-bookmarks suite, located by
;; walking up from this file, so the file can live in a scratch
;; subdirectory (run alone with `-d') or beside the other specs.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'bookmark)
(require 'tab-bar)
(require 'map)

(defconst review-findings-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(defconst review-findings-spec--test-dir
  (locate-dominating-file review-findings-spec--this-dir
                          "portable-bookmarks-fixtures.el")
  "The workspaces test directory, wherever this file sits under it.")

;; Under the harness init.el has loaded the workspaces package.  Load the
;; core modules by relative path only when it has not (the purge spec's
;; preamble, so `workspace-purge' is here too).
(unless (fboundp 'workspace-purge)
  (let ((dir review-findings-spec--test-dir))
    (load (expand-file-name "../data-model.el" dir))
    (load (expand-file-name "../integrations.el" dir))
    (load (expand-file-name "../home-org.el" dir))
    (load (expand-file-name "../scaffold.el" dir))
    (load (expand-file-name "../tabs.el" dir))
    (unless (featurep 'bufferlo)
      (defalias 'bufferlo-mode (lambda (&optional _) nil))
      (provide 'bufferlo))
    (load (expand-file-name "../buffer-membership.el" dir))
    (load (expand-file-name "../layouts.el" dir))
    (load (expand-file-name "../persistence.el" dir))
    (let ((workspaces-el (expand-file-name "../workspaces.el" dir)))
      (unless (fboundp 'jf/load-module)
        (defalias 'jf/load-module
          (lambda (path) (when (file-exists-p path) (load path nil t)))))
      (unless (boundp 'jf/emacs-dir)
        (defvar jf/emacs-dir (expand-file-name "../../.." dir)))
      (load workspaces-el nil t))))

(load (expand-file-name "portable-bookmarks-fixtures.el" review-findings-spec--test-dir))
(load (expand-file-name "../portable-bookmarks.el" review-findings-spec--test-dir))

(defvar jf/emacs-dir)
(defvar workspace-home-builder)
(defvar workspaces-default-parent-directory)
(defvar workspace--restoring-layout-leaf)


;;; Helpers

(defun review-findings-spec--reset-frame ()
  "Leave the frame on its first tab with one window and no pending restore."
  (tab-bar-select-tab 1)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (delete-other-windows)
  (workspace-bookmark-fixtures-reset-jump-state)
  (setq workspace-bookmark-fixtures-workspace-prompts nil))

(defun review-findings-spec--drain-timers ()
  "Run the event loop briefly so the deferred paint and the finish step fire."
  (sit-for 0)
  (sit-for 0)
  (accept-process-output nil 0.05))

(defun review-findings-spec--give-saved-layout (name state)
  "Store window-state STATE as workspace NAME's saved \"home\" layout."
  (let* ((ws (gethash name workspace--registry))
         (ws (workspace--upsert-group ws "home" (workspace--layout-make state)))
         (ws (workspace--set-recent-group ws "home")))
    (puthash name ws workspace--registry)))

(defun review-findings-spec--two-window-state ()
  "Capture a two-window window-state of the current tab, then undo the split."
  (delete-other-windows)
  (split-window-right)
  (prog1 (workspace--capture-frameset)
    (delete-other-windows)))

(defun review-findings-spec--layout-beside-scratch (path)
  "Capture PATH in the selected window beside *scratch*; kill PATH's buffer after."
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (split-window-right)
  (find-file path)
  (prog1 (workspace--capture-frameset)
    (kill-buffer (find-buffer-visiting path))
    (delete-other-windows)))

(defun review-findings-spec--windows-visiting (path)
  "Return the windows of the selected frame whose buffer visits PATH."
  (seq-filter (lambda (window)
                (let ((file (buffer-file-name (window-buffer window))))
                  (and file (workspace--same-file-p file path))))
              (window-list nil 'no-mini)))

(defun review-findings-spec--window-showing (name)
  "Return the window showing the buffer named NAME, or nil."
  (seq-find (lambda (window)
              (equal (buffer-name (window-buffer window)) name))
            (window-list nil 'no-mini)))

(defun review-findings-spec--git-tally ()
  "Tally the spied `workspace-bookmark--git' calls by git subcommand.
Returns an alist (SUBCOMMAND . COUNT), most frequent first."
  (let ((tally nil))
    (dolist (call (spy-calls-all-args 'workspace-bookmark--git))
      (let* ((args (cdr call))
             (sub (seq-find (lambda (arg)
                              (and (stringp arg)
                                   (not (string-prefix-p "-" arg))))
                            args)))
        (let ((cell (assoc sub tally)))
          (if cell (cl-incf (cdr cell)) (push (cons sub 1) tally)))))
    (seq-sort-by #'cdr #'> tally)))

(defmacro review-findings-spec--tallying (&rest body)
  "Run BODY with `workspace-bookmark--git' spied; return (RESULT . TALLY)."
  (declare (indent 0))
  `(progn
     (spy-on 'workspace-bookmark--git :and-call-through)
     (let ((result (progn ,@body)))
       (cons result (review-findings-spec--git-tally)))))

(defun review-findings-spec--foreign-user-error-handler (_record)
  "A stock-like bookmark handler that signals a `user-error' of ITS OWN.
Modelled on `Info-find-file' (info.el, Emacs 30.2: \"Info file `%s' does
not exist; consider installing it\"), which is not a portable-bookmarks
decline but a `user-error' all the same."
  (user-error "Info file `%s' does not exist; consider installing it" "missing"))


;;; finish-restore-overrides-caller-display

(describe "finding: finish-restore overrides the caller's display"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (review-findings-spec--reset-frame)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-fin" "git@example.com:fixtures/fin.git")))

  (xit "bookmark-jump-other-window into a workspace with no live tab leaves the painted layout's selected window alone and shows the file in another"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (painted-selected nil))
        (review-findings-spec--give-saved-layout
         "wsbm-fin-a" (review-findings-spec--two-window-state))
        (workspace-bookmark-fixtures-set-bookmark b-path "readme")
        (expect (workspace--tab-index-for "wsbm-fin-a") :to-be nil)
        (cl-letf* ((put (symbol-function 'window-state-put))
                   ((symbol-function 'window-state-put)
                    (lambda (&rest args)
                      (prog1 (apply put args)
                        (setq painted-selected (selected-window))))))
          (workspace-bookmark-fixtures-jump "readme" :pick "wsbm-fin-a"
                                            :jumper #'bookmark-jump-other-window)
          (review-findings-spec--drain-timers))
        (expect (workspace--current-name) :to-equal "wsbm-fin-a")
        (expect (window-live-p painted-selected) :to-be t)
        ;; The file is shown once, in a window OTHER than the one the
        ;; paint selected: that is what the caller asked for.
        (expect (length (review-findings-spec--windows-visiting a-path)) :to-equal 1)
        (expect (memq painted-selected (review-findings-spec--windows-visiting a-path))
                :to-be nil)))))


;;; purge-scope-recheck-after-registry-removal

(describe "finding: workspace-purge re-checks scope after the registry entry is gone"
  :var (parent)

  (workspace-bookmark-fixtures-with-empty-registry
    (setq parent (workspace-bookmark-fixtures-temp-dir "wsbm-purge-parent-")))

  (xit "leaves the workspace registered, its tab open and its home on disk when the home left scope during the prompt"
    (let* ((home (expand-file-name "moving" parent))
           (workspaces-default-parent-directory parent)
           (in-scope t)
           (deleted nil)
           (tab-closed nil))
      (make-directory home t)
      (workspace-bookmark-fixtures-register-workspace "moving" home)
      (spy-on 'workspace--flush-state :and-return-value nil)
      (spy-on 'workspace--dispatch-purge-integrations)
      (cl-letf (((symbol-function 'workspace--purge-in-scope-p)
                 (lambda (&rest _) in-scope))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq in-scope nil) t))
                ((symbol-function 'workspace--tab-index-for)
                 (lambda (&rest _) 2))
                ((symbol-function 'tab-bar-close-tab)
                 (lambda (&rest _) (setq tab-closed t)))
                ((symbol-function 'delete-directory)
                 (lambda (dir &rest _) (push dir deleted))))
        (let ((current-prefix-arg nil))
          (expect (workspace-purge "moving") :to-throw 'user-error)))
      (expect deleted :to-be nil)
      (expect 'workspace--dispatch-purge-integrations :not :to-have-been-called)
      (expect (file-directory-p home) :to-be t)
      ;; Nothing irreversible happened before the decision: the user asked
      ;; for a purge and got neither a purge nor a delete.
      (expect (gethash "moving" workspace--registry) :not :to-be nil)
      (expect tab-closed :to-be nil)
      (expect 'workspace--flush-state :not :to-have-been-called))))


;;; reincarnation-decline-gate-too-broad

(describe "finding: the reincarnation decline gate catches every user-error"
  (xit "restores a leaf through its stored filename when the handler signals a user-error of its own (an Info file missing), not a portable-bookmarks decline"
    (let* ((path (make-temp-file "wsbm-foreign-" nil nil "still here\n"))
           (wb (make-workspace-buffer
                :bookmark '("foreign" (handler . review-findings-spec--foreign-user-error-handler))
                :filename path
                :name (file-name-nondirectory path)))
           (collector (list nil))
           (result (let ((workspace--reincarnation-declines collector))
                     (workspace--deserialize-buffer wb))))
      (unwind-protect
          (progn
            (expect (buffer-file-name result) :not :to-be nil)
            (expect (file-truename (buffer-file-name result))
                    :to-equal (file-truename path))
            (expect (car collector) :to-be nil))
        (delete-file path)
        (when (buffer-live-p result) (kill-buffer result)))))

  (xit "restores a leaf through the live buffer of its name when the handler signals a user-error of its own and there is no filename (an *info* leaf)"
    (let* ((same-name (get-buffer-create "*info*"))
           (wb (make-workspace-buffer
                :bookmark '("foreign" (handler . review-findings-spec--foreign-user-error-handler)
                            (info-node . "Top"))
                :filename nil
                :name "*info*"))
           (collector (list nil))
           (result (let ((workspace--reincarnation-declines collector))
                     (workspace--deserialize-buffer wb))))
      (unwind-protect
          (progn
            (expect result :to-be same-name)
            (expect (car collector) :to-be nil))
        (when (buffer-live-p same-name) (kill-buffer same-name))
        (when (and (buffer-live-p result) (not (eq result same-name)))
          (kill-buffer result))))))


;;; restore-stamped-leaf-in-external-canonical-checkout

(describe "finding: a stamped record whose file IS the canonical checkout's copy"
  :var (repo home-b)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (review-findings-spec--reset-frame)
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote "git@example.com:fixtures/canon.git"
                :contents workspace-bookmark-fixtures-jump-contents)
          home-b (workspace-bookmark-fixtures-make-workspace "wsbm-canon-b")))

  (xit "resolves to that same file in a workspace holding no checkout, instead of missing-repo"
    (ignore home-b)
    (let* ((canonical-path (expand-file-name "README.md" repo))
           (record (workspace-bookmark--stamp-record
                    (list "canon" (cons 'filename canonical-path))))
           (result (let ((jf/emacs-dir repo))
                     (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
                                (lambda () "wsbm-canon-b")))
                       (workspace-bookmark--resolve record "wsbm-canon-b")))))
      (expect (bookmark-prop-get record workspace-bookmark-relpath-property)
              :to-equal "README.md")
      (expect (plist-get result :outcome) :to-be 'resolved)
      (expect (workspace--same-file-p (plist-get result :path) canonical-path)
              :to-be-truthy)))

  (xit "restores as a layout leaf in a workspace holding no checkout, rather than landing on the error buffer"
    (workspace-bookmark-fixtures-hermetic
      (let* ((canonical-path (expand-file-name "README.md" repo))
             (reports nil)
             (asked nil))
        (review-findings-spec--give-saved-layout
         "wsbm-canon-b" (review-findings-spec--layout-beside-scratch canonical-path))
        (cl-letf* ((guard (symbol-function 'workspace-bookmark--can-prompt-p))
                   ((symbol-function 'workspace-bookmark--can-prompt-p)
                    (lambda () (let ((noninteractive nil)) (funcall guard))))
                   ((symbol-function 'read-multiple-choice)
                    (lambda (&rest args) (setq asked args) (error "Prompted: %S" args)))
                   ((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (when format-string
                        (push (apply #'format-message format-string args) reports))
                      nil)))
          (let ((jf/emacs-dir repo))
            (workspace-restore "wsbm-canon-b")
            (review-findings-spec--drain-timers)))
        (expect asked :to-be nil)
        (expect (workspace--current-name) :to-equal "wsbm-canon-b")
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be-truthy)
        (expect (review-findings-spec--window-showing "*workspace-restore: README.md*")
                :to-be nil)
        (expect (seq-filter (lambda (report) (string-match-p "not restored" report))
                            reports)
                :to-equal nil)))))


;;; substituted-record-reaches-bookmark-error-no-filename

(describe "finding: a resolved path that exists but is unreadable"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-unr" "git@example.com:fixtures/unr.git")))

  (xit "declines naming the path, rather than returning silently or signalling a type error"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (modes (file-modes a-path))
             (origin (generate-new-buffer "wsbm-unr-origin"))
             (outcome nil))
        (workspace-bookmark-fixtures-set-bookmark b-path "readme")
        (unwind-protect
            (progn
              (set-file-modes a-path #o000)
              (expect (file-readable-p a-path) :to-be nil)
              (with-current-buffer origin
                (setq outcome
                      (condition-case err
                          (progn
                            (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-unr-a")
                            (list 'returned
                                  :current (buffer-name (current-buffer))
                                  :visits-a (workspace-bookmark-fixtures-visits-p a-path)))
                        (user-error (list 'declined (cadr err)))
                        (error (list 'error (error-message-string err)))))))
          (set-file-modes a-path modes)
          (kill-buffer origin))
        (message "OBSERVED unreadable-path jump: %S" outcome)
        (expect (car outcome) :to-be 'declined)
        (expect (cadr outcome) :to-match (regexp-quote a-path))))))


;;; stamp-advice-cost-on-layout-capture (measurement)

(describe "measurement: the stamp advice during a layout capture"
  :var (repo files)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (review-findings-spec--reset-frame)
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote "git@example.com:fixtures/capture.git")
          files (mapcar (lambda (relpath)
                          (workspace-bookmark-fixtures-commit-file repo relpath))
                        '("one.el" "two.el" "three.el"))))

  (xit "captures a three-leaf layout in one checkout with at most one rev-parse (the task's budget)"
    (workspace-bookmark-fixtures-hermetic
      (delete-other-windows)
      (find-file (nth 0 files))
      (select-window (split-window-right))
      (find-file (nth 1 files))
      (select-window (split-window-below))
      (find-file (nth 2 files))
      ;; Warm the identity memo: the first capture pays the remote lookup.
      (workspace--capture-frameset)
      (let* ((measured (review-findings-spec--tallying (workspace--capture-frameset)))
             (tally (cdr measured))
             (state (car measured))
             (stamped 0))
        (workspace--window-state-walk-leaves
         state
         (lambda (leaf)
           (let* ((ws-buffer (map-elt (map-elt (cdr leaf) 'parameters) 'workspace-buffer))
                  (record (and ws-buffer (workspace-buffer-bookmark ws-buffer))))
             (when (and record (bookmark-prop-get record workspace-bookmark-relpath-property))
               (cl-incf stamped)))
           leaf))
        (message "OBSERVED capture of 3 leaves, warm identity memo: %d stamped, git calls %S"
                 stamped tally)
        (expect stamped :to-equal 3)
        (expect (or (cdr (assoc "rev-parse" tally)) 0) :to-be-less-than 2)))))


;;; off-workspace-picker-repeated-discovery (measurement)

(describe "measurement: discovery across the off-workspace picker and the resolve after it"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (review-findings-spec--reset-frame)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-pick" "git@example.com:fixtures/pick.git"
                 :holding '("a" "b" "c"))))

  (xit "positions each distinct checkout once across the picker and the resolve (the task's budget)"
    (workspace-bookmark-fixtures-hermetic
      (let* ((b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (identity "example.com/fixtures/pick"))
        (workspace-bookmark-fixtures-set-bookmark b-path "readme")
        ;; Warm the identity memo for every checkout the picker will see.
        (workspace-bookmark--offerable-workspaces identity)
        (let* ((measured (review-findings-spec--tallying
                           (workspace-bookmark-fixtures-jump "readme" :pick "wsbm-pick-a")))
               (tally (cdr measured)))
          (review-findings-spec--drain-timers)
          (message "OBSERVED off-workspace jump, 3 holders x 1 checkout, warm identity memo: git calls %S"
                   tally)
          (expect (workspace--current-name) :to-equal "wsbm-pick-a")
          ;; Three distinct checkouts in the registry: three positions, not
          ;; three for the picker plus one more for the chosen workspace.
          (expect (or (cdr (assoc "rev-parse" tally)) 0) :to-be-less-than 4))))))

;;; review-findings-2026-10-10-spec.el ends here
