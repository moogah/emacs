;;; gptel-integration-spec.el --- Tests for workspace-sessions-dir (producer) -*- lexical-binding: t; -*-

;; Producer-side contract for register/boundary/
;; gptel-sessions-workspace-consult.  Asserts:
;;
;;   - nil when no workspace owns the current tab
;;   - nil when the current workspace is broken
;;   - nil when the workspace exists but its sessions/ dir is missing
;;   - <:home>/sessions/ when the workspace is healthy and sessions/ exists
;;   - no signal and no side effects under any input
;;
;; The directionality lint — "no .el under config/workspaces/ references
;; gptel-sessions-*" — also lives here because it asserts a structural
;; property of the workspaces tree itself.

(require 'buttercup)
(require 'cl-lib)

(defconst gptel-integration-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this spec file; captured at load time.")

(let ((dir gptel-integration-spec--this-dir))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir))
  ;; workspaces.el provides workspace-sessions-dir.  Loading it pulls
  ;; in the submodule chain via jf/load-module; for tests we just need
  ;; the function definition, so we load the file directly and tolerate
  ;; the submodule loader having already been satisfied above.
  (load (expand-file-name "../workspaces.el" dir)))

(defun gptel-integration-spec--reset ()
  (clrhash workspace--registry))

(defun gptel-integration-spec--with-tmp-home (body-fn)
  "Call BODY-FN with HOME bound to a fresh tmp directory; clean up after."
  (let ((home (make-temp-file "ws-gptel-int-" t)))
    (unwind-protect
        (funcall body-fn home)
      (delete-directory home t))))

(describe "workspace-sessions-dir (producer)"

  (before-each (gptel-integration-spec--reset))

  (it "returns nil when no workspace owns the current tab"
    ;; workspace--current-name returns nil for stock (unregistered) tabs.
    (cl-letf (((symbol-function 'workspace--current-name) (lambda () nil)))
      (expect (workspace-sessions-dir) :to-be nil)))

  (it "returns nil when the current workspace is broken"
    (gptel-integration-spec--with-tmp-home
     (lambda (home)
       (let* ((name (file-name-nondirectory (directory-file-name home)))
              (ws   (workspace--mark-broken (workspace--make name home))))
         (puthash name ws workspace--registry)
         (cl-letf (((symbol-function 'workspace--current-name)
                    (lambda () name)))
           (expect (workspace-sessions-dir) :to-be nil))))))

  (it "returns nil when the workspace exists but sessions/ is missing"
    (gptel-integration-spec--with-tmp-home
     (lambda (home)
       (let* ((name (file-name-nondirectory (directory-file-name home)))
              (ws   (workspace--make name home)))
         (puthash name ws workspace--registry)
         ;; Deliberately do NOT create <home>/sessions/.
         (cl-letf (((symbol-function 'workspace--current-name)
                    (lambda () name)))
           (expect (workspace-sessions-dir) :to-be nil))))))

  (it "returns <:home>/sessions/ when workspace is healthy and sessions/ exists"
    (gptel-integration-spec--with-tmp-home
     (lambda (home)
       (let* ((name (file-name-nondirectory (directory-file-name home)))
              (ws   (workspace--make name home))
              (sessions (expand-file-name "sessions" home)))
         (make-directory sessions t)
         (puthash name ws workspace--registry)
         (cl-letf (((symbol-function 'workspace--current-name)
                    (lambda () name)))
           (expect (workspace-sessions-dir) :to-equal sessions))))))

  (it "never signals and has no *Messages* side effects across all branches"
    (let ((cases
           (list
            ;; no-workspace case
            (lambda ()
              (cl-letf (((symbol-function 'workspace--current-name)
                         (lambda () nil)))
                (workspace-sessions-dir)))
            ;; broken case
            (lambda ()
              (gptel-integration-spec--with-tmp-home
               (lambda (home)
                 (let* ((name (file-name-nondirectory
                               (directory-file-name home)))
                        (ws   (workspace--mark-broken
                               (workspace--make name home))))
                   (puthash name ws workspace--registry)
                   (cl-letf (((symbol-function 'workspace--current-name)
                              (lambda () name)))
                     (workspace-sessions-dir))))))
            ;; healthy case
            (lambda ()
              (gptel-integration-spec--with-tmp-home
               (lambda (home)
                 (let* ((name (file-name-nondirectory
                               (directory-file-name home)))
                        (ws   (workspace--make name home)))
                   (make-directory (expand-file-name "sessions" home) t)
                   (puthash name ws workspace--registry)
                   (cl-letf (((symbol-function 'workspace--current-name)
                              (lambda () name)))
                     (workspace-sessions-dir)))))))))
      (dolist (case cases)
        (let ((msg-before (with-current-buffer "*Messages*" (buffer-string))))
          (expect (funcall case) :not :to-throw)
          (let ((msg-after (with-current-buffer "*Messages*" (buffer-string))))
            (expect msg-after :to-equal msg-before)))))))

(describe "directionality: workspaces never references gptel"
  ;; Structural lint. Any future change that violates the one-way
  ;; contract by importing or naming gptel-sessions-* from inside
  ;; config/workspaces/ is a defect — this test catches it. The
  ;; producer's strict no-knowledge-of-gptel posture is what lets the
  ;; consumer's (featurep 'workspaces) guard stay sound.

  (it "no .el under config/workspaces/ contains any gptel-sessions-* symbol reference"
    (let* ((workspaces-dir
            (file-name-as-directory
             (expand-file-name ".." gptel-integration-spec--this-dir)))
           ;; Only inspect production sources; skip the test/ subdir
           ;; (this very file mentions gptel-sessions in comments).
           (production-files
            (seq-filter
             (lambda (f)
               (and (string-suffix-p ".el" f)
                    (not (string-match-p "/test/" f))))
             (directory-files-recursively workspaces-dir "\\.el\\'")))
           (regex "\\bgptel-sessions-\\|\\bgptel-sessions/\\|require[ \t]+'gptel-sessions")
           (violations nil))
      (dolist (file production-files)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (re-search-forward regex nil t)
            (push file violations))))
      (expect violations :to-equal nil))))

(provide 'gptel-integration-spec)
;;; gptel-integration-spec.el ends here
