;;; workspace-routing-spec.el --- Tests for gptel/workspaces session-dir routing -*- lexical-binding: t; -*-

;; Consumer-side contract for register/boundary/
;; gptel-sessions-workspace-consult.  Asserts that
;; `jf/gptel--target-sessions-root' (the project-prefixed canonical
;; producer named `gptel-sessions--target-dir' in the boundary
;; contract) routes correctly across the four required states:
;;
;;   - workspaces loaded + workspace-sessions-dir returns dir → that dir
;;   - workspaces loaded + workspace-sessions-dir returns nil  → global
;;   - workspaces NOT loaded (featurep returns nil)             → global
;;   - force-global = t (escape hatch)                           → global
;;
;; The end-to-end `jf/gptel--create-session-directory' path is also
;; exercised on disk to verify the new session subdirectory actually
;; lands under the resolved root.

(require 'buttercup)
(require 'cl-lib)
(require 'gptel-session-constants)
(require 'gptel-session-filesystem)

(defvar workspace-routing-spec--tempdirs nil
  "Temp dirs created during the suite for unwind-cleanup.")

(defun workspace-routing-spec--make-tmpdir ()
  (let ((dir (make-temp-file "gptel-ws-routing-" t)))
    (push dir workspace-routing-spec--tempdirs)
    dir))

(defun workspace-routing-spec--cleanup ()
  (dolist (d workspace-routing-spec--tempdirs)
    (when (file-directory-p d)
      (delete-directory d t)))
  (setq workspace-routing-spec--tempdirs nil))

(describe "jf/gptel--target-sessions-root (consumer-side resolver)"

  (after-each (workspace-routing-spec--cleanup))

  (it "routes to the workspace's sessions/ when workspaces is loaded and active"
    (let* ((workspace-sessions "/tmp/workspace-routing-fake/sessions")
           (jf/gptel-sessions-directory (workspace-routing-spec--make-tmpdir)))
      (cl-letf (((symbol-function 'workspace-sessions-dir)
                 (lambda () workspace-sessions))
                ;; Force featurep to report workspaces loaded for the
                ;; duration of this case, without actually loading the
                ;; workspaces package into the test image.
                ((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (sym &optional v)
                     (if (eq sym 'workspaces) t (funcall orig sym v))))))
        (expect (jf/gptel--target-sessions-root nil)
                :to-equal workspace-sessions))))

  (it "with FORCE-GLOBAL non-nil, skips the consult and returns the global default"
    (let* ((jf/gptel-sessions-directory (workspace-routing-spec--make-tmpdir)))
      (cl-letf (((symbol-function 'workspace-sessions-dir)
                 (lambda () "/tmp/should-not-be-used/sessions"))
                ((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (sym &optional v)
                     (if (eq sym 'workspaces) t (funcall orig sym v))))))
        (expect (jf/gptel--target-sessions-root t)
                :to-equal (expand-file-name jf/gptel-sessions-directory)))))

  (it "with workspaces NOT loaded (featurep returns nil), returns the global default"
    (let* ((jf/gptel-sessions-directory (workspace-routing-spec--make-tmpdir)))
      (cl-letf (((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (sym &optional v)
                     (if (eq sym 'workspaces) nil (funcall orig sym v))))))
        (expect (jf/gptel--target-sessions-root nil)
                :to-equal (expand-file-name jf/gptel-sessions-directory)))))

  (it "with workspace-sessions-dir returning nil, falls through to the global default"
    ;; The producer-side nil-discipline (off-workspace / broken / missing
    ;; sessions/) composes via `or' into the global fallback.
    (let* ((jf/gptel-sessions-directory (workspace-routing-spec--make-tmpdir)))
      (cl-letf (((symbol-function 'workspace-sessions-dir) (lambda () nil))
                ((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (sym &optional v)
                     (if (eq sym 'workspaces) t (funcall orig sym v))))))
        (expect (jf/gptel--target-sessions-root nil)
                :to-equal (expand-file-name jf/gptel-sessions-directory))))))

(describe "jf/gptel--create-session-directory end-to-end routing"

  (after-each (workspace-routing-spec--cleanup))

  (it "creates the session subdirectory under the workspace's sessions/"
    (let* ((home (workspace-routing-spec--make-tmpdir))
           (sessions (expand-file-name "sessions" home))
           (global-root (workspace-routing-spec--make-tmpdir))
           (jf/gptel-sessions-directory global-root))
      (make-directory sessions t)
      (cl-letf (((symbol-function 'workspace-sessions-dir)
                 (lambda () sessions))
                ((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (sym &optional v)
                     (if (eq sym 'workspaces) t (funcall orig sym v))))))
        (let ((session-dir (jf/gptel--create-session-directory "demo-20260420")))
          (expect (file-directory-p session-dir) :to-be t)
          (expect (file-name-directory (directory-file-name session-dir))
                  :to-equal (file-name-as-directory sessions))
          ;; AND the global root was NOT written to.
          (expect (directory-files global-root nil "^demo-20260420")
                  :to-equal nil)))))

  (it "creates the session subdirectory in the global root when FORCE-GLOBAL is non-nil"
    (let* ((home (workspace-routing-spec--make-tmpdir))
           (sessions (expand-file-name "sessions" home))
           (global-root (workspace-routing-spec--make-tmpdir))
           (jf/gptel-sessions-directory global-root))
      (make-directory sessions t)
      (cl-letf (((symbol-function 'workspace-sessions-dir)
                 (lambda () sessions))
                ((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (sym &optional v)
                     (if (eq sym 'workspaces) t (funcall orig sym v))))))
        (let ((session-dir (jf/gptel--create-session-directory "demo-forced" t)))
          (expect (file-directory-p session-dir) :to-be t)
          (expect (file-name-directory (directory-file-name session-dir))
                  :to-equal (file-name-as-directory
                             (expand-file-name global-root)))
          ;; AND the workspace sessions/ was NOT written to.
          (expect (directory-files sessions nil "^demo-forced")
                  :to-equal nil)))))

  (it "creates the session subdirectory in the global root when workspaces is not loaded"
    (let* ((global-root (workspace-routing-spec--make-tmpdir))
           (jf/gptel-sessions-directory global-root))
      (cl-letf (((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (sym &optional v)
                     (if (eq sym 'workspaces) nil (funcall orig sym v))))))
        (let ((session-dir (jf/gptel--create-session-directory "demo-no-ws")))
          (expect (file-directory-p session-dir) :to-be t)
          (expect (file-name-directory (directory-file-name session-dir))
                  :to-equal (file-name-as-directory
                             (expand-file-name global-root)))))))

  (it "creates the session subdirectory in the global root when workspace-sessions-dir returns nil"
    ;; Simulates the "current tab is not a workspace" case end-to-end.
    (let* ((global-root (workspace-routing-spec--make-tmpdir))
           (jf/gptel-sessions-directory global-root))
      (cl-letf (((symbol-function 'workspace-sessions-dir) (lambda () nil))
                ((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (sym &optional v)
                     (if (eq sym 'workspaces) t (funcall orig sym v))))))
        (let ((session-dir (jf/gptel--create-session-directory "demo-off-ws")))
          (expect (file-directory-p session-dir) :to-be t)
          (expect (file-name-directory (directory-file-name session-dir))
                  :to-equal (file-name-as-directory
                             (expand-file-name global-root))))))))

(provide 'workspace-routing-spec)
;;; workspace-routing-spec.el ends here
