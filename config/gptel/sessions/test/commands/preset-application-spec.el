;;; preset-application-spec.el --- Preset application during auto-init -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verify preset application behavior in
;; `jf/gptel--auto-init-session-buffer':
;;
;; 1. The preset named in `metadata.yml' is applied via
;;    `gptel--apply-preset' with a buffer-local setter.
;; 2. `gptel-mode' is NOT called — session buffers run
;;    `gptel-chat-mode' exclusively (Decision 16).
;; 3. When both a property-drawer preset (simulated via a buffer-local
;;    `gptel--preset') AND a `metadata.yml'-declared preset are present,
;;    `metadata.yml' wins.  Sessions are authoritative for their own
;;    configuration (Decision 16, point 2).

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'yaml)

(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-metadata)
(require 'gptel-session-registry)
(require 'gptel-session-commands)

(defvar jf-gptel-preset-app-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-preset-app-test--register-cleanup (session-id branch-name)
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-preset-app-test--registry-keys))

(describe "Preset application during auto-init"

  (after-each
    (dolist (key jf-gptel-preset-app-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-preset-app-test--registry-keys nil))

  (describe "metadata.yml preset path"

    (it "calls gptel--apply-preset with buffer-local setter"
      (let ((buf (generate-new-buffer "session.org"))
            (apply-preset-args nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    "/sessions/sess-one/branches/main/session.org")
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"sess-one\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (name setter)
                           (setq apply-preset-args (list name setter))
                           ;; Exercise the setter to confirm it produces
                           ;; buffer-local bindings (not globals).
                           (when setter
                             (funcall setter 'gptel-model "from-preset"))))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) nil))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-preset-app-test--register-cleanup "sess-one" "main")

                (expect apply-preset-args :to-be-truthy)
                (expect (car apply-preset-args) :to-equal 'executor)
                (expect (functionp (cadr apply-preset-args)) :to-be t)
                ;; The setter's effect is buffer-local.
                (expect (local-variable-p 'gptel-model) :to-be t)
                (expect gptel-model :to-equal "from-preset")))
          (kill-buffer buf))))

    (it "does NOT call (gptel-mode 1)"
      (let ((buf (generate-new-buffer "session.org"))
            (gptel-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    "/sessions/sess-two/branches/main/session.org")
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"sess-two\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (_name _setter) nil))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) nil))
                        ((symbol-function 'gptel-mode)
                         (lambda (&optional _)
                           (setq gptel-mode-called t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-preset-app-test--register-cleanup "sess-two" "main")
                (expect gptel-mode-called :to-be nil)))
          (kill-buffer buf)))))

  (describe "precedence: metadata.yml wins over property drawer"

    (it "applies metadata.yml's preset even when gptel--preset is set locally"
      ;; Simulate a property drawer / file-local having set gptel--preset
      ;; to 'drawer-preset before auto-init runs.  metadata.yml names
      ;; 'executor.  Expect gptel--apply-preset to be invoked with
      ;; 'executor (from metadata.yml), not 'drawer-preset.
      (let ((buf (generate-new-buffer "session.org"))
            (apply-preset-calls nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    "/sessions/sess-three/branches/main/session.org")
              (setq-local gptel--preset 'drawer-preset)
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"sess-three\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (name)
                           (pcase name
                             ('executor '((gptel-model . "exec")))
                             ('drawer-preset '((gptel-model . "drawer")))
                             (_ nil))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (name _setter)
                           (push name apply-preset-calls)))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) nil))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-preset-app-test--register-cleanup "sess-three" "main")

                ;; Exactly one call, with 'executor from metadata.yml.
                (expect apply-preset-calls :to-equal '(executor))
                ;; The property-drawer preset is NOT applied during auto-init.
                (expect (memq 'drawer-preset apply-preset-calls)
                        :to-be nil)))
          (kill-buffer buf))))))

(provide 'preset-application-spec)
;;; preset-application-spec.el ends here
