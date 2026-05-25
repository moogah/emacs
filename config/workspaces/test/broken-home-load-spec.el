;;; broken-home-load-spec.el --- Broken-home detection on load -*- lexical-binding: t; -*-

;; Pinned register entries:
;;   register/invariant/broken-tag-runtime-only (load-side derivation)
;;   register/invariant/home-required-no-floating-workspaces
;;
;; Covers the persistence deserializer's broken-home detection path
;; (design.md §D5, §D6): a v3 file naming a :home that no longer
;; exists on disk loads cleanly, the workspace is registered, the
;; :broken tag is set fresh from (file-directory-p :home), the missing
;; path is named in *Messages*, and no directory is created as a
;; side-effect of the load.

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)
(require 'frameset)

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

(defvar broken-home-load-spec--tmp-dir nil)

(defun broken-home-load-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq broken-home-load-spec--tmp-dir
        (make-temp-file "ws-broken-" t)))

(defun broken-home-load-spec--cleanup ()
  (when (and broken-home-load-spec--tmp-dir
             (file-directory-p broken-home-load-spec--tmp-dir))
    (delete-directory broken-home-load-spec--tmp-dir t)))

(defmacro broken-home-load-spec--with-state-file (&rest body)
  (declare (indent 0))
  `(let ((tmp broken-home-load-spec--tmp-dir))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () (file-name-as-directory tmp)))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld"
                                             (file-name-as-directory tmp)))))
       ,@body)))

(defun broken-home-load-spec--write-raw (form)
  (make-directory (workspace--state-directory) t)
  (with-temp-file (workspace--state-file)
    (let ((print-length nil)
          (print-level nil))
      (prin1 form (current-buffer)))))

(describe "Broken-home detection on load"
  (before-each (broken-home-load-spec--reset))
  (after-each (broken-home-load-spec--cleanup))

  (it "loads a workspace whose :home is missing, marks it :broken, names the path"
    (broken-home-load-spec--with-state-file
      (let* ((missing-home (file-name-as-directory
                            (expand-file-name "never-existed"
                                              broken-home-load-spec--tmp-dir)))
             (messages-before (with-current-buffer "*Messages*"
                                (buffer-string))))
        ;; Precondition: the directory really is missing.
        (expect (file-exists-p missing-home) :to-be nil)
        (broken-home-load-spec--write-raw
         `(:version 3
                    :workspaces ((:name "alpha"
                                        :home ,missing-home
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (workspace--restore)
        ;; The workspace is in the registry.
        (let ((alpha (gethash "alpha" workspace--registry)))
          (expect alpha :not :to-be nil)
          ;; The :home directory was named in the load-side notice.
          (let ((messages-after (with-current-buffer "*Messages*"
                                  (buffer-string))))
            (expect (substring messages-after (length messages-before))
                    :to-match "missing :home")
            (expect (substring messages-after (length messages-before))
                    :to-match (regexp-quote missing-home)))
          ;; The broken tag is set freshly via workspace--mark-broken
          ;; (NOT inlined as plist-put — the helper is the architect-
          ;; pinned attachment point; using it closes
          ;; arch-cycle-20260525-200459-2).
          (expect (workspace--broken-p alpha) :to-be t)
          ;; :home is preserved verbatim (the user may want to inspect
          ;; or restore the path).
          (expect (workspace--home alpha) :to-equal missing-home))
        ;; The load did NOT create the missing directory as a side
        ;; effect — `workspace--mark-broken' must be the only
        ;; observable response.
        (expect (file-exists-p missing-home) :to-be nil))))

  (it "is non-destructive: re-loading does not flip the :broken state once the dir reappears"
    ;; A property of the load-side derivation: marking is fresh on
    ;; every load.  If the user creates the missing directory between
    ;; sessions and restarts, the next load sees the dir and does NOT
    ;; mark the workspace broken.
    (broken-home-load-spec--with-state-file
      (let ((restored-home (file-name-as-directory
                            (expand-file-name "restored"
                                              broken-home-load-spec--tmp-dir))))
        ;; First load: directory is missing → broken.
        (broken-home-load-spec--write-raw
         `(:version 3
                    :workspaces ((:name "alpha"
                                        :home ,restored-home
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (workspace--restore)
        (expect (workspace--broken-p
                 (gethash "alpha" workspace--registry))
                :to-be t)
        ;; Simulate the user creating the directory between sessions.
        (clrhash workspace--registry)
        (make-directory restored-home t)
        ;; Second load: directory exists → NOT broken.
        (workspace--restore)
        (let ((alpha (gethash "alpha" workspace--registry)))
          (expect alpha :not :to-be nil)
          (expect (workspace--broken-p alpha) :to-be nil))))))

(provide 'broken-home-load-spec)
;;; broken-home-load-spec.el ends here
