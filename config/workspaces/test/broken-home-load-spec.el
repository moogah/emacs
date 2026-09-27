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
(require 'seq)

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
      (let* ((message-log-max t)
             (missing-home (file-name-as-directory
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

;;; Remote (TRAMP) :home in the state file (user decision 2026-09-27:
;;; remote workspace homes are unsupported).  Loaded TAGGED BROKEN with no
;;; filesystem call on it, one warning naming it; never signals, never
;;; dropped.

(defconst broken-home-load-spec--remote-prefix "/wshl-remote:"
  "Prefix of the stand-in remote file names below.")

(defvar broken-home-load-spec--remote-touches nil
  "Filesystem operations the stand-in remote handler was asked, newest first.")

(defun broken-home-load-spec--remote-handler (operation &rest args)
  "A stand-in for TRAMP: names under the remote prefix are remote.
`file-remote-p' answers the prefix without connecting, as TRAMP does;
pure file-NAME operations use the default implementation.  Any other
operation is a filesystem touch (a real connection under TRAMP): it is
recorded and SIGNALS, so a load that stats the remote home fails the
spec instead of connecting anywhere."
  (let ((inhibit-file-name-handlers
         (cons 'broken-home-load-spec--remote-handler inhibit-file-name-handlers))
        (inhibit-file-name-operation operation))
    (pcase operation
      ('file-remote-p broken-home-load-spec--remote-prefix)
      ((or 'expand-file-name 'file-name-as-directory 'directory-file-name
           'file-name-directory 'file-name-nondirectory
           'file-name-sans-versions 'substitute-in-file-name
           'file-name-case-insensitive-p 'abbreviate-file-name)
       (apply operation args))
      (_
       (push operation broken-home-load-spec--remote-touches)
       (error "Remote filesystem touched: %S %S" operation args)))))

(describe "A remote :home in the state file"
  :var (remote-home local-home missing-home remote-groups)
  (before-each
    (broken-home-load-spec--reset)
    (setq broken-home-load-spec--remote-touches nil
          remote-home (concat broken-home-load-spec--remote-prefix "/srv/r/")
          local-home (file-name-as-directory
                      (expand-file-name "local" broken-home-load-spec--tmp-dir))
          missing-home (file-name-as-directory
                        (expand-file-name "gone" broken-home-load-spec--tmp-dir))
          remote-groups '((:name "home" :layouts nil)))
    (make-directory local-home t)
    (spy-on 'display-warning :and-return-value nil))
  (after-each (broken-home-load-spec--cleanup))

  (it "is loaded broken without any filesystem call on it, with one warning naming it, and its siblings still load"
    (broken-home-load-spec--with-state-file
      (broken-home-load-spec--write-raw
       `(:version 3
                  :workspaces ((:name "before" :home ,local-home
                                      :recent-layout-group nil
                                      :buffer-files nil :layout-groups nil)
                               (:name "r" :home ,remote-home
                                      :recent-layout-group "home"
                                      :buffer-files nil
                                      :layout-groups ,remote-groups)
                               (:name "gone" :home ,missing-home
                                      :recent-layout-group nil
                                      :buffer-files nil :layout-groups nil))))
      (let ((file-name-handler-alist
             (cons (cons (concat "\\`" (regexp-quote
                                         broken-home-load-spec--remote-prefix))
                         #'broken-home-load-spec--remote-handler)
                   file-name-handler-alist)))
        (expect (file-remote-p remote-home) :to-be-truthy)
        (workspace--restore)
        ;; Never contacted.
        (expect broken-home-load-spec--remote-touches :to-be nil)
        ;; Kept, tagged broken, data verbatim.
        (let ((r (gethash "r" workspace--registry)))
          (expect r :not :to-be nil)
          (expect (workspace--broken-p r) :to-be t)
          (expect (workspace--home r) :to-equal remote-home)
          (expect (workspace--layout-groups r) :to-equal remote-groups))
        ;; One warning, naming the entry and its home.
        (expect 'display-warning :to-have-been-called-times 1)
        (let ((args (spy-calls-args-for 'display-warning 0)))
          (expect (nth 0 args) :to-be 'workspaces)
          (expect (nth 1 args) :to-match "\"r\"")
          (expect (nth 1 args) :to-match (regexp-quote remote-home))
          (expect (nth 1 args) :to-match "not supported"))
        ;; Siblings on either side loaded normally.
        (expect (workspace--broken-p (gethash "before" workspace--registry))
                :to-be nil)
        (expect (workspace--broken-p (gethash "gone" workspace--registry))
                :to-be t)
        ;; It is never restored (refused as broken, still uncontacted) ...
        (expect (workspace-restore "r") :to-throw 'user-error)
        (expect broken-home-load-spec--remote-touches :to-be nil)
        ;; ... and it survives the next save, without the runtime tag.
        (let ((saved (seq-find (lambda (ws) (equal (plist-get ws :name) "r"))
                               (plist-get (workspace--serialize-registry)
                                          :workspaces))))
          (expect saved :not :to-be nil)
          (expect (plist-get saved :home) :to-equal remote-home)
          (expect (plist-get saved :layout-groups) :to-equal remote-groups)
          (expect (plist-member saved :broken) :to-be nil))))))

(provide 'broken-home-load-spec)
;;; broken-home-load-spec.el ends here
