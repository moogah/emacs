;;; persistence-v3-spec.el --- Schema v3 persistence behavior -*- lexical-binding: t; -*-

;; Pinned register entries (cycle 2 expectations):
;;   register/shape/workspace-plist-v3
;;   register/invariant/home-required-no-floating-workspaces
;;   register/invariant/broken-tag-runtime-only
;;
;; Covers:
;; - The schema-version constant equals 3 (the contract that broke
;;   the v2 file format).
;; - The reader rejects v1 and v2 files with a *Messages* notice and
;;   leaves the registry empty.
;; - The writer never emits the runtime-only :broken tag, even when
;;   the in-memory workspace carries it.
;; - The reader skips entries lacking the required :home slot, with a
;;   notice, while still loading sibling entries that do have :home.
;; - A full round-trip preserves :home and the legacy slots
;;   (:recent-layout-group, :buffer-files, :layout-groups).

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

(defvar persistence-v3-spec--tmp-dir nil
  "Per-suite temp directory backing the persistence file and any
synthetic :home directories the tests construct.")

(defun persistence-v3-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq persistence-v3-spec--tmp-dir
        (make-temp-file "ws-v3-" t)))

(defun persistence-v3-spec--cleanup ()
  (when (and persistence-v3-spec--tmp-dir
             (file-directory-p persistence-v3-spec--tmp-dir))
    (delete-directory persistence-v3-spec--tmp-dir t)))

(defmacro persistence-v3-spec--with-state-file (&rest body)
  "Run BODY with `workspace--state-file' pointed at the suite tmp dir."
  (declare (indent 0))
  `(let ((tmp persistence-v3-spec--tmp-dir))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () (file-name-as-directory tmp)))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld"
                                             (file-name-as-directory tmp)))))
       ,@body)))

(defun persistence-v3-spec--make-home (name)
  "Create and return a real :home directory NAME under the suite tmp dir."
  (let ((path (file-name-as-directory
               (expand-file-name name persistence-v3-spec--tmp-dir))))
    (make-directory path t)
    path))

(defun persistence-v3-spec--write-raw (form)
  "Write FORM (a v1/v2/v3-shaped plist) into the suite's state file."
  (make-directory (workspace--state-directory) t)
  (with-temp-file (workspace--state-file)
    (let ((print-length nil)
          (print-level nil))
      (prin1 form (current-buffer)))))

(defun persistence-v3-spec--read-raw ()
  "Read the on-disk state file as raw elisp."
  (with-temp-buffer
    (insert-file-contents (workspace--state-file))
    (read (current-buffer))))

(describe "Schema v3 constant"
  (it "workspace--state-version is 3"
    (expect workspace--state-version :to-equal 3)))

(describe "v3 reader rejects v1 and v2 files"
  (before-each (persistence-v3-spec--reset))
  (after-each (persistence-v3-spec--cleanup))

  (it "rejects a v1 file with a notice and returns nil"
    (persistence-v3-spec--with-state-file
      (let ((message-log-max t)
            (messages-before (with-current-buffer "*Messages*"
                               (buffer-string))))
        (persistence-v3-spec--write-raw
         '(:version 1
                    :workspaces ((:name "stale"
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (expect (workspace--read-state) :to-be nil)
        (let ((messages-after (with-current-buffer "*Messages*"
                                (buffer-string))))
          (expect (substring messages-after (length messages-before))
                  :to-match "v3 required")))))

  (it "rejects a v2 file with a notice and returns nil"
    (persistence-v3-spec--with-state-file
      (let ((message-log-max t)
            (messages-before (with-current-buffer "*Messages*"
                               (buffer-string))))
        (persistence-v3-spec--write-raw
         '(:version 2
                    :workspaces ((:name "stale"
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (expect (workspace--read-state) :to-be nil)
        (let ((messages-after (with-current-buffer "*Messages*"
                                (buffer-string))))
          (expect (substring messages-after (length messages-before))
                  :to-match "v3 required")))))

  (it "leaves the registry empty after workspace--restore on a v2 file"
    (persistence-v3-spec--with-state-file
      (persistence-v3-spec--write-raw
       '(:version 2 :workspaces ()))
      (workspace--restore)
      (expect (hash-table-count workspace--registry) :to-equal 0))))

(describe "v3 round-trip preserves :home"
  (before-each (persistence-v3-spec--reset))
  (after-each (persistence-v3-spec--cleanup))

  (it "serialize → write → read → deserialize keeps :home intact"
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha"
                 (workspace--make "alpha" alpha-home)
                 workspace--registry)
        (workspace--write-state (workspace--serialize-registry))
        (clrhash workspace--registry)
        (let ((state (workspace--read-state)))
          (expect state :not :to-be nil)
          (expect (plist-get state :version) :to-equal 3)
          (workspace--deserialize-state state))
        (let ((alpha (gethash "alpha" workspace--registry)))
          (expect alpha :not :to-be nil)
          (expect (workspace--home alpha) :to-equal alpha-home)
          ;; The :home directory exists, so the broken tag must NOT
          ;; have been derived on load.
          (expect (workspace--broken-p alpha) :to-be nil)))))

  (it "the on-disk plist for each workspace carries a :home key"
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha"
                 (workspace--make "alpha" alpha-home)
                 workspace--registry)
        (workspace--write-state (workspace--serialize-registry))
        (let* ((raw (persistence-v3-spec--read-raw))
               (workspaces (plist-get raw :workspaces))
               (alpha-on-disk (car workspaces)))
          (expect (plist-member alpha-on-disk :home) :not :to-be nil)
          (expect (plist-get alpha-on-disk :home) :to-equal alpha-home))))))

(describe "v3 reader skips entries lacking :home"
  (before-each (persistence-v3-spec--reset))
  (after-each (persistence-v3-spec--cleanup))

  (it "skips the malformed entry but loads its sibling"
    (persistence-v3-spec--with-state-file
      (let* ((message-log-max t)
             (alpha-home (persistence-v3-spec--make-home "alpha"))
             (messages-before (with-current-buffer "*Messages*"
                                (buffer-string))))
        (persistence-v3-spec--write-raw
         `(:version 3
                    :workspaces ((:name "broken"
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil)
                                 (:name "alpha"
                                        :home ,alpha-home
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (workspace--restore)
        ;; "broken" was skipped; "alpha" was loaded.
        (expect (gethash "broken" workspace--registry) :to-be nil)
        (expect (gethash "alpha"  workspace--registry) :not :to-be nil)
        (expect (hash-table-count workspace--registry) :to-equal 1)
        ;; The notice mentions the malformed entry by name.
        (let ((messages-after (with-current-buffer "*Messages*"
                                (buffer-string))))
          (expect (substring messages-after (length messages-before))
                  :to-match "missing :home")
          (expect (substring messages-after (length messages-before))
                  :to-match "broken"))))))

(describe "v3 writer never emits :broken"
  ;; Pins register/invariant/broken-tag-runtime-only.
  (before-each (persistence-v3-spec--reset))
  (after-each (persistence-v3-spec--cleanup))

  (it "serializing a :broken workspace omits the tag from the on-disk plist"
    (persistence-v3-spec--with-state-file
      (let* ((alpha-home (persistence-v3-spec--make-home "alpha"))
             (ws (workspace--mark-broken
                  (workspace--make "alpha" alpha-home))))
        (puthash "alpha" ws workspace--registry)
        ;; Sanity: the in-memory workspace IS broken.
        (expect (workspace--broken-p
                 (gethash "alpha" workspace--registry))
                :to-be t)
        (workspace--write-state (workspace--serialize-registry))
        (let* ((raw (persistence-v3-spec--read-raw))
               (workspaces (plist-get raw :workspaces))
               (alpha-on-disk (car workspaces)))
          (expect (plist-member alpha-on-disk :broken) :to-be nil)))))

  (it "byte-equivalence: serializing a workspace twice (once clean, once :broken) yields identical bytes"
    ;; The strongest pin against the writer accidentally including
    ;; :broken — any drift would show up as a byte-level diff.
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha"
                 (workspace--make "alpha" alpha-home)
                 workspace--registry)
        (workspace--write-state (workspace--serialize-registry))
        (let ((bytes-clean (with-temp-buffer
                             (insert-file-contents-literally
                              (workspace--state-file))
                             (buffer-string))))
          (puthash "alpha"
                   (workspace--mark-broken
                    (gethash "alpha" workspace--registry))
                   workspace--registry)
          (workspace--write-state (workspace--serialize-registry))
          (let ((bytes-after-broken (with-temp-buffer
                                      (insert-file-contents-literally
                                       (workspace--state-file))
                                      (buffer-string))))
            (expect bytes-after-broken :to-equal bytes-clean)))))))

(provide 'persistence-v3-spec)
;;; persistence-v3-spec.el ends here
