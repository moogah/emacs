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
;; - :node-id (register/shape/workspace-plist-v3, optional, opaque)
;;   round-trips; a v3 plist lacking it loads with nil and NO notice
;;   (schema stays 3); `workspace-set-node-id' flushes synchronously.

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

(describe "v3 :node-id slot"
  ;; register/shape/workspace-plist-v3 (optional :node-id); workspaces
  ;; spec "Workspace node identity" + "Per-machine persistence and
  ;; restoration".
  (before-each (persistence-v3-spec--reset))
  (after-each (persistence-v3-spec--cleanup))

  (it "round-trips a node-id through serialize → write → read → deserialize"
    ;; Scenario: "Node ID round-trips through persistence".
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha"
                 (workspace--make "alpha" alpha-home "ABC")
                 workspace--registry)
        (workspace--write-state (workspace--serialize-registry))
        (clrhash workspace--registry)
        (workspace--deserialize-state (workspace--read-state))
        (expect (workspace-node-id (gethash "alpha" workspace--registry))
                :to-equal "ABC"))))

  (it "writes :node-id into the on-disk plist (whitelisted, unlike :broken)"
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha"
                 (workspace--make "alpha" alpha-home "ABC")
                 workspace--registry)
        (workspace--write-state (workspace--serialize-registry))
        (let ((on-disk (car (plist-get (persistence-v3-spec--read-raw)
                                       :workspaces))))
          (expect (plist-member on-disk :node-id) :not :to-be nil)
          (expect (plist-get on-disk :node-id) :to-equal "ABC")))))

  (it "loads a v3 plist lacking :node-id with nil and no *Messages* notice"
    ;; Scenario: "Absent node ID loads as nil".  Schema version stays 3:
    ;; the pre-slot file is accepted as-is, silently.
    (persistence-v3-spec--with-state-file
      (let* ((message-log-max t)
             (alpha-home (persistence-v3-spec--make-home "alpha"))
             (messages-before (with-current-buffer "*Messages*"
                                (buffer-string))))
        (persistence-v3-spec--write-raw
         `(:version 3
                    :workspaces ((:name "alpha"
                                        :home ,alpha-home
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (workspace--restore)
        (let ((alpha (gethash "alpha" workspace--registry))
              (messages-after (with-current-buffer "*Messages*"
                                (buffer-string))))
          (expect alpha :not :to-be nil)
          (expect (workspace--broken-p alpha) :to-be nil)
          (expect (workspace-node-id alpha) :to-be nil)
          ;; Preserved verbatim: the loader does not synthesize the key.
          (expect (plist-member alpha :node-id) :to-be nil)
          ;; No notice of any kind was emitted for the absent slot.
          (expect (substring messages-after (length messages-before))
                  :to-equal "")))))

  (it "loads an explicit :node-id nil silently as well"
    (persistence-v3-spec--with-state-file
      (let* ((message-log-max t)
             (alpha-home (persistence-v3-spec--make-home "alpha"))
             (messages-before (with-current-buffer "*Messages*"
                                (buffer-string))))
        (persistence-v3-spec--write-raw
         `(:version 3
                    :workspaces ((:name "alpha"
                                        :home ,alpha-home
                                        :node-id nil
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (workspace--restore)
        (expect (workspace-node-id (gethash "alpha" workspace--registry))
                :to-be nil)
        (expect (substring (with-current-buffer "*Messages*" (buffer-string))
                           (length messages-before))
                :to-equal "")))))

(describe "workspace-set-node-id"
  ;; The public identity setter: validates, mutates the LIVE record in
  ;; place, flushes synchronously (workspaces spec, "Per-machine
  ;; persistence and restoration": listed flush trigger; design D2).
  (before-each (persistence-v3-spec--reset))
  (after-each (persistence-v3-spec--cleanup))

  (it "flushes synchronously exactly once per call"
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha" (workspace--make "alpha" alpha-home) workspace--registry)
        (spy-on 'workspace--flush-state :and-call-through)
        (workspace-set-node-id (gethash "alpha" workspace--registry) "ABC")
        (expect 'workspace--flush-state :to-have-been-called-times 1))))

  (it "updates the registry's live record and returns it"
    (persistence-v3-spec--with-state-file
      (let* ((alpha-home (persistence-v3-spec--make-home "alpha"))
             (ws (workspace--make "alpha" alpha-home)))
        (puthash "alpha" ws workspace--registry)
        (expect (workspace-set-node-id ws "ABC") :to-be ws)
        (expect (workspace-node-id (gethash "alpha" workspace--registry))
                :to-equal "ABC"))))

  (it "persists the new identity immediately and restores it after a reload"
    ;; Scenario: "Node ID is persisted and restored".
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha" (workspace--make "alpha" alpha-home) workspace--registry)
        (workspace-set-node-id (gethash "alpha" workspace--registry) "ABC")
        ;; On disk right away — no debounce.
        (let ((on-disk (car (plist-get (persistence-v3-spec--read-raw)
                                       :workspaces))))
          (expect (plist-get on-disk :node-id) :to-equal "ABC"))
        ;; And back through a cold restore.
        (clrhash workspace--registry)
        (workspace--restore)
        (expect (workspace-node-id (gethash "alpha" workspace--registry))
                :to-equal "ABC"))))

  (it "mutates in place a pre-slot record that lacks the :node-id key"
    ;; A record hydrated from a file written before the slot existed has
    ;; no :node-id key at all; the setter must still update the very
    ;; object the registry holds (the ordering contract depends on it).
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (persistence-v3-spec--write-raw
         `(:version 3
                    :workspaces ((:name "alpha"
                                        :home ,alpha-home
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (workspace--restore)
        (let ((ws (gethash "alpha" workspace--registry)))
          (workspace-set-node-id ws "ABC")
          (expect (workspace-node-id ws) :to-equal "ABC")
          (expect (eq ws (gethash "alpha" workspace--registry)) :to-be t)
          (expect (workspace-node-id (gethash "alpha" workspace--registry))
                  :to-equal "ABC")))))

  (it "accepts nil to clear the identity, and flushes"
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha" (workspace--make "alpha" alpha-home "ABC") workspace--registry)
        (spy-on 'workspace--flush-state :and-call-through)
        (workspace-set-node-id (gethash "alpha" workspace--registry) nil)
        (expect (workspace-node-id (gethash "alpha" workspace--registry))
                :to-be nil)
        (expect 'workspace--flush-state :to-have-been-called-times 1))))

  (it "rejects a non-string node-id before mutating or flushing"
    (persistence-v3-spec--with-state-file
      (let ((alpha-home (persistence-v3-spec--make-home "alpha")))
        (puthash "alpha" (workspace--make "alpha" alpha-home "ABC") workspace--registry)
        (spy-on 'workspace--flush-state :and-call-through)
        (expect (workspace-set-node-id (gethash "alpha" workspace--registry) 42)
                :to-throw 'wrong-type-argument)
        (expect (workspace-node-id (gethash "alpha" workspace--registry))
                :to-equal "ABC")
        (expect 'workspace--flush-state :not :to-have-been-called)))))

(provide 'persistence-v3-spec)
;;; persistence-v3-spec.el ends here
