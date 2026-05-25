;;; broken-tag-runtime-only.el --- invariant pin -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/broken-tag-runtime-only
;; generated-at: 2026-05-25T21:35:00Z
;; license: implementor-may-revise
;;
;; Invariant: the :broken plist tag is a runtime OBSERVATION about
;; the current filesystem state of :home. It is set at load time by
;; the deserializer's (file-directory-p :home) check or by an
;; explicit workspace--mark-broken call. It is NEVER serialized to
;; disk — the persistence writer MUST filter it out before writing.
;;
;; Enforcement mechanism: test (the persistence round-trip with
;; :broken set; assert the tag is absent from disk). Architect
;; signal-class 7 (interface-drift) catches a serializer revision
;; that emits :broken by inspecting the function body against the
;; entry's statement.

(require 'buttercup)
(require 'cl-lib)

(describe "Invariant: broken-tag-runtime-only"

  (it "serializing a broken workspace produces an on-disk plist with no :broken key"
    ;; The round-trip assertion. Construct a workspace, mark broken,
    ;; serialize via the persistence write path, read the file back
    ;; as plain elisp, assert (plist-member ... :broken) is nil.
    (error "TODO: implement serialize-omits-broken — \
(let* ((home (make-temp-file \"ws-rt-\" t)) \
       (registry-file (expand-file-name \"registry.eld\" home))) \
  (unwind-protect \
      (let ((workspace--registry (copy-hash-table workspace--registry))) \
        (cl-letf (((symbol-function 'workspace--persistence-file-path) \
                   (lambda () registry-file))) \
          (let ((ws (workspace--make :name \"alpha\" :home home))) \
            (workspace--mark-broken ws) \
            (puthash \"alpha\" ws workspace--registry) \
            (workspace--persistence-save) \
            (let* ((on-disk (with-temp-buffer \
                              (insert-file-contents registry-file) \
                              (read (current-buffer))))) \
              ;; on-disk shape depends on persistence schema layout; \
              ;; the assertion is: the alpha workspace's plist on disk \
              ;; has no :broken member. Adjust the navigation to match \
              ;; the actual on-disk shape. \
              (let ((alpha-plist (cdr (assoc \"alpha\" (plist-get on-disk :workspaces))))) \
                (expect (plist-member alpha-plist :broken) :to-be nil)))))) \
    (delete-directory home t)))"))

  (it "loading a v3 file whose workspace points at a missing :home yields a broken workspace"
    ;; The load-side complement: even though :broken was never on disk,
    ;; the deserializer re-derives the broken state from the live
    ;; (file-directory-p :home) check.
    (error "TODO: implement deserialize-derives-broken — \
(let* ((tmp (make-temp-file \"ws-load-\" t)) \
       (missing-home (expand-file-name \"never-existed\" tmp)) \
       (registry-file (expand-file-name \"registry.eld\" tmp))) \
  (unwind-protect \
      (let ((workspace--registry (copy-hash-table workspace--registry))) \
        (cl-letf (((symbol-function 'workspace--persistence-file-path) \
                   (lambda () registry-file))) \
          ;; Write a v3 file with one workspace whose :home is missing. \
          (with-temp-file registry-file \
            (prin1 (list :version 3 \
                         :workspaces (list (cons \"alpha\" (list :name \"alpha\" :home missing-home)))) \
                   (current-buffer))) \
          (workspace--persistence-load) \
          (let ((ws (gethash \"alpha\" workspace--registry))) \
            (expect ws :not :to-be nil) \
            (expect (workspace--broken-p ws) :to-be-truthy)))) \
    (delete-directory tmp t)))"))

  (it "the round-trip is idempotent: serialize → load → serialize produces byte-equivalent output, broken-state aside"
    ;; A property test: marking a workspace broken and re-saving does
    ;; not change the on-disk bytes vs. saving without marking broken.
    ;; Pins that :broken is NOT a content-discriminating attribute.
    (error "TODO: implement broken-roundtrip-byte-equivalence — \
(let* ((tmp (make-temp-file \"ws-rt-\" t)) \
       (home (let ((p (expand-file-name \"alpha\" tmp))) \
               (make-directory p t) p)) \
       (registry-file (expand-file-name \"registry.eld\" tmp))) \
  (unwind-protect \
      (let ((workspace--registry (copy-hash-table workspace--registry))) \
        (cl-letf (((symbol-function 'workspace--persistence-file-path) \
                   (lambda () registry-file))) \
          (let ((ws (workspace--make :name \"alpha\" :home home))) \
            (puthash \"alpha\" ws workspace--registry) \
            (workspace--persistence-save) \
            (let ((bytes-clean (with-temp-buffer \
                                 (insert-file-contents-literally registry-file) \
                                 (buffer-string)))) \
              (workspace--mark-broken ws) \
              (workspace--persistence-save) \
              (let ((bytes-after-broken \
                     (with-temp-buffer \
                       (insert-file-contents-literally registry-file) \
                       (buffer-string)))) \
                (expect bytes-after-broken :to-equal bytes-clean)))))) \
    (delete-directory tmp t)))"))

  (it "the persistence-write function body does not reference :broken (structural lint)"
    ;; Architect signal-class 7 enforcement, expressed as a test.
    ;; The serializer must filter :broken by construction, not by
    ;; assignment. A reference to :broken in the writer would mean
    ;; the omission is conditional and could regress.
    (error "TODO: implement no-broken-in-serializer lint — \
(let* ((repo-root (let ((here (file-name-directory \
                               (or load-file-name buffer-file-name)))) \
                     (expand-file-name \"../../../../../\" here))) \
       (persistence-el (expand-file-name \"config/workspaces/persistence.el\" repo-root))) \
  (when (file-readable-p persistence-el) \
    (with-temp-buffer \
      (insert-file-contents persistence-el) \
      ;; The serializer function name is determined by the cycle-2 \
      ;; Implementor (likely workspace--persistence-serialize-workspace \
      ;; per the task body). The assertion below is heuristic — adjust \
      ;; to grep within the actual serializer's defun body if the name \
      ;; differs. \
      (goto-char (point-min)) \
      (let ((found (re-search-forward \":broken\" nil t))) \
        ;; Allow :broken in the LOAD path; only flag the WRITE path. \
        ;; Refining this assertion is the Implementor's job — note in \
        ;; ## Discoveries how the write path's structure makes the lint \
        ;; reliable. \
        (when found \
          (expect (save-excursion \
                    (goto-char found) \
                    (re-search-backward \"^(defun \\\\([^ ]+\\\\)\" nil t) \
                    (match-string 1)) \
                  :not :to-match \"serialize\\\\|save\\\\|write\")))))). \
The file may not exist yet at this scaffold's run time; the \
file-readable-p guard makes the assertion vacuously pass until \
persistence-schema-v3 lands.")))

(provide 'broken-tag-runtime-only)
;;; broken-tag-runtime-only.el ends here
