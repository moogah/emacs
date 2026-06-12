;;; registry-name-equals-basename.el --- invariant pin -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/registry-name-equals-basename
;; generated-at: 2026-05-25T18:04:59Z
;; license: implementor-may-revise
;;
;; Invariant: for every workspace ws in the registry,
;;   (equal (workspace--name ws)
;;          (file-name-nondirectory (directory-file-name (workspace--home ws))))
;;
;; Enforcement mechanism: test (property check over the live registry
;; after each mutation entry point: workspace--make, workspace-restore
;; deserialization, workspace-re-anchor). The structural-audit (no
;; code path mutates :name or :home in isolation) is a secondary
;; layer maintained by the Architect.

(require 'buttercup)
(require 'cl-lib)

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name "../../../../../" here))
       (mod  (lambda (p) (expand-file-name p root))))
  (load (funcall mod "config/workspaces/data-model.el")))

(defun ws-basename--basename-of (home)
  "Return the basename of HOME (the registry-name component)."
  (file-name-nondirectory (directory-file-name home)))

(describe "Invariant: registry-name-equals-basename"

  (it "workspace--make derives :name from HOME basename when invoked correctly"
    ;; Speculation: callers pass NAME explicitly AND ensure it matches
    ;; basename(HOME). workspace--make does not itself enforce equality
    ;; (the caller is responsible) — this test verifies the contract at
    ;; the canonical call site.
    (error "TODO: implement assertion — \
(let* ((home \"/tmp/alpha/\") \
       (name (ws-basename--basename-of home)) \
       (ws (workspace--make name home))) \
  (expect (workspace--name ws) :to-equal \
          (ws-basename--basename-of (workspace--home ws))))."))

  (it "every workspace in the registry satisfies name = basename(:home) (property check)"
    (error "TODO: implement assertion — \
(maphash (lambda (_key ws) \
           (let ((name (workspace--name ws)) \
                 (home (workspace--home ws))) \
             (expect name :to-equal (ws-basename--basename-of home)))) \
         workspace--registry). \
Fixture: at least one well-formed workspace. Negative case: hand- \
puthash a workspace whose name and basename disagree, then re-run \
the property check and expect it to fail (proves the assertion \
actually checks the contract)."))

  (it "trailing-slash differences do not produce false negatives"
    ;; basename normalization across /tmp/alpha vs /tmp/alpha/ vs
    ;; /tmp/alpha/. The directory-file-name normalisation should
    ;; produce the same basename regardless of trailing slash.
    (error "TODO: implement assertion — \
(dolist (variant '(\"/tmp/alpha\" \"/tmp/alpha/\" \"/tmp/alpha//\")) \
  (expect (ws-basename--basename-of variant) :to-equal \"alpha\"))."))

  (it "registry hash key equals workspace :name (registry-key = registry-name = basename(:home))"
    ;; The registry is a hash; the hash key under which a workspace is
    ;; stored MUST equal its :name. This is implied by the invariant
    ;; but worth a direct assertion because workspace-re-anchor's
    ;; rename path involves a remhash + puthash and must keep the
    ;; three values aligned.
    (error "TODO: implement assertion — \
(maphash (lambda (key ws) \
           (expect key :to-equal (workspace--name ws))) \
         workspace--registry)."))

  (it "out-of-band rename: workspace-re-anchor renames the registry key when basename changes"
    ;; Cycle-3+ scenario sketched here for forward-pinning. The
    ;; broken-home-tolerance task (cycle 3+) implements
    ;; workspace-re-anchor; this scaffold's expectation is that the
    ;; implementor honours the basename invariant when the new HOME's
    ;; basename differs from the old NAME.
    (error "TODO: implement assertion — \
(progn \
  (puthash \"alpha\" (workspace--make \"alpha\" \"/tmp/alpha/\") workspace--registry) \
  (workspace-re-anchor \"alpha\" \"/tmp/renamed/\") \
  (expect (gethash \"alpha\" workspace--registry) :to-be nil) \
  (expect (gethash \"renamed\" workspace--registry) :not :to-be nil) \
  (expect (workspace--home (gethash \"renamed\" workspace--registry)) \
          :to-equal \"/tmp/renamed/\")). \
This case is forward-pinned for cycle-3+; the cycle-1 Implementor of \
add-home-slot-to-data-model is NOT expected to make it pass.")))

(provide 'registry-name-equals-basename)
;;; registry-name-equals-basename.el ends here
