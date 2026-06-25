;;; home-required-no-floating-workspaces.el --- invariant pin -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/home-required-no-floating-workspaces
;; generated-at: 2026-05-25T18:04:59Z
;; license: implementor-may-revise
;;
;; Invariant: every workspace in workspace--registry has :home set
;; to an absolute filesystem path. Floating workspaces are
;; unrepresentable post-this-change.
;;
;; Enforcement mechanism (per register entry): structural-audit at
;; the constructor (workspace--make signature requires HOME) +
;; persistence skip-with-notice for malformed serialized entries +
;; this test (property check over the live registry post each
;; mutation entry point).

(require 'buttercup)
(require 'cl-lib)

;; Load the workspaces data-model from the project root. The scaffold
;; resolves paths relative to its own location (4 levels under repo
;; root: openspec/changes/<change>/scaffolding/invariants/).
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name "../../../../../" here))
       (mod  (lambda (p) (expand-file-name p root))))
  (load (funcall mod "config/workspaces/data-model.el")))

(describe "Invariant: home-required-no-floating-workspaces"

  (it "workspace--make signature rejects construction without HOME"
    ;; Speculation: workspace--make's arglist is (NAME HOME); calling
    ;; with only NAME signals wrong-number-of-arguments. Pre-this-task
    ;; the arglist was (NAME); this assertion will FAIL until task
    ;; add-home-slot-to-data-model lands.
    (error "TODO: implement assertion — \
(expect (workspace--make \"alpha\") :to-throw 'wrong-number-of-arguments). \
After task add-home-slot-to-data-model lands, this should pass."))

  (it "workspace--make produces a workspace whose :home is set to HOME"
    (error "TODO: implement assertion — \
(let ((ws (workspace--make \"alpha\" \"/tmp/alpha/\"))) \
  (expect (workspace--home ws) :to-equal \"/tmp/alpha/\"))."))

  (it "every workspace in the registry has a non-nil absolute :home (property check)"
    ;; Property test over the live registry. Walks workspace--registry
    ;; (a hash table per data-model.org) and asserts the contract on
    ;; every entry. Catches retroactive regressions where a new code
    ;; path inserts into the registry without going through
    ;; workspace--make.
    (error "TODO: implement assertion — \
(maphash (lambda (_name ws) \
           (let ((home (workspace--home ws))) \
             (expect home :not :to-be nil) \
             (expect (stringp home) :to-be-truthy) \
             (expect (file-name-absolute-p home) :to-be-truthy))) \
         workspace--registry). \
Set up the registry fixture with at least one well-formed entry, then \
also test the corruption case: hand-puthash a no-:home plist and \
expect the property check to fail (proving the test actually checks)."))

  (it "workspace--home returns the :home slot value"
    (error "TODO: implement assertion — \
(let ((ws (list :name \"alpha\" :home \"/tmp/alpha/\"))) \
  (expect (workspace--home ws) :to-equal \"/tmp/alpha/\"))."))

  (it "workspace--home returns nil for a workspace missing the slot (defensive)"
    ;; This case exists because, even with the invariant in place, the
    ;; accessor must be defensive — a stale plist passed in from a
    ;; corrupted persistence file (caught by skip-with-notice but
    ;; defensively re-checked here) should return nil rather than error.
    (error "TODO: implement assertion — \
(let ((ws (list :name \"alpha\"))) \
  (expect (workspace--home ws) :to-be nil)).")))

(provide 'home-required-no-floating-workspaces)
;;; home-required-no-floating-workspaces.el ends here
