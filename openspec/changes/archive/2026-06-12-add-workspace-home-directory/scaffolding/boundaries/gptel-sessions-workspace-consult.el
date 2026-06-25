;;; gptel-sessions-workspace-consult.el --- boundary mapping function -*- lexical-binding: t; -*-

;; scaffolding-of: register/boundary/gptel-sessions-workspace-consult
;; generated-at: 2026-05-25T21:35:00Z
;; license: implementor-may-revise
;;
;; Speculated boundary: the one-way soft-dependency consult that lets
;; gptel/sessions file new session files under <HOME>/sessions/ when
;; a workspace is active. Two canonical functions are pinned here:
;;
;;   - workspace-sessions-dir (producer; in config/workspaces/workspaces.el)
;;   - gptel-sessions--target-dir (consumer; in config/gptel/sessions/<tbd>.el)
;;
;; The exact gptel function the Implementor modifies is forward-pinned
;; to design.md Q1; the boundary contract holds regardless of which
;; gptel function turns out to be the right insertion point. The
;; Implementor of gptel-sessions-workspace-consult locates it during
;; apply and records the resolution in ## Discoveries.

(require 'buttercup)
(require 'cl-lib)

;; --- Canonical signature shells -------------------------------------

(defun workspace-sessions-dir ()
  "Return the sessions/ directory of the current workspace, or nil.

Returns nil when:
  - no workspace is the current tab's owner
    ((workspace--current-name) returns nil)
  - the current workspace is broken
    ((workspace--broken-p ws) returns t)
  - the workspace's :home/sessions/ directory is missing on disk
    ((file-directory-p dir) returns nil)

Returns the absolute path to <:home>/sessions/ otherwise.
MUST NOT signal. MUST have no side effects."
  (error "TODO: Implementor of gptel-sessions-workspace-consult \
replaces this body with the real producer-side function in \
config/workspaces/workspaces.el. The signature, nil-discipline, \
and three nil-conditions above are the speculated contract for \
register/boundary/gptel-sessions-workspace-consult."))

(defun gptel-sessions--target-dir (&optional force-global)
  "Return directory where a new gptel session file should be created.

When FORCE-GLOBAL is nil AND `workspaces' is loaded AND
`workspace-sessions-dir' is bound AND it returns a non-nil
directory: return that directory. Otherwise: return the global
default (`gptel-sessions-default-directory' or whatever the
existing variable in config/gptel/sessions/ is named — locate
during apply)."
  (ignore force-global)
  (error "TODO: Implementor of gptel-sessions-workspace-consult \
replaces this body with the consumer-side resolver in the \
appropriate file under config/gptel/sessions/. The (or ... \
gptel-sessions-default-directory) fallback chain pattern is in \
the register entry's consumer_side section."))

;; --- Behavioural contract test --------------------------------------

(describe "Boundary: gptel-sessions-workspace-consult"

  (describe "producer side: workspace-sessions-dir"

    (it "returns nil when no workspace is the current tab's owner"
      ;; Stub workspace--current-name to return nil; expect nil.
      (error "TODO: implement nil-on-no-workspace — \
(cl-letf (((symbol-function 'workspace--current-name) (lambda () nil))) \
  (expect (workspace-sessions-dir) :to-be nil))"))

    (it "returns nil when the current workspace is broken"
      ;; Build a registry entry whose workspace--broken-p returns t;
      ;; expect nil.
      (error "TODO: implement nil-on-broken — \
construct a broken workspace via workspace--make and \
workspace--mark-broken; stub workspace--current-name to return \
its :name; stub gethash to return the broken ws; \
(expect (workspace-sessions-dir) :to-be nil)."))

    (it "returns nil when the workspace exists but :home/sessions/ is missing"
      ;; Healthy workspace, but the sessions/ subdirectory was deleted.
      (error "TODO: implement nil-on-missing-sessions-dir — \
create a tmp dir as :home; do NOT create sessions/ underneath; \
construct a healthy workspace; expect (workspace-sessions-dir) nil."))

    (it "returns the expanded sessions/ path when the workspace is healthy and sessions/ exists"
      ;; The positive path. Asserts the value shape:
      ;; (expand-file-name \"sessions\" home).
      (error "TODO: implement positive case — \
create tmp dir as :home; make-directory <home>/sessions/; construct \
healthy workspace; stub current-name + registry; \
(expect (workspace-sessions-dir) :to-equal (expand-file-name \"sessions\" home))."))

    (it "never signals — never has side effects (no directory creation, no *Messages* writes)"
      ;; Structural property: capture *Messages* before/after; expect equality.
      ;; Capture default-directory; expect unchanged. Expect no error.
      (error "TODO: implement no-signal / no-side-effects assertion — \
(let ((msg-before (with-current-buffer \"*Messages*\" (buffer-string)))) \
  (ignore (condition-case nil (workspace-sessions-dir) (error nil))) \
  (let ((msg-after (with-current-buffer \"*Messages*\" (buffer-string)))) \
    (expect msg-after :to-equal msg-before)))")))

  (describe "consumer side: gptel-sessions--target-dir"

    (it "with workspaces loaded and a healthy current workspace, returns <home>/sessions/"
      ;; Stub workspace-sessions-dir to return a known string;
      ;; expect gptel-sessions--target-dir nil returns that string.
      (error "TODO: implement workspace-routes-to-home — \
(cl-letf (((symbol-function 'workspace-sessions-dir) (lambda () \"/tmp/x/sessions\"))) \
  (expect (gptel-sessions--target-dir nil) :to-equal \"/tmp/x/sessions\"))"))

    (it "with FORCE-GLOBAL non-nil, skips the workspace consult and returns the global default"
      ;; Stub workspace-sessions-dir to return a value; expect FORCE-GLOBAL t
      ;; still picks the global var.
      (error "TODO: implement force-global-escape-hatch — \
(cl-letf (((symbol-function 'workspace-sessions-dir) (lambda () \"/tmp/x/sessions\")) \
          (gptel-sessions-default-directory \"/var/global/sessions\")) \
  (expect (gptel-sessions--target-dir t) :to-equal \"/var/global/sessions\"))"))

    (it "with `workspaces' not loaded (featurep nil), returns the global default"
      ;; Simulate workspaces unfeatured; expect global default.
      (error "TODO: implement soft-dep-fallthrough — \
(cl-letf (((symbol-function 'featurep) \
           (lambda (sym &optional v) (and (not (eq sym 'workspaces)) v))) \
          (gptel-sessions-default-directory \"/var/global/sessions\")) \
  (expect (gptel-sessions--target-dir nil) :to-equal \"/var/global/sessions\"))"))

    (it "with workspace-sessions-dir returning nil (off-workspace), returns the global default"
      ;; The producer-side nil-discipline composed via `or'.
      (error "TODO: implement nil-falls-through-to-global — \
(cl-letf (((symbol-function 'workspace-sessions-dir) (lambda () nil)) \
          (gptel-sessions-default-directory \"/var/global/sessions\")) \
  (expect (gptel-sessions--target-dir nil) :to-equal \"/var/global/sessions\"))")))

  (describe "directionality: workspaces never references gptel"

    (it "no .el under config/workspaces/ contains any gptel-sessions-* symbol reference"
      ;; Structural assertion. Catches a future change that violates
      ;; the one-way contract by making workspaces aware of gptel.
      (error "TODO: implement directionality assertion — \
walk every .el under config/workspaces/ (excluding test/); for each, \
load into a temp buffer and grep for the regex \
\"\\\\bgptel-sessions-\\\\|\\\\bgptel-sessions/\\\\|require[ \\t]+'gptel-sessions\". \
Expect zero matches across the tree. Any match is a defect — \
the contract is one-way."))))

(provide 'gptel-sessions-workspace-consult)
;;; gptel-sessions-workspace-consult.el ends here
