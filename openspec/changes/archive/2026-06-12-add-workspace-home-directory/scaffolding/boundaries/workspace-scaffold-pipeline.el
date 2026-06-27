;;; workspace-scaffold-pipeline.el --- boundary mapping function -*- lexical-binding: t; -*-

;; scaffolding-of: register/boundary/workspace-scaffold-pipeline
;; generated-at: 2026-05-25T21:35:00Z
;; license: implementor-may-revise
;;
;; Speculated boundary: the six-stage scaffold pipeline produced by
;; the scaffold-module task. This file holds the canonical signature
;; shell + a behavioural contract test against the stage-order and
;; INIT-AND-COMMIT? gating. The Implementor of scaffold-module is
;; expected to satisfy the assertions in either of two ways:
;;
;;   1. Implement `workspace-scaffold' with exactly the signature and
;;      contract speculated here (status flips speculated → confirmed).
;;   2. Push back on the speculation with a written reason in the
;;      task's ## Discoveries (status flips speculated → divergent and
;;      the entry is re-stated at integrate).
;;
;; The function shell below is intentionally a no-op; replacing its
;; body is the Implementor's job. The test scaffold below MUST FAIL
;; loudly until the real implementation lands — green-on-empty is a
;; defect of this scaffold, not a passing condition for the task.

(require 'buttercup)
(require 'cl-lib)

;; --- Canonical signature shell --------------------------------------

(defun workspace-scaffold (home name &rest plist)
  "Scaffold a workspace directory at HOME with display NAME.

Keyword arguments:
  :INIT-AND-COMMIT?  When non-nil, run `git init' (stage 2) and
                     `git add . && git commit' (stage 6). When nil,
                     skip those stages — caller is anchoring an
                     existing git repo without home.org and we
                     never touch the user's git state.

Stages (n = stage number; gated = always | INIT-AND-COMMIT?):
  1. ensure HOME directory                  (always)
  2. git init                               (INIT-AND-COMMIT?)
  3. write HOME/home.org skeleton           (always — guarded unless exists)
  4. ensure HOME/sessions/ directory        (always)
  5. create HOME/sessions/<date>-initial.org (always)
  6. git add . && git commit                (INIT-AND-COMMIT?)

Returns HOME on success. On any failure signals `user-error' with
the HOME path, failing stage/git args, and a remedy hint. Partial
filesystem state is LEFT IN PLACE — see invariant
`scaffold-leave-partial-on-failure'."
  (ignore home name plist)
  (error "TODO: Implementor of scaffold-module replaces this body \
with the real six-stage pipeline. The signature above is the \
speculated canonical mapping function for \
register/boundary/workspace-scaffold-pipeline; revise it (and this \
scaffold) if the contract turns out to need a different shape."))

;; --- Behavioural contract test --------------------------------------

(defconst workspace-scaffold-pipeline--repo-root
  (let ((here (file-name-directory (or load-file-name buffer-file-name))))
    (expand-file-name "../../../../../" here))
  "Repo root resolved relative to this scaffold's location.")

(defmacro workspace-scaffold-pipeline--with-tmp-home (binding &rest body)
  "Bind BINDING to a fresh tmp dir; delete on exit."
  (declare (indent 1))
  `(let ((,(car binding) (make-temp-file "ws-scaffold-test-" t)))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,(car binding))
         (delete-directory ,(car binding) t)))))

(describe "Boundary: workspace-scaffold-pipeline"

  (it "default path (INIT-AND-COMMIT? t) produces .git, home.org with #+TITLE, sessions/<date>-initial.org, and exactly one commit"
    ;; The default-path branch (the larger of the three INIT-AND-COMMIT?-t
    ;; consumers). The acceptance shape lifted from
    ;; tasks/open/scaffold-module.md's step 8 "default-path scaffold" case.
    (error "TODO: implement default-path acceptance — \
(workspace-scaffold-pipeline--with-tmp-home (home) \
  (let ((result (workspace-scaffold home \"alpha\" :init-and-commit? t))) \
    (expect result :to-equal home) \
    (expect (file-directory-p (expand-file-name \".git\" home)) :to-be-truthy) \
    (let ((homeorg (expand-file-name \"home.org\" home))) \
      (expect (file-exists-p homeorg) :to-be-truthy) \
      (expect (with-temp-buffer (insert-file-contents homeorg) (buffer-string)) \
              :to-match \"^#\\\\+TITLE: alpha$\")) \
    (let ((sessions (expand-file-name \"sessions\" home))) \
      (expect (file-directory-p sessions) :to-be-truthy) \
      (expect (length (directory-files sessions nil \"-initial\\\\.org\\\\'\")) \
              :to-equal 1)) \
    (let ((default-directory (file-name-as-directory home))) \
      (expect (split-string \
                 (shell-command-to-string \"git log --oneline\") \"\\n\" t) \
              :to-have-same-items-as '(\"<sha> Initial workspace\")))))"))

  (it "anchor-without-commit (INIT-AND-COMMIT? nil) writes files but does not init git or commit"
    ;; Cycle-3 consumer: workspace-new-anchor-existing's "repo without
    ;; home.org" sub-case. Speculation: the pipeline takes the same path
    ;; minus stages 2 and 6.
    (error "TODO: implement anchor-without-commit acceptance — \
(workspace-scaffold-pipeline--with-tmp-home (home) \
  ;; Pre-init the dir as a git repo so stage 2 isn't relevant. \
  (let ((default-directory (file-name-as-directory home))) \
    (call-process \"git\" nil nil nil \"init\")) \
  (workspace-scaffold home \"beta\" :init-and-commit? nil) \
  (expect (file-exists-p (expand-file-name \"home.org\" home)) :to-be-truthy) \
  (expect (file-directory-p (expand-file-name \"sessions\" home)) \
          :to-be-truthy) \
  ;; Files are present but UNTRACKED — the function did not run \
  ;; `git add' or `git commit'. \
  (let ((default-directory (file-name-as-directory home))) \
    (expect (shell-command-to-string \"git status --porcelain\") \
            :to-match \"^?? home\\\\.org$\\\\|^?? sessions/$\")))"))

  (it "stage 3 is idempotent: pre-existing home.org content is preserved"
    ;; The (unless (file-exists-p path) ...) guard is the contract that
    ;; makes the "anchor existing repo + user pre-created home.org"
    ;; sub-case safe.
    (error "TODO: implement idempotency case — \
(workspace-scaffold-pipeline--with-tmp-home (home) \
  (let ((homeorg (expand-file-name \"home.org\" home)) \
        (custom \"#+TITLE: hand-written\\n* My content\\n\")) \
    (with-temp-file homeorg (insert custom)) \
    (workspace-scaffold home \"gamma\" :init-and-commit? nil) \
    (expect (with-temp-buffer (insert-file-contents homeorg) (buffer-string)) \
            :to-equal custom)))"))

  (it "stage 5 is the SINGLE producer of sessions/<date>-initial.org (no other call site writes that path)"
    ;; Structural assertion that catches a future workspace command
    ;; that mints initial sessions outside the scaffold path. The
    ;; cycle-2 scaffold-module's :sessions producer is the only one.
    (error "TODO: implement single-producer assertion — \
walk every .el under config/workspaces/ (excluding test/); for each, \
load into temp buffer and grep for the substring \"-initial.org\". \
Expect at most ONE such file: scaffold.el. Multiple producers is a \
defect — surfaces as architect signal-class 4 (cross-task duplication).")))

(provide 'workspace-scaffold-pipeline)
;;; workspace-scaffold-pipeline.el ends here
