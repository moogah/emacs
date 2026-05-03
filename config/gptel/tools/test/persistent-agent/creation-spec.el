;;; creation-spec.el --- Persistent-agent creation-flow tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs that pin the on-disk shape of agent session creation,
;; including validation errors, as documented in
;; openspec/changes/persistent-agent-rebuild/specs/persistent-agent/spec.md
;; (delta) §"Agent session creation", "Tool invocation and validation",
;; and "Error handling".
;;
;; Each `it` block carries a leading scenario-mapping comment so drift
;; between the spec and the implementation surfaces here first.
;;
;; These tests exercise the real filesystem (mkdir, write files); cleanup
;; runs via the `with-mock-parent-session' fixture's unwind-protect.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared persistent-agent fixtures from the co-located helpers file.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path spec-dir)
  (load (expand-file-name "helpers-spec.el" spec-dir) nil t))

;; Load the module under test.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tools-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path tools-dir)
  (require 'gptel-persistent-agent
           (expand-file-name "persistent-agent.el" tools-dir)))

;; Helper: open AGENT-DIR's `session.org' in `org-mode' so drawer
;; queries (`org-entry-get', `org-entry-get-multivalued-property')
;; route through the same parser the production loader uses
;; (`register/shape/drawer-text-block', `register/boundary/scope-profile-applicator').
(defmacro jf/persistent-agent-test--with-agent-session-org (agent-dir &rest body)
  "Open AGENT-DIR's session.org in a temp org buffer and run BODY there."
  (declare (indent 1) (debug t))
  `(let ((session-org (expand-file-name "session.org" ,agent-dir)))
     (with-temp-buffer
       (insert-file-contents session-org)
       (org-mode)
       ,@body)))

;;; Tests

(describe "PersistentAgent creation flow"

  (it "creates the agent directory under the parent branch"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Agent session
    ;; creation" -> "Agent directory created under parent branch"
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil))
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"))
         (let* ((agents-dir (expand-file-name "agents" mock-branch-dir))
                (entries    (and (file-directory-p agents-dir)
                                 (cl-remove-if
                                  (lambda (n) (member n '("." "..")))
                                  (directory-files agents-dir))))
                (agent-name (car entries))
                (agent-dir  (and agent-name
                                 (expand-file-name agent-name agents-dir))))
           (expect (file-directory-p agents-dir) :to-be t)
           (expect (length entries) :to-equal 1)
           (expect agent-name :to-match
                   "\\`test-preset-[0-9]\\{14\\}-analyze-code\\'")
           (expect (file-directory-p agent-dir) :to-be t)
           (expect (file-directory-p (expand-file-name "branches" agent-dir))
                   :to-be nil)
           (expect (file-exists-p (expand-file-name "current" agent-dir))
                   :to-be nil))))))

  (it "writes session.org with a self-describing :PROPERTIES: drawer"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Agent session
    ;; creation" -> "session.org carries a self-describing :PROPERTIES:
    ;; drawer".  The drawer carries `:GPTEL_PRESET:',
    ;; `:GPTEL_PARENT_SESSION_ID:', the agent's `:GPTEL_SCOPE_*:' keys
    ;; (Mode 2a — `register/boundary/scope-profile-applicator'), and
    ;; the resolved preset's chat-mode snapshot keys
    ;; (`:GPTEL_MODEL:', `:GPTEL_BACKEND:', etc. —
    ;; `register/shape/drawer-text-block', Decision 4 of
    ;; gptel-drawer-as-source-of-truth).  `:GPTEL_SYSTEM:' is NEVER
    ;; emitted (Decision 2 / `register/invariant/drawer-system-key-
    ;; write-exclusion').
    ;; With `allowed-paths' omitted, the drawer has the standard deny
    ;; set + `:GPTEL_SCOPE_WRITE: /tmp/**' but no `:GPTEL_SCOPE_READ:'.
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil))
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "DO THE THING"))
         (let* ((agents-dir (expand-file-name "agents" mock-branch-dir))
                (agent-name (car (cl-remove-if
                                  (lambda (n) (member n '("." "..")))
                                  (directory-files agents-dir))))
                (agent-dir  (expand-file-name agent-name agents-dir))
                (session-org (expand-file-name "session.org" agent-dir)))
           (expect (file-exists-p session-org) :to-be t)
           (jf/persistent-agent-test--with-agent-session-org agent-dir
             (expect (org-entry-get (point-min) "GPTEL_PRESET")
                     :to-equal "test-preset")
             (expect (org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")
                     :to-equal mock-session-id)
             ;; Chat-mode snapshot keys (Decision 4 / Layer 2 of
             ;; gptel-drawer-as-source-of-truth).  Structural
             ;; assertions (presence-only) so the test stays stable as
             ;; the resolved `gptel-backend' / `gptel-model' values
             ;; change between environments — the wire-snapshot
             ;; production path emits these from the resolved
             ;; `preset-spec' and forcing it nil drops the lines.
             (expect (org-entry-get (point-min) "GPTEL_MODEL")
                     :not :to-be nil)
             (expect (org-entry-get (point-min) "GPTEL_BACKEND")
                     :not :to-be nil)
             ;; :GPTEL_SYSTEM: must NEVER appear in the rendered
             ;; drawer (Decision 2 /
             ;; register/invariant/drawer-system-key-write-exclusion).
             (expect (org-entry-get (point-min) "GPTEL_SYSTEM")
                     :to-be nil)
             ;; No allowed-paths supplied ⇒ no `:GPTEL_SCOPE_READ:'.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_READ")
                     :to-be nil)
             ;; Standard write target: `/tmp/**'.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_WRITE")
                     :to-equal '("/tmp/**"))
             ;; Standard deny set hoisted to a defconst in
             ;; persistent-agent.org (cycle-2 rewire-persistent-agent);
             ;; reference the defconst so the test does not double-pin
             ;; the literal pattern strings.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_DENY")
                     :to-equal jf/gptel-persistent-agent--standard-deny-paths)
             ;; Body still carries the user prompt.
             (expect (buffer-string) :to-match "#\\+begin_user\nDO THE THING\n#\\+end_user"))
           ;; No `scope.yml' is produced (drawer-resident scope —
           ;; cycle-2 rewire-persistent-agent).
           (expect (file-exists-p (expand-file-name "scope.yml" agent-dir))
                   :to-be nil))))))

  (it "writes session.org drawer with :GPTEL_SCOPE_READ reflecting allowed paths"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Agent session
    ;; creation" -> "scope keys recorded in agent's session.org drawer".
    ;; Replaces the legacy "writes scope.yml with allowed paths" test;
    ;; drawer-resident scope is the sole route — no `scope.yml' sidecar.
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil))
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            '("/path/to/project/**" "/another/**")))
         (let* ((agents-dir (expand-file-name "agents" mock-branch-dir))
                (agent-name (car (cl-remove-if
                                  (lambda (n) (member n '("." "..")))
                                  (directory-files agents-dir))))
                (agent-dir  (expand-file-name agent-name agents-dir))
                (session-org (expand-file-name "session.org" agent-dir)))
           (expect (file-exists-p session-org) :to-be t)
           (jf/persistent-agent-test--with-agent-session-org agent-dir
             ;; Read patterns reflect the supplied allowed paths,
             ;; in order, with no inheritance from parent.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_READ")
                     :to-equal '("/path/to/project/**" "/another/**"))
             ;; Standard write target.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_WRITE")
                     :to-equal '("/tmp/**"))
             ;; Standard deny set referenced by defconst.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_DENY")
                     :to-equal jf/gptel-persistent-agent--standard-deny-paths)
             ;; Parent-session-id captured (positive assertion —
             ;; this drawer-key is the agent-side pointer to the
             ;; parent session).
             (expect (org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")
                     :to-equal mock-session-id))
           ;; No `scope.yml' is produced.
           (expect (file-exists-p (expand-file-name "scope.yml" agent-dir))
                   :to-be nil))))))

  (it "writes session.org drawer with no :GPTEL_SCOPE_READ when allowed-paths is omitted"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation
    ;; and validation" -> "Explicit path configuration (zero inheritance)".
    ;; Replaces the legacy "writes scope.yml with empty read paths" test.
    ;; The drawer has `:GPTEL_PRESET:', `:GPTEL_PARENT_SESSION_ID:',
    ;; `:GPTEL_SCOPE_WRITE:' and `:GPTEL_SCOPE_DENY:' but the
    ;; `:GPTEL_SCOPE_READ:' key is absent (zero inheritance).
    ;;
    ;; Note: this test pins the agent-side renderer's behaviour only —
    ;; downstream validator behaviour for the empty-allowed-paths case
    ;; is still subject to `disposition-empty-drawer-collapse'.
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil))
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"))
         (let* ((agents-dir (expand-file-name "agents" mock-branch-dir))
                (agent-name (car (cl-remove-if
                                  (lambda (n) (member n '("." "..")))
                                  (directory-files agents-dir))))
                (agent-dir  (expand-file-name agent-name agents-dir))
                (session-org (expand-file-name "session.org" agent-dir)))
           (expect (file-exists-p session-org) :to-be t)
           (jf/persistent-agent-test--with-agent-session-org agent-dir
             ;; `:GPTEL_SCOPE_READ:' is absent (zero inheritance).
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_READ")
                     :to-be nil)
             ;; Standard write + deny still present.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_WRITE")
                     :to-equal '("/tmp/**"))
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_DENY")
                     :to-equal jf/gptel-persistent-agent--standard-deny-paths)
             ;; Identity keys preserved.
             (expect (org-entry-get (point-min) "GPTEL_PRESET")
                     :to-equal "test-preset")
             (expect (org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")
                     :to-equal mock-session-id)
             ;; Mock-parent-session fixture writes no parent scope; the
             ;; agent's drawer should not carry any inherited pattern.
             (expect (buffer-string) :not :to-match "/path/to/project/")
             (expect (buffer-string) :not :to-match "/another/"))
           ;; No `scope.yml' is produced (negative assertion catches
           ;; regressions where the YAML write path is re-introduced).
           (expect (file-exists-p (expand-file-name "scope.yml" agent-dir))
                   :to-be nil))))))

  (it "rejects an unknown preset before any directory is created"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Error handling"
    ;; -> "Unknown preset rejected before any side effect"
    (jf/persistent-agent-test--with-mock-parent-session
     (let ((agents-dir (expand-file-name "agents" mock-branch-dir))
           (err nil))
       ;; Capture the user-error so we can both confirm it fired and
       ;; inspect its message.
       (condition-case caught
           (jf/gptel-persistent-agent--task
            #'ignore "nonexistent" "analyze code" "do the thing")
         (user-error (setq err caught)))
       (expect err :not :to-be nil)
       (expect (car-safe err) :to-equal 'user-error)
       (expect (error-message-string err) :to-match
               "Preset .nonexistent. not found")
       ;; agents/ MUST be empty (or nonexistent) — no side effects on the
       ;; failure path.
       (let ((entries (and (file-directory-p agents-dir)
                           (cl-remove-if
                            (lambda (n) (member n '("." "..")))
                            (directory-files agents-dir)))))
         (expect entries :to-equal nil)))))

  (it "rejects invocation outside a parent session"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation
    ;; and validation" -> "Parent session requirement"
    (let ((jf/gptel--session-dir nil))
      (let ((err nil))
        (condition-case caught
            (jf/gptel-persistent-agent--task
             #'ignore "any-preset" "analyze code" "do the thing")
          (user-error (setq err caught)))
        (expect err :not :to-be nil)
        (expect (error-message-string err) :to-match
                "PersistentAgent requires parent persistent session"))))

  (it "tool registration drops denied_paths from the args"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation
    ;; and validation" -> "Tool argument schema"
    (let* ((tool       (gptel-get-tool "PersistentAgent"))
           (args       (gptel-tool-args tool))
           (arg-names  (mapcar (lambda (a) (plist-get a :name)) args)))
      (expect tool :not :to-be nil)
      (expect arg-names :to-contain "preset")
      (expect arg-names :to-contain "description")
      (expect arg-names :to-contain "prompt")
      (expect arg-names :to-contain "allowed_paths")
      (expect arg-names :not :to-contain "denied_paths"))))

(provide 'creation-spec)
;;; creation-spec.el ends here
