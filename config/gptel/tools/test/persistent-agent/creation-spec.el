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

;; The agent preamble is now sourced from the `agent-preamble' static
;; fragment under presets/sources/, which is not on `load-path' and is
;; loaded by `gptel.org' in production.  Load it by absolute path so the
;; composer's agent lead seam `jf/gptel-fragment-agent-preamble-text' is
;; populated with the pre-rendered preamble text (default is "" until
;; this loads).  The system-prompt writer reads that seam.
(load (expand-file-name "config/gptel/presets/sources/agent-preamble.el"
                        jf/emacs-dir)
      nil t)
(require 'jf-gptel-fragment-agent-preamble)

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
             ;; read_paths omitted ⇒ read scope is the work root alone:
             ;; `<work_root>/**' is auto-prepended so relative reads land
             ;; in scope (design.md D6; supersedes the old D7 guardrail).
             ;; Work root defaults to the parent buffer's `default-directory'.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_READ")
                     :to-equal
                     (list (concat (directory-file-name
                                    (expand-file-name default-directory))
                                   "/**")))
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
           ;; New signature: (... prompt work-root read-paths write-paths).
           ;; `read_paths' (6th positional, after `work_root') replaces the
           ;; read role of the removed `allowed_paths' (design.md D6).
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            "/path/to/project"
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

  (it "writes :GPTEL_SCOPE_READ as the work root alone when read_paths is omitted"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation
    ;; and validation" -> "Explicit path configuration (zero inheritance)".
    ;; Replaces the legacy "writes scope.yml with empty read paths" test.
    ;; Zero inheritance of the PARENT's read scope still holds — the agent
    ;; gets NO parent read patterns.  But the agent's OWN work root is
    ;; readable by construction: `<work_root>/**' is auto-prepended to
    ;; `:read' (design.md D6; supersedes the old D7 guardrail).  This is
    ;; self-consistency, not inheritance.
    ;;
    ;; Note: this test pins the agent-side renderer's behaviour only —
    ;; downstream validator behaviour for the empty-read-paths case
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
             ;; `:GPTEL_SCOPE_READ:' is the work root alone — no parent
             ;; patterns inherited.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_READ")
                     :to-equal
                     (list (concat (directory-file-name
                                    (expand-file-name default-directory))
                                   "/**")))
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

  (it "tool registration lists exactly the six params and excludes allowed_paths/denied_paths"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation
    ;; and validation" -> "Tool argument schema".  BREAKING rename
    ;; (design.md D6): `allowed_paths' is gone; the surface is the closed
    ;; six-param set {preset, description, prompt, work_root, read_paths,
    ;; write_paths} (register/vocabulary/agent-path-params closed_set).
    (let* ((tool       (gptel-get-tool "PersistentAgent"))
           (args       (gptel-tool-args tool))
           (arg-names  (mapcar (lambda (a) (plist-get a :name)) args)))
      (expect tool :not :to-be nil)
      (expect arg-names :to-equal
              '("preset" "description" "prompt"
                "work_root" "read_paths" "write_paths"))
      (expect arg-names :not :to-contain "allowed_paths")
      (expect arg-names :not :to-contain "denied_paths"))))

(describe "PersistentAgent session.org matches the canonical document layout"
  ;; After replace-system-prompt-heading-with-sibling-file, the
  ;; canonical agent layout is drawer at point-min followed directly
  ;; by the populated `#+begin_user' block — no `* System Prompt' or
  ;; `* Chat' heading.  The preset's `:system' is materialised in a
  ;; sibling `system-prompt.<ext>' file written by
  ;; `jf/gptel--write-system-prompt-sibling-file' and pointed at by
  ;; the drawer's `:GPTEL_SYSTEM_PROMPT_FILE:' key (covered by the
  ;; sibling-file describe block below).

  (it "emits the config drawer at point-min followed by the user-block prompt, with no headings"
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
                (session-org (expand-file-name "session.org" agent-dir))
                (content (with-temp-buffer
                           (insert-file-contents session-org)
                           (buffer-string))))
           ;; File-level config drawer at point-min.
           (expect content :to-match "\\`:PROPERTIES:\n")
           (expect content :to-match "\n:END:\n")
           ;; No `* System Prompt' or `* Chat' headings.
           (expect (string-match-p "^\\* System Prompt" content) :to-be nil)
           (expect (string-match-p "^\\* Chat" content) :to-be nil)
           (expect (string-match-p ":VISIBILITY: folded" content) :to-be nil)
           ;; Drawer is followed directly by the populated user block.
           (expect content :to-match
                   ":END:\n#\\+begin_user\nDO THE THING\n#\\+end_user\n\\'"))))))

  (it "the produced session.org has drawer-first ordering and exactly one user turn"
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
           (with-temp-buffer
             (insert-file-contents session-org)
             (save-excursion
               (goto-char (point-min))
               (expect (looking-at-p "[ \t\n]*:PROPERTIES:") :to-be t))
             ;; Exactly one user block; no assistant block yet.
             (expect (count-matches "^#\\+begin_user[ \t]*$"
                                    (point-min) (point-max))
                     :to-equal 1)
             (expect (count-matches "^#\\+begin_assistant[ \t]*$"
                                    (point-min) (point-max))
                     :to-equal 0)
             ;; Drawer's `:END:` precedes the user block.
             (let ((end-pos (save-excursion
                              (goto-char (point-min))
                              (re-search-forward "^:END:[ \t]*$" nil t)))
                   (turn-pos (save-excursion
                               (goto-char (point-min))
                               (re-search-forward
                                "^#\\+begin_user" nil t))))
               (expect end-pos :to-be-truthy)
               (expect turn-pos :to-be-truthy)
               (expect (< end-pos turn-pos) :to-be t)))))))))

(describe "PersistentAgent emits sibling system-prompt file"

  ;; Unlike the interactive session path (which writes the preset's
  ;; `:system' verbatim, or no file when there is none), the agent
  ;; creation path ALWAYS writes `system-prompt.<ext>' via
  ;; `jf/gptel-persistent-agent--write-system-prompt': the baseline
  ;; agent-harness preamble followed by the preset's `:system' body
  ;; when it declares one.  Every agent therefore has a system prompt
  ;; and a `:GPTEL_SYSTEM_PROMPT_FILE:' drawer key.  Agent directories
  ;; are flat (no `branches/' subdirectory — per
  ;; `gptel-session-constants'), so the sibling file lands directly
  ;; in the agent-dir.

  (it "writes system-prompt.md (preamble + preset :system) when preset has :system"
    (jf/persistent-agent-test--with-mock-parent-session
     (let ((preset-name 'sibling-agent-preset))
       (unwind-protect
           (progn
             (gptel-make-preset preset-name
               :description "agent preset with :system"
               :backend gptel-backend
               :model gptel-model
               :system "Agent operating instructions.\nBe terse.")
             (let ((captured nil))
               (jf/persistent-agent-test--with-mock-gptel-request captured
                 (jf/gptel-persistent-agent--task
                  #'ignore "sibling-agent-preset" "analyze code" "DO THE THING"))
               (let* ((agents-dir (expand-file-name "agents" mock-branch-dir))
                      (agent-name (car (cl-remove-if
                                        (lambda (n) (member n '("." "..")))
                                        (directory-files agents-dir))))
                      (agent-dir  (expand-file-name agent-name agents-dir))
                      (session-org (expand-file-name "session.org" agent-dir))
                      (sibling (expand-file-name "system-prompt.md" agent-dir))
                      (session-content (with-temp-buffer
                                         (insert-file-contents session-org)
                                         (buffer-string)))
                      (sibling-content (and (file-exists-p sibling)
                                            (with-temp-buffer
                                              (insert-file-contents sibling)
                                              (buffer-string)))))
                 ;; Sibling file exists; content is the baseline agent
                 ;; preamble followed by the preset's :system body.
                 (expect (file-exists-p sibling) :to-be t)
                 (expect sibling-content
                         :to-equal
                         (concat jf/gptel-fragment-agent-preamble-text
                                 "\n\n"
                                 "Agent operating instructions.\nBe terse."))
                 ;; Drawer carries :GPTEL_SYSTEM_PROMPT_FILE:.
                 (jf/persistent-agent-test--with-agent-session-org agent-dir
                   (expect (org-entry-get (point-min) "GPTEL_SYSTEM_PROMPT_FILE")
                           :to-equal "system-prompt.md")
                   ;; :GPTEL_SYSTEM: must still NEVER appear.
                   (expect (org-entry-get (point-min) "GPTEL_SYSTEM")
                           :to-be nil))
                 ;; Drawer + user block adjacency unchanged.
                 (expect session-content
                         :to-match
                         ":END:\n#\\+begin_user\nDO THE THING\n#\\+end_user\n\\'"))))
         (setq gptel--known-presets
               (assq-delete-all preset-name gptel--known-presets))))))

  (it "writes the preamble alone and still threads :GPTEL_SYSTEM_PROMPT_FILE: when preset has no :system"
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       ;; The mock preset has no `:system'; the agent writer still
       ;; emits the baseline harness preamble so every agent has a
       ;; system prompt (a behaviour change from the preset-only era).
       (let ((captured nil))
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "DO THE THING"))
         (let* ((agents-dir (expand-file-name "agents" mock-branch-dir))
                (agent-name (car (cl-remove-if
                                  (lambda (n) (member n '("." "..")))
                                  (directory-files agents-dir))))
                (agent-dir  (expand-file-name agent-name agents-dir))
                (sibling (expand-file-name "system-prompt.md" agent-dir))
                (sibling-content (and (file-exists-p sibling)
                                      (with-temp-buffer
                                        (insert-file-contents sibling)
                                        (buffer-string)))))
           (expect (file-exists-p sibling) :to-be t)
           (expect sibling-content
                   :to-equal jf/gptel-fragment-agent-preamble-text)
           (jf/persistent-agent-test--with-agent-session-org agent-dir
             (expect (org-entry-get (point-min) "GPTEL_SYSTEM_PROMPT_FILE")
                     :to-equal "system-prompt.md"))))))))

(describe "PersistentAgent system-prompt preamble"

  ;; openspec/specs/gptel/persistent-agent.md §Requirement: Agent
  ;; system-prompt preamble.  The preamble is the `agent-preamble' static
  ;; fragment (config/gptel/presets/sources/agent-preamble.org), wired
  ;; into the composer's agent lead seam `jf/gptel-fragment-agent-preamble-
  ;; text' and prepended to every agent's system prompt by
  ;; `jf/gptel-persistent-agent--write-system-prompt', independent of the
  ;; preset (the former hard-coded `jf/gptel-persistent-agent--system-
  ;; preamble' defconst is deleted).  Content-contract + writer-
  ;; composition unit tests (no gptel-request, no parent-session fixture
  ;; needed).

  (it "is a non-empty string"
    (expect (stringp jf/gptel-fragment-agent-preamble-text) :to-be t)
    (expect (string-blank-p jf/gptel-fragment-agent-preamble-text) :to-be nil))

  (it "is sourced from the agent-preamble fragment's committed artifact"
    ;; The preamble is no longer a hard-coded defconst — it is the
    ;; pre-rendered `agent-preamble' static fragment, consumed verbatim
    ;; through the composer's agent lead seam, and matches the committed
    ;; `.txt' artifact byte-for-byte.
    (expect jf/gptel-fragment-agent-preamble-text
            :to-equal
            (with-temp-buffer
              (insert-file-contents
               (expand-file-name
                "config/gptel/presets/sources/agent-preamble.txt"
                jf/emacs-dir))
              (buffer-string))))

  (it "forbids self-delegation via the PersistentAgent tool"
    ;; Scenario: Preamble forbids self-delegation — the delegation-loop fix.
    (expect jf/gptel-fragment-agent-preamble-text :to-match "PersistentAgent")
    (expect (downcase jf/gptel-fragment-agent-preamble-text)
            :to-match "do the task yourself"))

  (it "instructs the agent to return a single final message"
    (expect (downcase jf/gptel-fragment-agent-preamble-text)
            :to-match "final message"))

  (it "tells the agent it is headless and cannot ask follow-up questions"
    (expect (downcase jf/gptel-fragment-agent-preamble-text)
            :to-match "follow-up question"))

  (describe "jf/gptel-persistent-agent--write-system-prompt"

    (it "composes preamble + preset :system and returns the basename"
      ;; Scenario: Preamble composed ahead of a preset's :system.
      (let* ((dir (make-temp-file "pa-preamble-" t))
             (basename (jf/gptel-persistent-agent--write-system-prompt
                        dir 'some-preset '(:system "Role body."))))
        (unwind-protect
            (let ((content (with-temp-buffer
                             (insert-file-contents (expand-file-name basename dir))
                             (buffer-string))))
              (expect basename :to-equal "system-prompt.md")
              (expect content :to-equal
                      (concat jf/gptel-fragment-agent-preamble-text
                              "\n\n" "Role body.")))
          (delete-directory dir t))))

    (it "writes the preamble alone when the preset declares no :system"
      ;; Scenario: Preamble written alone when the preset has no :system.
      (let* ((dir (make-temp-file "pa-preamble-" t))
             (basename (jf/gptel-persistent-agent--write-system-prompt
                        dir 'some-preset '(:description "no system"))))
        (unwind-protect
            (let ((content (with-temp-buffer
                             (insert-file-contents (expand-file-name basename dir))
                             (buffer-string))))
              (expect basename :to-equal "system-prompt.md")
              (expect content :to-equal jf/gptel-fragment-agent-preamble-text))
          (delete-directory dir t))))

    (it "treats a whitespace-only :system as no :system (preamble alone)"
      (let* ((dir (make-temp-file "pa-preamble-" t))
             (basename (jf/gptel-persistent-agent--write-system-prompt
                        dir 'some-preset '(:system "   \n  "))))
        (unwind-protect
            (let ((content (with-temp-buffer
                             (insert-file-contents (expand-file-name basename dir))
                             (buffer-string))))
              (expect content :to-equal jf/gptel-fragment-agent-preamble-text))
          (delete-directory dir t)))))

  (it "leaves the shared interactive writer's output free of the preamble"
    ;; Scenario: Interactive sessions do not receive the preamble — the
    ;; shared writer keeps the preset's :system verbatim.
    (let* ((dir (make-temp-file "pa-interactive-" t))
           (basename (jf/gptel--write-system-prompt-sibling-file
                      dir 'some-preset '(:system "Interactive role body."))))
      (unwind-protect
          (let ((content (with-temp-buffer
                           (insert-file-contents (expand-file-name basename dir))
                           (buffer-string))))
            (expect content :to-equal "Interactive role body.")
            (expect content :not
                    :to-match (regexp-quote
                               (substring jf/gptel-fragment-agent-preamble-text
                                          0 30))))
        (delete-directory dir t)))))

(provide 'creation-spec)
;;; creation-spec.el ends here
