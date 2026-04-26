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
    ;; drawer"
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil))
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "DO THE THING"))
         (let* ((agents-dir  (expand-file-name "agents" mock-branch-dir))
                (agent-name  (car (cl-remove-if
                                   (lambda (n) (member n '("." "..")))
                                   (directory-files agents-dir))))
                (agent-dir   (expand-file-name agent-name agents-dir))
                (session-org (expand-file-name "session.org" agent-dir))
                (content     (with-temp-buffer
                               (insert-file-contents session-org)
                               (buffer-string)))
                (expected    (format
                              ":PROPERTIES:\n:GPTEL_PRESET: %s\n:GPTEL_PARENT_SESSION_ID: %s\n:END:\n#+begin_user\n%s\n#+end_user\n"
                              "test-preset" mock-session-id "DO THE THING")))
           (expect (file-exists-p session-org) :to-be t)
           (expect content :to-equal expected))))))

  (it "writes scope.yml with allowed paths"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Agent session
    ;; creation" -> "scope.yml written via scope-module helper"
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
                (scope-yml  (expand-file-name "scope.yml" agent-dir))
                (content    (with-temp-buffer
                              (insert-file-contents scope-yml)
                              (buffer-string))))
           (expect (file-exists-p scope-yml) :to-be t)
           ;; paths.read contains both supplied patterns, exactly.
           (expect content :to-match "  read:\n    - \"/path/to/project/\\*\\*\"\n    - \"/another/\\*\\*\"\n")
           ;; paths.write is the constant /tmp/** entry.
           (expect content :to-match "  write:\n    - \"/tmp/\\*\\*\"\n")
           ;; paths.deny includes the four standard deny patterns.
           (expect content :to-match "    - \"\\*\\*/\\.git/\\*\\*\"")
           (expect content :to-match "    - \"\\*\\*/runtime/\\*\\*\"")
           (expect content :to-match "    - \"\\*\\*/\\.env\"")
           (expect content :to-match "    - \"\\*\\*/node_modules/\\*\\*\""))))))

  (it "writes scope.yml with empty read paths when allowed-paths is omitted"
    ;; Scenario: specs/persistent-agent/spec.md (delta) § "Tool invocation
    ;; and validation" -> "Explicit path configuration (zero inheritance)"
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
                (scope-yml  (expand-file-name "scope.yml" agent-dir))
                (content    (with-temp-buffer
                              (insert-file-contents scope-yml)
                              (buffer-string))))
           (expect (file-exists-p scope-yml) :to-be t)
           ;; Literal `read:\n    []` (zero inheritance).
           (expect content :to-match "  read:\n    \\[\\]\n")
           ;; The mock-parent-session fixture writes no parent scope.yml,
           ;; so absence of any inherited pattern is implicit; assert no
           ;; sample inherited pattern appears.
           (expect content :not :to-match "/path/to/project/")
           (expect content :not :to-match "/another/"))))))

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
