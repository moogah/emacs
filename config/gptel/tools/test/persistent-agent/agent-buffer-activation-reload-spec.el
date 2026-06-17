;;; agent-buffer-activation-reload-spec.el --- Persistent-agent buffer activation + reload tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for the persistent-agent module's reliance on
;; content-addressed session activation.  Verifies four contracts:
;;
;; 1. Agent buffer activates `gptel-chat-mode' on creation, with the
;;    session vars (`jf/gptel--session-id', `jf/gptel--branch-name',
;;    `jf/gptel-autosave-enabled') set buffer-locally by the
;;    `gptel-chat-mode-hook' identity binder.  Activation itself is
;;    content-addressed: `magic-mode-alist' recognises the session.org
;;    `:PROPERTIES:' drawer signature and selects `gptel-chat-mode'.
;;
;; 2. Agent buffer is registered in `jf/gptel--session-registry' under
;;    its own session-id (not the parent's), retrievable via
;;    `jf/gptel-session-find'.
;;
;; 3. A saved agent `session.org' reloads as an interactive chat
;;    session: kill the agent buffer, `find-file-noselect' the same
;;    path again, and the result is `gptel-chat-mode' with the same
;;    drawer-declared preset applied (backend/model from the preset),
;;    same branch/session ids restored, same parent-session-id
;;    installed from the drawer.
;;
;; 4. The persistent-agent module installs NO agent-specific autosave
;;    hook on `gptel-post-response-functions' (default value or buffer-
;;    local) — persistence is delegated to the chat-mode activation
;;    path.
;;
;; Real `find-file-noselect' is used (no mocking of file I/O); the
;; assertions land on the side effect of content-addressed activation
;; (`magic-mode-alist' → `gptel-chat-mode' → `gptel-chat-mode-hook'
;; identity binder).  Only `gptel-request' is mocked, so no network
;; call fires.
;;
;; Parent-buffer pattern: the shared
;; `jf/persistent-agent-test--with-mock-parent-session' fixture binds
;; the four session vars via `let' (dynamic binding).  The chat-mode
;; identity binder's fast-path guard `(not (bound-and-true-p
;; jf/gptel--session-id))' would see the dynamic let value when the
;; hook fires in the freshly-created agent buffer (which has no
;; buffer-local override), and would bail out — defeating the test.
;; We therefore shadow the let-bound vars with nil and re-install them
;; as buffer-local values in a dedicated parent buffer, so the dynamic
;; binding in any other buffer (the new agent buffer) is nil and the
;; identity binder proceeds.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Resolve the shared fixtures module via an explicit path — the test
;; runner loads each spec individually with `load-file', so the test
;; directory is not on the load-path at load time.  Mirrors the pattern
;; used by `config/gptel/scope/test/expansion/expansion-integration-spec.el'.
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (require 'jf-persistent-agent-test-helpers
           (expand-file-name "helpers-spec.el" test-dir)))

;; Production modules under test.
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-commands)        ; session create/open commands
(require 'gptel-chat-mode)
(require 'gptel-chat-menu)               ; installs apply-declared-preset hook
(require 'gptel-persistent-agent)
(require 'gptel)

(defun jf-pa-auto-init-test--find-agent-buffer (branch-dir)
  "Return the agent buffer visiting a `session.org' under BRANCH-DIR/agents/.
Walks `(buffer-list)' for buffers whose `buffer-file-name' lies under
the parent BRANCH-DIR's `agents/' subtree.  Compares via
`file-truename' to absorb macOS `/var' → `/private/var' resolution.
Returns nil if no such buffer is open."
  (let ((agents-prefix (file-name-as-directory
                        (file-truename
                         (expand-file-name "agents" branch-dir)))))
    (cl-find-if
     (lambda (buf)
       (when-let* ((file (buffer-file-name buf)))
         (string-prefix-p agents-prefix (file-truename file))))
     (buffer-list))))

(defun jf-pa-auto-init-test--cleanup-agent-buffer (agent-buffer)
  "Kill AGENT-BUFFER and remove its registry entry, if any.
Safe to call with a nil or already-dead AGENT-BUFFER."
  (when (and agent-buffer (buffer-live-p agent-buffer))
    (let ((sid (buffer-local-value 'jf/gptel--session-id agent-buffer))
          (bname (buffer-local-value 'jf/gptel--branch-name agent-buffer)))
      (when (and sid bname)
        (remhash (jf/gptel--registry-key sid bname)
                 jf/gptel--session-registry)))
    (with-current-buffer agent-buffer
      (set-buffer-modified-p nil))
    (kill-buffer agent-buffer)))

(defmacro jf-pa-auto-init-test--with-parent-buffer (&rest body)
  "Shadow the fixture's let-bound session vars with a buffer-local parent.

The fixture macro `jf/persistent-agent-test--with-mock-parent-session'
binds the four session vars via `let' (dynamic).  In tests that drive
`find-file-noselect', that dynamic binding leaks into the freshly-
created agent buffer's view of `jf/gptel--session-id', causing the
auto-init guard `(not (bound-and-true-p ...))' to bail out.

This macro shadows those let-bound vars with nil for BODY's duration
and re-establishes them as buffer-local values in a dedicated parent
buffer (`pa-parent-buffer'), inside which BODY runs.  The new agent
buffer created by `find-file-noselect' sees nil for the vars (no
buffer-local override, dynamic value is nil) and auto-init proceeds.

Inside BODY, `pa-parent-buffer' names the parent buffer."
  (declare (indent 0) (debug t))
  `(let ((jf/gptel--session-id nil)
         (jf/gptel--session-dir nil)
         (jf/gptel--branch-name nil)
         (jf/gptel--branch-dir nil)
         (pa-parent-buffer (generate-new-buffer "*pa-parent*")))
     (unwind-protect
         (with-current-buffer pa-parent-buffer
           (setq-local jf/gptel--session-id mock-session-id)
           (setq-local jf/gptel--session-dir mock-session-dir)
           (setq-local jf/gptel--branch-name "main")
           (setq-local jf/gptel--branch-dir mock-branch-dir)
           ;; Pin the parent buffer's work root so a `work_root'-omitting
           ;; agent freezes a KNOWN default into its `:GPTEL_WORK_ROOT:'
           ;; drawer key (design.md D5).  Without this the default would be
           ;; the test process's CWD (the worktree root), which the
           ;; activation assertions below cannot predict.
           (setq-local default-directory (file-name-as-directory mock-branch-dir))
           ,@body)
       (when (buffer-live-p pa-parent-buffer)
         (kill-buffer pa-parent-buffer)))))

(describe "persistent-agent buffer activation + reload"

  (describe "agent buffer activates gptel-chat-mode on creation"

    ;; Scenario: specs/persistent-agent/spec.md (delta) §
    ;; "Agent session creation" → "Agent buffer activates via
    ;; content-addressed magic-mode recognition"
    (it "the agent buffer is in gptel-chat-mode after creation"
      (let ((preset 'pa-auto-init-preset)
            (agent-buffer nil)
            (captured nil))
        (unwind-protect
            (jf/persistent-agent-test--with-mock-parent-session
              (jf/persistent-agent-test--with-mock-preset preset
                (jf-pa-auto-init-test--with-parent-buffer
                  (jf/persistent-agent-test--with-mock-gptel-request captured
                    (jf/gptel-persistent-agent--task
                     #'ignore (symbol-name preset) "auto-init test" "do the thing"))
                  (setq agent-buffer
                        (jf-pa-auto-init-test--find-agent-buffer mock-branch-dir))
                  (expect agent-buffer :to-be-truthy)
                  ;; Major mode is gptel-chat-mode (selected by
                  ;; content-addressed `magic-mode-alist' recognition of
                  ;; the drawer signature, not by the agent module
                  ;; directly).
                  (expect (buffer-local-value 'major-mode agent-buffer)
                          :to-equal 'gptel-chat-mode)
                  ;; Agent's session-id is buffer-local and non-empty.
                  ;; Identity is bound by the `gptel-chat-mode-hook'
                  ;; binder from the drawer (with a basename fallback).
                  ;; The assertion the spec actually requires is
                  ;; "activation fired on the agent's session.org," so
                  ;; pin observable identity.  After the work-root change
                  ;; (design.md D5) the agent buffer's `default-directory'
                  ;; comes from its `:GPTEL_WORK_ROOT:' drawer key — here a
                  ;; `work_root'-omitting agent froze the parent's work
                  ;; root (set to `mock-branch-dir' by the parent-buffer
                  ;; fixture).  The binder normalizes it via
                  ;; `file-name-as-directory'+`expand-file-name'.
                  (let ((agent-sid (buffer-local-value 'jf/gptel--session-id
                                                       agent-buffer))
                        (agent-dir (buffer-local-value 'default-directory
                                                       agent-buffer)))
                    (expect agent-sid :to-be-truthy)
                    (expect (stringp agent-sid) :to-be-truthy)
                    (expect (file-truename agent-dir)
                            :to-equal
                            (file-name-as-directory
                             (file-truename mock-branch-dir))))
                  ;; Branch-name buffer-local is set by the identity binder.
                  (expect (buffer-local-value 'jf/gptel--branch-name agent-buffer)
                          :to-be-truthy)
                  ;; Autosave is enabled by the chat-mode activation path.
                  (expect (buffer-local-value 'jf/gptel-autosave-enabled
                                              agent-buffer)
                          :to-be-truthy))))
          (jf-pa-auto-init-test--cleanup-agent-buffer agent-buffer)))))

  (describe "agent buffer registers in jf/gptel--session-registry"

    ;; Scenario: specs/persistent-agent/spec.md (delta) §
    ;; "Agent session creation" → "Agent buffer activates via
    ;; content-addressed magic-mode recognition" (registry assertion
    ;; piece).  The exact lookup function is `jf/gptel-session-find',
    ;; verified via:
    ;;   grep -n 'defun jf/gptel--' config/gptel/sessions/registry.el
    ;; (`jf/gptel--register-session', `jf/gptel-session-find').
    (it "the agent buffer registers in jf/gptel--session-registry"
      (let ((preset 'pa-registry-preset)
            (agent-buffer nil)
            (captured nil))
        (unwind-protect
            (jf/persistent-agent-test--with-mock-parent-session
              (jf/persistent-agent-test--with-mock-preset preset
                (jf-pa-auto-init-test--with-parent-buffer
                  (jf/persistent-agent-test--with-mock-gptel-request captured
                    (jf/gptel-persistent-agent--task
                     #'ignore (symbol-name preset) "registry test" "do work"))
                  (setq agent-buffer
                        (jf-pa-auto-init-test--find-agent-buffer mock-branch-dir))
                  (expect agent-buffer :to-be-truthy)
                  (let* ((agent-sid (buffer-local-value 'jf/gptel--session-id
                                                        agent-buffer))
                         (agent-bname (buffer-local-value 'jf/gptel--branch-name
                                                          agent-buffer))
                         (entry (jf/gptel-session-find agent-sid agent-bname)))
                    (expect entry :to-be-truthy)
                    (expect (plist-get entry :session-id) :to-equal agent-sid)
                    (expect (plist-get entry :branch-name) :to-equal agent-bname)
                    (expect (plist-get entry :buffer) :to-equal agent-buffer)))))
          (jf-pa-auto-init-test--cleanup-agent-buffer agent-buffer)))))

  (describe "saved session.org reloads as an interactive chat session"

    ;; Scenario: specs/persistent-agent/spec.md (delta) §
    ;; "Persistence and resumption" → "Saved agent session reloads as
    ;; interactive"
    (it "the saved session.org reloads as an interactive chat session"
      (let ((preset 'pa-reload-preset)
            (agent-buffer nil)
            (captured nil)
            (preset-backend nil)
            (preset-model nil)
            (session-file nil)
            (agent-sid nil)
            (reloaded-buffer nil))
        ;; Capture the backend/model at preset-registration time so we
        ;; can assert the reloaded buffer matches what the preset
        ;; declared (not whatever happens to be the global default at
        ;; reload time).  The fixture macro registers the preset with
        ;; `:backend gptel-backend :model gptel-model'.
        (setq preset-backend gptel-backend
              preset-model gptel-model)
        (unwind-protect
            (jf/persistent-agent-test--with-mock-parent-session
              (jf/persistent-agent-test--with-mock-preset preset
                (jf-pa-auto-init-test--with-parent-buffer
                  (jf/persistent-agent-test--with-mock-gptel-request captured
                    (jf/gptel-persistent-agent--task
                     #'ignore (symbol-name preset) "reload test" "first turn"))
                  (setq agent-buffer
                        (jf-pa-auto-init-test--find-agent-buffer mock-branch-dir))
                  (expect agent-buffer :to-be-truthy)
                  (setq session-file (buffer-file-name agent-buffer)
                        agent-sid (buffer-local-value 'jf/gptel--session-id
                                                      agent-buffer))
                  ;; Kill the agent buffer (and clear its registry slot)
                  ;; to simulate a session that was created earlier and
                  ;; later closed.  The file on disk is the
                  ;; `create-session-core' initial content.
                  (jf-pa-auto-init-test--cleanup-agent-buffer agent-buffer)
                  (setq agent-buffer nil)
                  ;; Reopen via real find-file-noselect — content-
                  ;; addressed activation fires: `magic-mode-alist'
                  ;; recognises the drawer and selects chat-mode, the
                  ;; identity binder runs, drawer preset is applied.  We
                  ;; must reopen from a context where the parent's
                  ;; session-id is NOT dynamically visible, otherwise the
                  ;; identity binder's guard bails.  The
                  ;; with-parent-buffer macro keeps the dynamic shadow
                  ;; (let nil), so reopening from the surrounding scope
                  ;; (outside `pa-parent-buffer') sees nil for
                  ;; `jf/gptel--session-id' and auto-init proceeds.
                  (with-temp-buffer
                    (setq reloaded-buffer (find-file-noselect session-file)))
                  (with-current-buffer reloaded-buffer
                    ;; Same major mode as a fresh interactive session.
                    (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
                    ;; Backend / model came from the preset's declared
                    ;; values (drawer-driven preset application fired on
                    ;; reload via `gptel-chat--apply-declared-preset').
                    (expect gptel-backend :to-equal preset-backend)
                    (expect gptel-model :to-equal preset-model)
                    ;; Branch / session ids restored on reload (drawer
                    ;; identity, with basename fallback).
                    (expect jf/gptel--branch-name :to-equal "main")
                    (expect jf/gptel--session-id :to-equal agent-sid)
                    ;; Parent-session-id restored from the drawer
                    ;; (`:GPTEL_PARENT_SESSION_ID:').
                    (expect (bound-and-true-p jf/gptel--parent-session-id)
                            :to-equal mock-session-id)))))
          (jf-pa-auto-init-test--cleanup-agent-buffer reloaded-buffer)
          (jf-pa-auto-init-test--cleanup-agent-buffer agent-buffer)))))

  (describe "no agent-specific auto-save hook is installed"

    ;; Scenario: specs/persistent-agent/spec.md (delta) §
    ;; "Persistence and resumption" → "No agent-specific auto-save hook"
    ;; This is a structural-correctness check: the rebuild explicitly
    ;; delegates autosave to the chat-mode activation path.  A
    ;; regression that re-introduces `jf/gptel--auto-save-session-buffer'
    ;; (or any agent-defined save function) on
    ;; `gptel-post-response-functions' is caught here.
    (it "no agent-specific auto-save hook is installed"
      ;; Default value: persistent-agent module is loaded above
      ;; (`(require 'gptel-persistent-agent)'), so any module-level
      ;; `add-hook' would already have fired by now.
      (expect (memq 'jf/gptel--auto-save-session-buffer
                    (default-value 'gptel-post-response-functions))
              :to-be nil)
      ;; Buffer-local: a freshly-created agent buffer also must NOT
      ;; carry the legacy hook entry.  Use the same fixtures-driven
      ;; flow as the other specs.
      (let ((preset 'pa-no-hook-preset)
            (agent-buffer nil)
            (captured nil))
        (unwind-protect
            (jf/persistent-agent-test--with-mock-parent-session
              (jf/persistent-agent-test--with-mock-preset preset
                (jf-pa-auto-init-test--with-parent-buffer
                  (jf/persistent-agent-test--with-mock-gptel-request captured
                    (jf/gptel-persistent-agent--task
                     #'ignore (symbol-name preset) "no-hook test" "do work"))
                  (setq agent-buffer
                        (jf-pa-auto-init-test--find-agent-buffer mock-branch-dir))
                  (expect agent-buffer :to-be-truthy)
                  (with-current-buffer agent-buffer
                    (expect (memq 'jf/gptel--auto-save-session-buffer
                                  gptel-post-response-functions)
                            :to-be nil)))))
          (jf-pa-auto-init-test--cleanup-agent-buffer agent-buffer))))))

(provide 'agent-buffer-activation-reload-spec)
;;; agent-buffer-activation-reload-spec.el ends here
