;;; identity-keys-emission-spec.el --- Identity drawer-key emission -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural specs for the identity-bearing drawer keys emitted by
;; the three session writers (change
;; gptel-content-addressed-session-activation, task
;; emit-identity-keys-in-writers).
;;
;; The OPEN identity-key set (register/vocabulary/identity-drawer-keys)
;; the writers MUST emit:
;;
;;   :GPTEL_SESSION_ID:        authoritative session id (agents carry
;;                             their OWN id, not the parent's)
;;   :GPTEL_BRANCH:            bare branch name (new sessions/agents
;;                             write `main')
;;   :GPTEL_PARENT_SESSION_ID: agents only — the link to the spawning
;;                             session (already emitted today)
;;
;; Invariants verified:
;;
;; 1. A freshly-created (non-agent) session's drawer carries
;;    `:GPTEL_SESSION_ID: <id>' and `:GPTEL_BRANCH: main'
;;    (register/boundary/drawer-first-identity-resolution: these are
;;    exactly what the drawer-first resolvers read).
;;
;; 2. A freshly-created agent's drawer carries its OWN
;;    `:GPTEL_SESSION_ID:' (the agent directory's basename, NOT the
;;    parent's id), `:GPTEL_BRANCH: main', AND
;;    `:GPTEL_PARENT_SESSION_ID: <parent>' — the uniform identity rule
;;    of design.md D3 (own id + parent link; TYPE inferable from the
;;    parent key's presence).
;;
;; 3. A branch created from a parent SHARES the parent's
;;    `:GPTEL_SESSION_ID:' but carries its OWN new `:GPTEL_BRANCH:'
;;    (register/invariant/branch-drawer-shares-id-not-branch): the
;;    parent's branch value MUST NOT leak, and there must be NO
;;    duplicate `:GPTEL_BRANCH:' key (a verbatim copy + append would
;;    produce one; the writer REPLACES instead).
;;
;; The drawer remains a clean `register/shape/drawer-text-block' in
;; every case: exactly one `:PROPERTIES:' / `:END:' pair, the trailing
;; newline preserved, one key-line per identity property.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Shared `with-captured-io' helper (intercepts the filesystem write
;; primitives so the full production path runs for real).
(add-to-list 'load-path
             (expand-file-name
              "../../../test"
              (file-name-directory (or load-file-name buffer-file-name))))
(require 'persistence-test-helpers)

;; Persistent-agent test fixtures (`with-mock-parent-session',
;; `with-mock-preset', `with-mock-gptel-request') live with the
;; persistent-agent specs; the agent identity case reuses them.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (pa-test-dir (expand-file-name
                     "../../../tools/test/persistent-agent" spec-dir)))
  (add-to-list 'load-path pa-test-dir)
  (load (expand-file-name "helpers-spec.el" pa-test-dir) nil t))

;; Production modules.
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)
(require 'gptel-session-branching)

;; The persistent-agent module is loaded from its own .el so the agent
;; case can call `jf/gptel-persistent-agent--task'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tools-dir (expand-file-name "../../../tools" spec-dir)))
  (add-to-list 'load-path tools-dir)
  (require 'gptel-persistent-agent
           (expand-file-name "persistent-agent.el" tools-dir)))

;;; Helpers

(defun jf/identity-keys-test--count-matches (regexp string)
  "Return how many times REGEXP matches in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun jf/identity-keys-test--file-contents (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;;; Case 1 — fresh (non-agent) session

(describe "jf/gptel--create-session-core emits identity drawer keys"

  (it "writes :GPTEL_SESSION_ID: and :GPTEL_BRANCH: main into the drawer"
    (with-captured-io
      (jf/gptel--create-session-core
       "sess-identity-20260421120000"
       "/sessions/sess-identity-20260421120000"
       'executor)
      (let ((content (captured-file-content
                      captured-files
                      (concat "/sessions/sess-identity-20260421120000"
                              "/branches/main/session.org"))))
        (expect content :to-be-truthy)
        ;; Identity keys present with the canonical values.
        (expect content :to-match
                ":GPTEL_SESSION_ID: sess-identity-20260421120000\n")
        (expect content :to-match ":GPTEL_BRANCH: main\n")
        ;; Non-agent session: no parent link.
        (expect (string-match-p ":GPTEL_PARENT_SESSION_ID:" content) :to-be nil)
        ;; Drawer-text-block stays clean: one drawer, keys inside it.
        (expect (jf/identity-keys-test--count-matches ":PROPERTIES:" content)
                :to-equal 1)
        (expect (jf/identity-keys-test--count-matches "^:END:$" content)
                :to-equal 1)
        (expect (jf/identity-keys-test--count-matches ":GPTEL_SESSION_ID:" content)
                :to-equal 1)
        (expect (jf/identity-keys-test--count-matches ":GPTEL_BRANCH:" content)
                :to-equal 1)
        ;; Identity key-lines fall INSIDE the :PROPERTIES: / :END: block.
        (let ((props-pos (string-match ":PROPERTIES:" content))
              (end-pos   (string-match "^:END:$" content))
              (sid-pos   (string-match ":GPTEL_SESSION_ID:" content))
              (branch-pos (string-match ":GPTEL_BRANCH:" content)))
          (expect (< props-pos sid-pos end-pos) :to-be-truthy)
          (expect (< props-pos branch-pos end-pos) :to-be-truthy))))))

;;; Case 2 — fresh agent

(describe "jf/gptel-persistent-agent--task emits the agent's own identity keys"

  (it "writes the agent's OWN :GPTEL_SESSION_ID:, :GPTEL_BRANCH: main, and parent link"
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
                (content (jf/identity-keys-test--file-contents session-org)))
           ;; The agent's OWN id is the agent directory's basename
           ;; (what the drawer-first resolvers fall back to when
           ;; reading GPTEL_SESSION_ID).
           (with-temp-buffer
             (insert-file-contents session-org)
             (org-mode)
             (expect (org-entry-get (point-min) "GPTEL_SESSION_ID")
                     :to-equal agent-name)
             (expect (org-entry-get (point-min) "GPTEL_BRANCH")
                     :to-equal "main")
             ;; Parent link still present and pointing at the spawning
             ;; session — NOT the agent's own id.
             (expect (org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")
                     :to-equal mock-session-id))
           ;; The agent's own id is distinct from the parent's id.
           (expect agent-name :not :to-equal mock-session-id)
           ;; Clean drawer-text-block: exactly one of each identity key.
           (expect (jf/identity-keys-test--count-matches
                    ":GPTEL_SESSION_ID:" content)
                   :to-equal 1)
           (expect (jf/identity-keys-test--count-matches
                    ":GPTEL_BRANCH:" content)
                   :to-equal 1)
           (expect (jf/identity-keys-test--count-matches
                    ":GPTEL_PARENT_SESSION_ID:" content)
                   :to-equal 1)))))))

;;; Case 3 — branch created from a parent

(describe "jf/gptel--create-branch-session rewrites identity keys on the new branch"

  (defvar jf/identity-keys-test--tempdirs nil
    "Temp directories created during branch identity tests.")

  (after-each
    (dolist (dir jf/identity-keys-test--tempdirs)
      (when (and dir (file-directory-p dir))
        (delete-directory dir t)))
    (setq jf/identity-keys-test--tempdirs nil))

  (it "shares the parent's :GPTEL_SESSION_ID: but takes its OWN :GPTEL_BRANCH:"
    (let* ((root (let ((d (make-temp-file "gptel-identity-branch-" t)))
                   (push d jf/identity-keys-test--tempdirs)
                   d))
           (jf/gptel-sessions-directory root)
           (session-id "integration-20260421000000")
           (session-dir (jf/gptel--create-session-directory session-id))
           (main-branch-dir (jf/gptel--create-branch-directory session-dir "main"))
           (main-ctx (jf/gptel--context-file-path main-branch-dir))
           ;; Parent drawer carries the shared session id and the
           ;; parent branch value (`main') exactly as
           ;; `create-session-core' would have written them.
           (parent-content
            (concat ":PROPERTIES:\n"
                    ":GPTEL_PRESET: executor\n"
                    ":GPTEL_SESSION_ID: " session-id "\n"
                    ":GPTEL_BRANCH: main\n"
                    ":END:\n"
                    "#+begin_user\n"
                    "First question?\n"
                    "#+end_user\n"
                    "\n"
                    "#+begin_assistant\n"
                    "First answer.\n"
                    "#+end_assistant\n")))
      (with-temp-file main-ctx (insert parent-content))
      ;; Branch at end-of-file (copy full parent content).
      (let* ((branch-pos
              (with-temp-buffer
                (insert-file-contents main-ctx)
                (point-max)))
             (new-branch-dir
              (jf/gptel--create-branch-session
               session-dir "main" "exp" branch-pos))
             (new-branch-name (file-name-nondirectory new-branch-dir))
             (new-ctx (jf/gptel--context-file-path new-branch-dir))
             (content (jf/identity-keys-test--file-contents new-ctx)))
        ;; SAME session id as the parent.
        (expect content :to-match
                (concat ":GPTEL_SESSION_ID: " (regexp-quote session-id) "\n"))
        ;; OWN new branch name — NOT the parent's `main'.
        (expect content :to-match
                (concat ":GPTEL_BRANCH: " (regexp-quote new-branch-name) "\n"))
        (expect new-branch-name :not :to-equal "main")
        ;; Parent's branch value did NOT leak through.
        (expect (string-match-p ":GPTEL_BRANCH: main\n" content) :to-be nil)
        ;; No duplicate key survives the copy-then-overwrite.
        (expect (jf/identity-keys-test--count-matches ":GPTEL_BRANCH:" content)
                :to-equal 1)
        (expect (jf/identity-keys-test--count-matches ":GPTEL_SESSION_ID:" content)
                :to-equal 1)
        ;; Drawer-text-block remains a single clean block.
        (expect (jf/identity-keys-test--count-matches ":PROPERTIES:" content)
                :to-equal 1)
        (expect (jf/identity-keys-test--count-matches "^:END:$" content)
                :to-equal 1)
        ;; The verbatim-copied body survives untouched below the drawer.
        (expect content :to-match "#\\+begin_user\n")))))

(provide 'identity-keys-emission-spec)
;;; identity-keys-emission-spec.el ends here
