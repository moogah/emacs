;;; branching-integration-spec.el --- Integration tests for session branching -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; End-to-end behavioural tests for `jf/gptel--create-branch-session',
;; mounted on a real temporary session directory. Exercises:
;;   - Parser-driven branch-point selection composed with truncation
;;     and branch-directory creation (registry, metadata, symlink).
;;   - Empty branch from first-turn EXCLUDE still produces a valid
;;     `session.org' in a real branch directory.
;;   - Org commentary before the first user turn is preserved verbatim
;;     in the new branch.
;;
;; Coverage (from
;; openspec/changes/gptel-chat-mode/specs/gptel/sessions-branching.md):
;;   Include → branch point is after #+end_user of the selected turn.
;;   Exclude → branch point is before #+begin_user of the selected turn.
;;   Empty branch from first-turn exclude is valid chat-mode.
;;   Branch preserves org commentary verbatim.
;;   session.org (not session.md) is the authoritative branch file name.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-chat-parser)
(require 'gptel-session-constants)
(require 'gptel-session-filesystem)
(require 'gptel-session-branching)

(defvar jf-branching-integration--tempdirs nil
  "List of temporary directories created during tests for cleanup.")

(defun jf-branching-integration--make-tempdir ()
  (let ((dir (make-temp-file "gptel-branching-integration-" t)))
    (push dir jf-branching-integration--tempdirs)
    dir))

(defun jf-branching-integration--file-contents (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;;; Sample content the branch point will be computed from.

(defconst jf-branching-integration--parent-session
  (concat "#+begin_user\n"
          "First question?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "First answer.\n"
          "#+end_assistant\n"
          "\n"
          "#+begin_user\n"
          "Second question?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Second answer.\n"
          "#+end_assistant\n"))

(defconst jf-branching-integration--parent-with-prose
  (concat "* Intro\n"
          "\n"
          "Context notes the user keeps above every turn.\n"
          "\n"
          "#+begin_user\n"
          "Hello?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Hi!\n"
          "#+end_assistant\n"))

(defun jf-branching-integration--bootstrap-parent (session-root parent-content)
  "Create a session directory with a `main' branch under SESSION-ROOT.
Writes PARENT-CONTENT to `main/session.org' and returns a plist:
  (:session-dir SESSION-DIR :branch-dir BRANCH-DIR :context-file PATH)."
  (let* ((jf/gptel-sessions-directory session-root)
         (session-dir (jf/gptel--create-session-directory
                       "integration-20260421000000"))
         (branch-dir (jf/gptel--create-branch-directory session-dir "main"))
         (ctx (jf/gptel--context-file-path branch-dir)))
    (with-temp-file ctx (insert parent-content))
    (list :session-dir session-dir
          :branch-dir branch-dir
          :context-file ctx)))

(defun jf-branching-integration--parse-file (path)
  "Parse PATH with `gptel-chat--parse-buffer' and return the turn list."
  (with-temp-buffer
    (insert-file-contents path)
    (gptel-chat--parse-buffer (current-buffer))))

(describe "Session branching integration (chat-mode turn list)"

  (after-each
    (dolist (dir jf-branching-integration--tempdirs)
      (when (and dir (file-directory-p dir))
        (delete-directory dir t)))
    (setq jf-branching-integration--tempdirs nil))

  (describe "INCLUDE branch point: new session.org ends after the selected #+end_user"

    (it "writes a session.org that contains exactly the selected turn and no more"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       ;; First user turn, INCLUDE.
                       (first (nth 0 turns)))
                  (jf/gptel--branching-turn-branch-point first t)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "exp-include" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx))
               (turns (jf-branching-integration--parse-file new-ctx)))
          ;; Output is parseable chat-mode: exactly one user turn.
          (expect (length turns) :to-equal 1)
          (expect (plist-get (car turns) :role) :to-equal 'user)
          (expect (string-trim (plist-get (car turns) :content))
                  :to-equal "First question?")
          ;; No assistant block carried through from the parent.
          (expect (string-match-p (regexp-quote "#+begin_assistant") written)
                  :not :to-be-truthy)
          (expect (string-match-p (regexp-quote "Second question?") written)
                  :not :to-be-truthy)
          ;; The selected `#+end_user' line is preserved verbatim.
          (expect (string-match-p (regexp-quote "#+end_user") written)
                  :to-be-truthy)))))

  (describe "EXCLUDE branch point: new session.org stops before the selected #+begin_user"

    (it "writes a session.org whose final content is strictly before the selected turn"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       ;; Second user turn, EXCLUDE.
                       (second (nth 1 turns)))
                  (jf/gptel--branching-turn-branch-point second nil)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "exp-exclude" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx))
               (turns (jf-branching-integration--parse-file new-ctx)))
          ;; Parseable chat-mode with the first user turn and its
          ;; assistant reply — but NOT the second user turn.
          (expect (mapcar (lambda (tn) (plist-get tn :role)) turns)
                  :to-equal '(user assistant))
          (expect (string-match-p (regexp-quote "Second question?") written)
                  :not :to-be-truthy)
          (expect (string-match-p (regexp-quote "First question?") written)
                  :to-be-truthy)))))

  (describe "First-turn EXCLUDE produces a valid empty chat-mode session"

    (it "writes a session.org with zero user turns and zero assistant turns"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       (first (nth 0 turns)))
                  (jf/gptel--branching-turn-branch-point first nil)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "empty-head" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx))
               (turns (jf-branching-integration--parse-file new-ctx)))
          ;; No turns at all — a valid empty chat-mode session.
          (expect turns :to-equal nil)
          (expect (string-match-p "#\\+begin_user" written)
                  :not :to-be-truthy)
          (expect (string-match-p "#\\+begin_assistant" written)
                  :not :to-be-truthy)))))

  (describe "Branch preserves org commentary verbatim"

    (it "retains headings and prose above the selected turn"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-with-prose))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       (first (nth 0 turns)))
                  ;; INCLUDE the lone user turn.
                  (jf/gptel--branching-turn-branch-point first t)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "exp-commentary" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx)))
          (expect (string-match-p (regexp-quote "* Intro\n") written)
                  :to-be-truthy)
          (expect (string-match-p
                   (regexp-quote "Context notes the user keeps above every turn.\n")
                   written)
                  :to-be-truthy)
          (expect (string-match-p (regexp-quote "#+begin_user\nHello?\n#+end_user\n")
                                  written)
                  :to-be-truthy)
          ;; The assistant reply is truncated.
          (expect (string-match-p (regexp-quote "#+begin_assistant") written)
                  :not :to-be-truthy)))))

  (describe "Branch directory conventions"

    (it "creates branches/<timestamp>-<name>/session.org (not session.md)"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       (first (nth 0 turns)))
                  (jf/gptel--branching-turn-branch-point first t)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "convention" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir)))
          (expect (file-exists-p new-ctx) :to-be-truthy)
          (expect (file-name-nondirectory new-ctx) :to-equal "session.org")
          ;; Legacy markdown filename must NOT appear in the new branch.
          (expect (file-exists-p (expand-file-name "session.md" new-branch-dir))
                  :not :to-be-truthy)
          ;; Branch-metadata.yml records parent and branch-point position.
          (let ((branch-metadata-path
                 (expand-file-name "branch-metadata.yml" new-branch-dir)))
            (expect (file-exists-p branch-metadata-path) :to-be-truthy))
          ;; Current symlink resolves to the new branch.
          (let ((current-link (expand-file-name "current" session-dir)))
            (expect (file-symlink-p current-link) :to-be-truthy)
            (expect (file-truename (expand-file-name "session.org" current-link))
                    :to-equal (file-truename new-ctx))))))))

(provide 'branching-integration-spec)
;;; branching-integration-spec.el ends here
