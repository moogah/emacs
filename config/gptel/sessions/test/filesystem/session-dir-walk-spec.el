;;; session-dir-walk-spec.el --- Buttercup tests for ancestor-marker session-dir walk -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verify `jf/gptel--session-dir-from-branch-dir'
;; (register/boundary/session-dir-marker-walk): derive a session's
;; session-dir by walking up from the branch-dir to the nearest ANCESTOR
;; directory whose child is `branches/'.
;;
;; - branch type: walk up to the nearest ancestor D such that
;;   (file-directory-p D/branches) AND branch-dir is under D/branches/.
;;   The walk is DEPTH-INDEPENDENT — no fixed `../..' count — proven with
;;   a deeper/relocated layout (an extra wrapping directory) resolving to
;;   the same structural marker.
;; - agent type: branch-dir is itself the session root (agents do not
;;   branch); the function returns branch-dir unchanged.
;; - corrupt/standalone (no branches/ ancestor): return branch-dir.
;;
;; Tests build a REAL temp dir tree rather than mocking `file-directory-p',
;; because the contract under test is structural ancestor-walking over a
;; real on-disk layout. Paths are compared via `file-equal-p' /
;; `file-name-as-directory' to stay robust against trailing-slash and
;; truename differences (e.g. macOS /var -> /private/var).

;;; Code:

(require 'buttercup)
(require 'gptel-session-filesystem)

(defun jf/gptel-test--make-tree ()
  "Create a fresh temp session-tree root and return its absolute path.
Caller is responsible for cleanup via `delete-directory'."
  (file-name-as-directory
   (make-temp-file "gptel-session-dir-walk-" t)))

(describe "jf/gptel--session-dir-from-branch-dir"

  (describe "branch layout"

    (let (root)

      (before-each
        (setq root (jf/gptel-test--make-tree))
        ;; <root>/branches/main/
        (make-directory (expand-file-name "branches/main" root) t))

      (after-each
        (delete-directory root t))

      (it "resolves a branch buffer's dir to the root whose child is branches/"
        ;; branch-dir = <root>/branches/main ; session-dir = <root>
        (let ((branch-dir (expand-file-name "branches/main" root)))
          (expect (file-equal-p
                   (jf/gptel--session-dir-from-branch-dir branch-dir 'branch)
                   root)
                  :to-be t)))))

  (describe "deeper / relocated layout (depth-independence)"

    (let (wrapper root)

      (before-each
        ;; An EXTRA wrapping directory above the session root proves the
        ;; walk is not a fixed `../..' count: the marker (branches/) still
        ;; determines session-dir regardless of how deep the tree sits.
        (setq wrapper (jf/gptel-test--make-tree))
        (setq root (file-name-as-directory
                    (expand-file-name "wrap-a/wrap-b/session-root" wrapper)))
        (make-directory (expand-file-name "branches/main" root) t))

      (after-each
        (delete-directory wrapper t))

      (it "still resolves to the dir whose child is branches/ (no fixed ../..)"
        (let ((branch-dir (expand-file-name "branches/main" root)))
          (expect (file-equal-p
                   (jf/gptel--session-dir-from-branch-dir branch-dir 'branch)
                   root)
                  :to-be t)))

      (it "does NOT stop at an ancestor that merely has a deeper branches/ descendant"
        ;; The wrapper itself has NO branches/ child; only `root' does.
        ;; A naive parent-loop that matched any ancestor with a branches/
        ;; *descendant* would over-walk. The nearest marker is `root'.
        (let ((branch-dir (expand-file-name "branches/main" root)))
          (expect (file-equal-p
                   (jf/gptel--session-dir-from-branch-dir branch-dir 'branch)
                   wrapper)
                  :to-be nil)))))

  (describe "agent layout"

    (let (root agent-dir)

      (before-each
        (setq root (jf/gptel-test--make-tree))
        ;; <root>/branches/main/agents/<agent>/
        (setq agent-dir (expand-file-name
                         "branches/main/agents/researcher-20260120153042" root))
        (make-directory agent-dir t))

      (after-each
        (delete-directory root t))

      (it "returns the agent's OWN directory (agents do not branch)"
        (expect (file-equal-p
                 (jf/gptel--session-dir-from-branch-dir agent-dir 'agent)
                 agent-dir)
                :to-be t))

      (it "does NOT walk an agent up to the enclosing branch root"
        (expect (file-equal-p
                 (jf/gptel--session-dir-from-branch-dir agent-dir 'agent)
                 root)
                :to-be nil))))

  (describe "corrupt / standalone layout (no branches/ ancestor)"

    (let (lonely)

      (before-each
        ;; A directory with NO branches/ marker anywhere above it. Guard the
        ;; inherited precondition: locate-dominating-file walks all the way to
        ;; root, so a stray branches/ ancestor above TMPDIR would silently void
        ;; this test. Assert its absence so a pathological environment fails
        ;; loudly at setup rather than as a confusing assertion mismatch.
        (setq lonely (file-name-as-directory
                      (make-temp-file "gptel-session-dir-lonely-" t)))
        (expect (locate-dominating-file lonely "branches") :to-be nil))

      (after-each
        (delete-directory lonely t))

      (it "returns branch-dir unchanged when no branches/ ancestor exists"
        (expect (file-equal-p
                 (jf/gptel--session-dir-from-branch-dir lonely 'branch)
                 lonely)
                :to-be t)))))

(provide 'session-dir-walk-spec)
;;; session-dir-walk-spec.el ends here
