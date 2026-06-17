;;; work-root-emission-spec.el --- GPTEL_WORK_ROOT drawer-key emission -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural specs for the `:GPTEL_WORK_ROOT:' drawer key emitted by
;; `jf/gptel--create-session-core' (change
;; gptel-work-root-default-directory, task session-write-work-root).
;;
;; `:GPTEL_WORK_ROOT:' is a SECOND, verbatim persistence of the SAME
;; PROJECT-ROOT input that `jf/gptel-scope-profile--create-for-session'
;; expands into the `:GPTEL_SCOPE_*:' keys.  Because both outputs derive
;; from one input, the session's working directory and its scope
;; boundary cannot disagree — the cwd↔scope agreement invariant
;; (register/invariant/cwd-scope-agreement) is STRUCTURAL, not enforced
;; (design.md D1).  The value is stored absolute to match how
;; `${project_root}' expands (design.md D3).
;;
;; Invariants verified:
;;
;; 1. WHEN a session is created with a non-nil PROJECT-ROOT THEN the
;;    drawer carries `:GPTEL_WORK_ROOT: <abs>' (the absolute root),
;;    inside the `:PROPERTIES:' / `:END:' block, exactly once.
;;
;; 2. Agreement-by-construction: the persisted `:GPTEL_WORK_ROOT:' value
;;    equals `(expand-file-name PROJECT-ROOT)' for the SAME PROJECT-ROOT
;;    argument handed to `jf/gptel-scope-profile--create-for-session'
;;    (the scope-expansion root).  A relative-form PROJECT-ROOT still
;;    lands as the same absolute string both writers see.
;;
;; 3. WHEN PROJECT-ROOT is nil THEN NO `:GPTEL_WORK_ROOT:' line is
;;    emitted (keyless session — the binder falls back to branch-dir).

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

;; Production modules.
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)

;;; Helpers

(defun jf/work-root-test--count-matches (regexp string)
  "Return how many times REGEXP matches in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun jf/work-root-test--session-content (captured-files session-dir)
  "Return the captured `session.org' content under SESSION-DIR."
  (captured-file-content
   captured-files
   (concat session-dir "/branches/main/session.org")))

;;; Case 1 — non-nil PROJECT-ROOT writes the absolute key

(describe "jf/gptel--create-session-core emits :GPTEL_WORK_ROOT:"

  (it "writes the absolute PROJECT-ROOT into the drawer"
    (with-captured-io
      (jf/gptel--create-session-core
       "sess-workroot-20260617120000"
       "/sessions/sess-workroot-20260617120000"
       'executor nil nil "/Users/x/proj")
      (let ((content (jf/work-root-test--session-content
                      captured-files
                      "/sessions/sess-workroot-20260617120000")))
        (expect content :to-be-truthy)
        ;; Absolute work-root value present, exactly once.
        (expect content :to-match ":GPTEL_WORK_ROOT: /Users/x/proj\n")
        (expect (jf/work-root-test--count-matches ":GPTEL_WORK_ROOT:" content)
                :to-equal 1)
        ;; Key falls INSIDE the :PROPERTIES: / :END: block.
        (let ((props-pos (string-match ":PROPERTIES:" content))
              (end-pos   (string-match "^:END:$" content))
              (wr-pos    (string-match ":GPTEL_WORK_ROOT:" content)))
          (expect (and props-pos wr-pos end-pos
                       (< props-pos wr-pos end-pos))
                  :to-be-truthy)))))

  ;;; Case 2 — agreement-by-construction with the scope-expansion root

  (it "writes the SAME root the scope expansion received (agreement-by-construction)"
    ;; Capture the PROJECT-ROOT argument the scope-profile renderer sees
    ;; so we can prove `:GPTEL_WORK_ROOT:' is `(expand-file-name <that>)'.
    ;; A relative-form PROJECT-ROOT exercises the normalisation: both
    ;; writers MUST agree on the same absolute string.
    (let ((scope-root :unset)
          ;; default-directory makes the relative form resolvable to a
          ;; deterministic absolute path for the assertion.
          (default-directory "/Users/x/"))
      (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
                 (lambda (_preset project-root &rest _args)
                   (setq scope-root project-root)
                   ;; Minimal valid drawer-text-block carrying a scope key
                   ;; expanded from the SAME root, so the test exercises a
                   ;; realistic drawer shape.
                   (concat ":PROPERTIES:\n"
                           ":GPTEL_PRESET: executor\n"
                           ":GPTEL_SCOPE_FILESYSTEM_WRITE: "
                           (expand-file-name project-root) "\n"
                           ":END:\n"))))
        (with-captured-io
          (jf/gptel--create-session-core
           "sess-workroot-rel-20260617120000"
           "/sessions/sess-workroot-rel-20260617120000"
           'executor nil nil "proj")
          (let ((content (jf/work-root-test--session-content
                          captured-files
                          "/sessions/sess-workroot-rel-20260617120000"))
                (expected-abs (expand-file-name "proj")))
            ;; The renderer actually received the project-root.
            (expect scope-root :to-equal "proj")
            ;; Work-root value == absolute form of the SAME input the
            ;; scope expansion used.
            (expect content :to-match
                    (concat ":GPTEL_WORK_ROOT: " (regexp-quote expected-abs) "\n"))
            ;; And it agrees verbatim with the scope-expansion root the
            ;; mocked renderer wrote into the scope key.
            (expect content :to-match
                    (concat ":GPTEL_SCOPE_FILESYSTEM_WRITE: "
                            (regexp-quote expected-abs) "\n")))))))

  ;;; Case 3 — nil PROJECT-ROOT emits NO key

  (it "omits :GPTEL_WORK_ROOT: entirely for a keyless (nil PROJECT-ROOT) session"
    (with-captured-io
      (jf/gptel--create-session-core
       "sess-workroot-keyless-20260617120000"
       "/sessions/sess-workroot-keyless-20260617120000"
       'executor)
      (let ((content (jf/work-root-test--session-content
                      captured-files
                      "/sessions/sess-workroot-keyless-20260617120000")))
        (expect content :to-be-truthy)
        (expect (string-match-p ":GPTEL_WORK_ROOT:" content) :to-be nil)
        ;; Drawer remains a clean single block.
        (expect (jf/work-root-test--count-matches ":PROPERTIES:" content)
                :to-equal 1)
        (expect (jf/work-root-test--count-matches "^:END:$" content)
                :to-equal 1)))))

(provide 'work-root-emission-spec)
;;; work-root-emission-spec.el ends here
