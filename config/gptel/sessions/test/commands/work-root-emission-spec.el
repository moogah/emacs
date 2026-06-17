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

  (it "writes the SAME verbatim root the scope expansion substitutes (agreement-by-construction)"
    ;; The renderer mock here FAITHFULLY models the real substitution:
    ;; `jf/gptel-scope-profile--expand-string' does a raw
    ;; `(replace-regexp-in-string "${project_root}" project-root str t t)'
    ;; (scope-profiles.org §expand-string) — it inserts PROJECT-ROOT
    ;; VERBATIM, NOT `(expand-file-name project-root)'.  We deliberately
    ;; pass a NON-canonical relative-form PROJECT-ROOT ("proj") so the
    ;; assertion would FAIL if the writer applied any transformation
    ;; (expand-file-name, trailing-slash munging) — proving the agreement
    ;; is structural for any input, not just already-absolute ones.
    (let ((scope-root :unset))
      (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
                 (lambda (_preset project-root &rest _args)
                   (setq scope-root project-root)
                   ;; Substitute PROJECT-ROOT VERBATIM, exactly as the real
                   ;; --expand-string does (raw replace, no expand-file-name).
                   (concat ":PROPERTIES:\n"
                           ":GPTEL_PRESET: executor\n"
                           ":GPTEL_SCOPE_FILESYSTEM_WRITE: "
                           project-root "/**\n"
                           ":END:\n"))))
        (with-captured-io
          (jf/gptel--create-session-core
           "sess-workroot-rel-20260617120000"
           "/sessions/sess-workroot-rel-20260617120000"
           'executor nil nil "proj")
          (let ((content (jf/work-root-test--session-content
                          captured-files
                          "/sessions/sess-workroot-rel-20260617120000")))
            ;; The renderer actually received the raw project-root.
            (expect scope-root :to-equal "proj")
            ;; Work-root value is the VERBATIM input — byte-identical, no
            ;; transformation.  (A regression to (expand-file-name project-root)
            ;; would write an absolute path here and fail this assertion.)
            (expect content :to-match ":GPTEL_WORK_ROOT: proj\n")
            ;; And it is byte-identical to the root the scope key carries:
            ;; both derive from the same string, so they cannot disagree.
            (expect content :to-match ":GPTEL_SCOPE_FILESYSTEM_WRITE: proj/\\*\\*\n"))))))

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
