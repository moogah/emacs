;;; work-root-emission-spec.el --- GPTEL_WORK_ROOT drawer-key emission -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural specs for the `:GPTEL_WORK_ROOT:' drawer key emitted by
;; `jf/gptel--create-session-core' (change
;; gptel-work-root-default-directory, task session-write-work-root).
;;
;; `:GPTEL_WORK_ROOT:' is a SECOND persistence of the SAME PROJECT-ROOT
;; input that `jf/gptel-scope-profile--create-for-session' expands into
;; the `:GPTEL_SCOPE_*:' keys.  `jf/gptel--create-session-core'
;; canonicalizes that input ONCE at entry with `expand-file-name', and
;; fans the single canonical-absolute string out to BOTH consumers.
;; Because both outputs derive from one canonical string, the session's
;; working directory and its scope boundary cannot disagree AND are both
;; absolute — the cwd↔scope agreement invariant
;; (register/invariant/cwd-scope-agreement) is STRUCTURAL, not enforced,
;; and now ABSOLUTE-and-agreeing for ANY input shape (design.md D1/D3).
;; The renderer (`jf/gptel-scope-profile--expand-string') substitutes
;; `${project_root}' VERBATIM (raw `replace-regexp-in-string', NOT
;; `expand-file-name'); the canonical-absolute form is therefore
;; established at the SOURCE, not by the renderer (design.md D3).
;;
;; Invariants verified:
;;
;; 1. WHEN a session is created with a non-nil PROJECT-ROOT THEN the
;;    drawer carries `:GPTEL_WORK_ROOT: <abs>' (the absolute root),
;;    inside the `:PROPERTIES:' / `:END:' block, exactly once.
;;
;; 2. Absolute-agreement-by-construction: given a NON-canonical input
;;    (relative / trailing-slash), the persisted `:GPTEL_WORK_ROOT:'
;;    value AND the root the scope renderer substitutes are BOTH
;;    `(expand-file-name INPUT)' — byte-identical to each other.  The
;;    renderer mock models the REAL verbatim substitution (it does NOT
;;    re-apply `expand-file-name'), so a wrong impl that left either
;;    output non-canonical, or fed the two consumers different strings,
;;    would visibly diverge and fail.
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

  ;;; Case 2 — ABSOLUTE-agreement-by-construction with the scope-expansion root

  (it "canonicalizes a NON-canonical PROJECT-ROOT at source and feeds the SAME absolute string to BOTH the work-root and the scope expansion"
    ;; NON-TAUTOLOGY GUARD: the renderer mock FAITHFULLY models the real
    ;; substitution — `jf/gptel-scope-profile--expand-string' does a raw
    ;; `(replace-regexp-in-string "${project_root}" project-root str t t)'
    ;; (scope-profiles.org §expand-string), inserting whatever string it
    ;; RECEIVES VERBATIM, with NO `expand-file-name' of its own.  So the
    ;; ONLY way the scope key can end up absolute is if the production
    ;; code canonicalized the input BEFORE handing it to the renderer.
    ;;
    ;; We pass NON-canonical inputs (relative "proj" / trailing-slash) and
    ;; assert BOTH the `:GPTEL_WORK_ROOT:' value AND the renderer's
    ;; substitution are the ABSOLUTE `(expand-file-name INPUT)', and that
    ;; the two are byte-identical.  A wrong impl FAILS:
    ;;   - verbatim WORK_ROOT (no source canonicalization) → WORK_ROOT
    ;;     stays "proj", absolute :to-match fails;
    ;;   - canonicalize only on the WORK_ROOT side → renderer receives
    ;;     "proj", `scope-root :to-equal expected-abs' fails AND the scope
    ;;     key is relative;
    ;;   - the two consumers fed different strings → divergence fails.
    (dolist (input '("proj" "/Users/x/proj/"))
      (let* ((expected-abs (expand-file-name input))
             (scope-root :unset)
             (session-dir (concat "/sessions/sess-workroot-canon-"
                                  (md5 input))))
        (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
                   (lambda (_preset project-root &rest _args)
                     (setq scope-root project-root)
                     ;; Substitute PROJECT-ROOT VERBATIM, exactly as the
                     ;; real --expand-string does (raw replace, no
                     ;; expand-file-name).  Whatever string we receive is
                     ;; what lands in the scope key.
                     (concat ":PROPERTIES:\n"
                             ":GPTEL_PRESET: executor\n"
                             ":GPTEL_SCOPE_FILESYSTEM_WRITE: "
                             project-root "/**\n"
                             ":END:\n"))))
          (with-captured-io
            (jf/gptel--create-session-core
             "sess-workroot-canon" session-dir
             'executor nil nil input)
            (let ((content (jf/work-root-test--session-content
                            captured-files session-dir)))
              ;; (a) The renderer received the CANONICAL-ABSOLUTE root —
              ;; canonicalization happened at source, before fan-out.
              (expect scope-root :to-equal expected-abs)
              ;; (b) `:GPTEL_WORK_ROOT:' carries that SAME absolute string.
              (expect content :to-match
                      (concat ":GPTEL_WORK_ROOT: "
                              (regexp-quote expected-abs) "\n"))
              ;; (c) The scope key carries the byte-identical absolute root
              ;; — both consumers saw the one canonical string, so they
              ;; cannot disagree.
              (expect content :to-match
                      (concat ":GPTEL_SCOPE_FILESYSTEM_WRITE: "
                              (regexp-quote expected-abs) "/\\*\\*\n"))))))))

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
