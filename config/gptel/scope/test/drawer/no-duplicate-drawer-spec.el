;;; no-duplicate-drawer-spec.el --- Regression: drawer is never duplicated -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; REGRESSION SPEC: drawer writer integrity
;;
;; Pins the structural invariant `register/invariant/scope-drawer-no-duplication':
;; an add-to-scope sequence routed through `jf/gptel-scope--write-pattern-to-drawer'
;; produces exactly one `:PROPERTIES:' line and one `:END:' line in the
;; *pre-heading region* of the buffer (the span the file-level drawer occupies —
;; `(point-min)..(first ^* heading or point-max)' per
;; `no-duplicate-drawer-spec--file-level-drawer-region'), no matter how many
;; times the writer is invoked across distinct operations.  The pre-heading
;; scope is what excludes the canonical post-Addendum session.org's
;; heading-level `:PROPERTIES:' drawer on `* System Prompt' (per
;; `register/shape/session-document-layout') from the count while still
;; catching stacked file-level drawers at point-min (arch-cycle-1779522837-2).
;; The helper's docstring is the canonical span definition.
;;
;; This spec is anchored by the prior corruption incident
;; (`~/org/roam/20260419111957-gptel_preset_property_corruption.org') in which a
;; bug doubled the drawer and silently corrupted non-scope keys like
;; `:GPTEL_PRESET:'.  The four `it' blocks pin:
;;   1. Drawer singleton across read/write/execute writes.
;;   2. `:GPTEL_PRESET:' survives an add-to-scope write.
;;   3. Idempotent writes do not duplicate values.
;;   4. (cycle-3 add-on) Non-scope keys (`:GPTEL_PRESET:',
;;      `:GPTEL_PARENT_SESSION_ID:') survive the multi-write add-to-scope
;;      sequence verbatim AND the drawer remains a singleton.
;;
;; Cycle-3 finding 1 advisory: this spec consumes the existing helpers
;; (`jf/gptel-test--render-drawer', `jf/gptel-test--with-scope-drawer') from
;; `config/gptel/scope/test/helpers-spec.el' rather than introducing a
;; per-file fixture helper.  The drawer-count form uses inline `cl-count-if'
;; over split lines per the task design pattern.
;;
;; Scaffold disposition: the cycle-2 scaffold at
;; `openspec/changes/.../scaffolding/invariants/scope-drawer-no-duplication.test.el'
;; was dispositioned `archived' at cycle-2/cycle-3 integrate.  This spec
;; supersedes the scaffold; the scaffold file is deleted in the same commit.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'org)

;; Load helpers and the drawer writer.  Path layout:
;;   this file        config/gptel/scope/test/drawer/no-duplicate-drawer-spec.el
;;   scope-test-dir   config/gptel/scope/test/
;;   scope-dir        config/gptel/scope/
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

(defun no-duplicate-drawer-spec--file-level-drawer-region (buffer)
  "Return (BEG . END) of the pre-heading region in BUFFER.

The span starts at `(point-min)' and ends at the position
immediately BEFORE the first `^\\* ' heading line (or `(point-max)'
when no heading is present).  Always returns a region — never nil
— because the empty pre-heading region is still a valid scope for
counting.

Scoping the `:PROPERTIES:'/`:END:' counters to the pre-heading
region makes the test match the prose of
`register/invariant/scope-drawer-no-duplication', which restricts
the singleton claim to the file-level drawer at point-min, AND
preserves detection of the stacked-drawer corruption class
documented at `openspec/changes/gptel-org-mode-sessions/handoff-
property-drawer-corruption.md' — multiple consecutive `:PROPERTIES:'
drawers at point-min before any heading would still raise the
count above 1 under this span.  An earlier cycle-8 implementation
stopped the span at the first `:END:' line, which had the unwanted
side effect of hiding any second stacked drawer immediately after
the terminator (arch-cycle-1779522837-2)."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((heading-pos (and (re-search-forward "^\\* " nil t)
                              (line-beginning-position))))
        (cons (point-min) (or heading-pos (point-max)))))))

(defun no-duplicate-drawer-spec--count-properties-headers (buffer)
  "Count `:PROPERTIES:' header lines in the file-level drawer of BUFFER.

Splits the file-level drawer span (see
`no-duplicate-drawer-spec--file-level-drawer-region') on newlines and
counts lines matching a `:PROPERTIES:'-only line (allowing leading
whitespace).  The matching form is intentionally explicit about what
it asserts — \"exactly one drawer header line at point-min\" — rather
than relying on a regex search count that could collide with the
closing `:END:' or with property-key lines.

Scoped to the file-level drawer span per
`register/invariant/scope-drawer-no-duplication' prose; heading-level
drawers introduced by the post-cycle-7 canonical layout
\(`register/shape/session-document-layout') are excluded by construction."
  (let* ((region (no-duplicate-drawer-spec--file-level-drawer-region buffer))
         (content (if region
                      (with-current-buffer buffer
                        (buffer-substring-no-properties (car region) (cdr region)))
                    "")))
    (cl-count-if
     (lambda (line) (string-match-p "^[ \t]*:PROPERTIES:[ \t]*$" line))
     (split-string content "\n"))))

(defun no-duplicate-drawer-spec--count-end-lines (buffer)
  "Count `:END:' lines in the file-level drawer of BUFFER.

Parallel to `no-duplicate-drawer-spec--count-properties-headers';
scoped to the file-level drawer span (see
`no-duplicate-drawer-spec--file-level-drawer-region').

The cited invariant `register/invariant/scope-drawer-no-duplication'
names both lines (\"exactly one `:PROPERTIES:' line and exactly one
`:END:' line for the file-level drawer at point-min\").  Counting
both halves discharges the contract fully and bites a hypothetical
narrower regression that emits a stray `:END:' without a paired
`:PROPERTIES:'."
  (let* ((region (no-duplicate-drawer-spec--file-level-drawer-region buffer))
         (content (if region
                      (with-current-buffer buffer
                        (buffer-substring-no-properties (car region) (cdr region)))
                    "")))
    (cl-count-if
     (lambda (line) (string-match-p "^[ \t]*:END:[ \t]*$" line))
     (split-string content "\n"))))

(describe "drawer writer integrity"

  ;; The writer calls `save-buffer'; our temp buffers are not file-backed,
  ;; so each test stubs `save-buffer' to a no-op (the same approach used
  ;; by the existing "Drawer writer preserves structure" tests in
  ;; expansion-ui-spec.el).

  (it "produces exactly one :PROPERTIES: block after multiple add-to-scope writes"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_PRESET . "executor")
            (:GPTEL_SCOPE_READ . ("/initial/**")))
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/one/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/two/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :write "/output/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :execute "/usr/local/bin/**")
        (expect (no-duplicate-drawer-spec--count-properties-headers
                 (current-buffer))
                :to-equal 1)
        (expect (no-duplicate-drawer-spec--count-end-lines
                 (current-buffer))
                :to-equal 1))))

  (it "preserves :GPTEL_PRESET: across writes"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_PRESET . "executor")
            (:GPTEL_SCOPE_READ . ("/initial/**")))
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/**")
        (expect (org-entry-get (point-min) "GPTEL_PRESET")
                :to-equal "executor"))))

  (it "is idempotent for duplicate patterns"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_READ . ("/a/**")))
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/a/**")
        (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/a/**")))))

  ;; Cycle-3 add-on: preset-property corruption pin.  Cycle-3
  ;; finding-2 advisory on `migrate-expansion-tests' noted that the
  ;; spec scenario "Existing drawer keys are preserved" specifically
  ;; named `:GPTEL_PRESET:' as a non-scope key that must survive a
  ;; writer call.  The migrate-expansion test fixtured only
  ;; `:GPTEL_SCOPE_*:' keys, so the targeted defect class — the
  ;; corruption observed in
  ;; `~/org/roam/20260419111957-gptel_preset_property_corruption.org'
  ;; — was not directly pinned.  This `it' block fixtures both
  ;; `:GPTEL_PRESET:' AND `:GPTEL_PARENT_SESSION_ID:' alongside
  ;; scope keys, runs the writer through several add-to-scope cycles,
  ;; and asserts (a) exactly one `:PROPERTIES:' block AND (b) the
  ;; non-scope keys survive verbatim.
  (it "non-scope keys (preset, parent-session-id) survive a multi-write add-to-scope sequence"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_PRESET . "default")
            (:GPTEL_PARENT_SESSION_ID . "abc-123")
            (:GPTEL_SCOPE_READ . ("/initial/**")))
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/one/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :write "/output/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :execute "/usr/local/bin/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :modify "/etc/local/**")
        ;; (a) Drawer-singleton invariant survives the multi-write sequence.
        ;; Both header lines (`:PROPERTIES:' and `:END:') must remain singular
        ;; per `register/invariant/scope-drawer-no-duplication'.
        (expect (no-duplicate-drawer-spec--count-properties-headers
                 (current-buffer))
                :to-equal 1)
        (expect (no-duplicate-drawer-spec--count-end-lines
                 (current-buffer))
                :to-equal 1)
        ;; (b) Non-scope keys survive verbatim — neither corrupted nor
        ;; clobbered by the scope-key writes.
        (expect (org-entry-get (point-min) "GPTEL_PRESET")
                :to-equal "default")
        (expect (org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")
                :to-equal "abc-123"))))

  ;; Post-cycle-7 fixture coverage.  `register/shape/session-
  ;; document-layout' introduced a second drawer (the `* System Prompt'
  ;; heading's own :PROPERTIES:/:VISIBILITY: folded/:END: block).  The
  ;; pre-Addendum fixture `jf/gptel-test--with-scope-drawer' emits the
  ;; old shape (one drawer, bare #+begin_user block, no headings), so a
  ;; buffer-wide :PROPERTIES:/:END: count passed trivially.  This `it'
  ;; body runs the same multi-write add-to-scope sequence against the
  ;; canonical post-Addendum shape via
  ;; `jf/gptel-test--with-session-document', and asserts the tightened
  ;; file-level-scoped counters still report exactly one
  ;; :PROPERTIES:/:END: pair at point-min — even though buffer-wide
  ;; counts in this fixture are 2/2.  Together with the buffer-wide
  ;; counts asserted by the fixture's own self-tests in helpers-spec.el,
  ;; this discharges the invariant's prose ("file-level drawer at
  ;; point-min") against both fixture shapes.
  (it "file-level :PROPERTIES: drawer stays singular against the canonical session-document layout"
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-session-document
          '((:GPTEL_PRESET . "executor")
            (:GPTEL_SCOPE_READ . ("/initial/**")))
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/one/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/added/two/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :write "/output/**")
        (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :execute "/usr/local/bin/**")
        ;; File-level drawer remains singular (counters scoped to the
        ;; pre-heading region per `--file-level-drawer-region').
        (expect (no-duplicate-drawer-spec--count-properties-headers
                 (current-buffer))
                :to-equal 1)
        (expect (no-duplicate-drawer-spec--count-end-lines
                 (current-buffer))
                :to-equal 1)
        ;; Non-scope key still readable at point-min.
        (expect (org-entry-get (point-min) "GPTEL_PRESET")
                :to-equal "executor")
        ;; Canonical layout is intact: the two heading-level singletons
        ;; the document-layout invariant guards still hold.
        (expect (count-matches "^\\* System Prompt[ \t]*$"
                               (point-min) (point-max))
                :to-equal 1)
        (expect (count-matches "^\\* Chat[ \t]*$"
                               (point-min) (point-max))
                :to-equal 1)))))

(provide 'no-duplicate-drawer-spec)
;;; no-duplicate-drawer-spec.el ends here
