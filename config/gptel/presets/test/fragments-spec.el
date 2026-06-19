;;; fragments-spec.el --- Specs for the fragment parser + renderer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `config/gptel/presets/fragments.el':
;;
;; - Section parsing: top-level Org headings -> ordered (name . body) sections.
;; - Fragment kind: defaults to static; declared kind is read.
;; - Claude rendering: each section -> XML block, joined with a blank line,
;;   asserted against golden snapshots.
;; - Section-name -> tag: multi-word heading -> snake_case tag.
;; - Backend dispatch: an unimplemented backend is reported (signal), never
;;   silently rendered.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test and the golden-file reader.
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (presets-dir (expand-file-name ".." test-dir)))
  (require 'presets-helpers-spec (expand-file-name "helpers-spec.el" test-dir))
  (require 'jf-gptel-fragments (expand-file-name "fragments.el" presets-dir)))

(describe "Fragment parsing"

  (it "parses three top-level headings into three ordered sections"
    (let* ((source "* Role\nYou are X.\n\n* Background\nContext here.\n\n* Constraints\nBe concise.\n")
           (fragment (jf/gptel-fragment--parse-source source))
           (sections (plist-get fragment :sections)))
      (expect (length sections) :to-equal 3)
      (expect (mapcar #'car sections)
              :to-equal '("Role" "Background" "Constraints"))))

  (it "captures each section's body trimmed of surrounding whitespace"
    (let* ((source "* Role\n\n  You are X.\n\n* Background\nContext here.\n")
           (sections (plist-get (jf/gptel-fragment--parse-source source)
                                :sections)))
      (expect (cdr (assoc "Role" sections)) :to-equal "You are X.")
      (expect (cdr (assoc "Background" sections)) :to-equal "Context here.")))

  (it "accepts a fragment with exactly one section"
    (let* ((source "* Role\nYou are X.\n")
           (sections (plist-get (jf/gptel-fragment--parse-source source)
                                :sections)))
      (expect (length sections) :to-equal 1)
      (expect (car (car sections)) :to-equal "Role")))

  (it "ignores content before the first heading"
    (let* ((source "preamble noise\n#+fragment_kind: static\n\n* Role\nYou are X.\n")
           (sections (plist-get (jf/gptel-fragment--parse-source source)
                                :sections)))
      (expect (length sections) :to-equal 1)
      (expect (car (car sections)) :to-equal "Role"))))

(describe "Fragment kind"

  (it "defaults to static when no kind is declared"
    (let ((fragment (jf/gptel-fragment--parse-source "* Role\nYou are X.\n")))
      (expect (plist-get fragment :kind) :to-equal 'static)))

  (it "reads a declared dynamic kind"
    (let ((fragment (jf/gptel-fragment--parse-source
                     "#+fragment_kind: dynamic\n* Role\nYou are X.\n")))
      (expect (plist-get fragment :kind) :to-equal 'dynamic)))

  (it "falls back to static for an unrecognized declared kind"
    (let ((fragment (jf/gptel-fragment--parse-source
                     "#+fragment_kind: bogus\n* Role\nYou are X.\n")))
      (expect (plist-get fragment :kind) :to-equal 'static))))

(describe "Section name to tag"

  (it "downcases a single-word heading"
    (expect (jf/gptel-fragment--section-name-to-tag "Role")
            :to-equal "role"))

  (it "converts a multi-word heading to snake_case"
    (expect (jf/gptel-fragment--section-name-to-tag "Output Format")
            :to-equal "output_format"))

  (it "collapses runs of whitespace into a single underscore"
    (expect (jf/gptel-fragment--section-name-to-tag "Output   Format")
            :to-equal "output_format")))

(describe "Claude rendering"

  (it "renders Role/Background/Constraints to the golden snapshot"
    (let* ((source "* Role\nYou are a careful assistant.\n\n* Background\nThe user works in Emacs.\n\n* Constraints\nBe concise.\n")
           (fragment (jf/gptel-fragment--parse-source source)))
      (expect (jf/gptel-fragment-render fragment 'claude)
              :to-equal (presets-test-read-golden "core-sample.claude.txt"))))

  (it "wraps a single section in its derived XML tag"
    (let* ((source "* Role\nYou are X.\n")
           (fragment (jf/gptel-fragment--parse-source source)))
      (expect (jf/gptel-fragment-render fragment 'claude)
              :to-equal "<role>\nYou are X.\n</role>")))

  (it "derives a snake_case tag from a multi-word heading"
    (let* ((source "* Output Format\nReturn JSON.\n")
           (fragment (jf/gptel-fragment--parse-source source)))
      (expect (jf/gptel-fragment-render fragment 'claude)
              :to-equal (presets-test-read-golden
                         "output-format-sample.claude.txt")))))

(describe "Backend dispatch"

  (it "signals for an unimplemented backend rather than rendering"
    (let* ((source "* Role\nYou are X.\n")
           (fragment (jf/gptel-fragment--parse-source source)))
      (expect (jf/gptel-fragment-render fragment 'gpt)
              :to-throw 'jf/gptel-fragment-unimplemented-backend)))

  (it "does not emit Claude/XML output for an unimplemented backend"
    (let* ((source "* Role\nYou are X.\n")
           (fragment (jf/gptel-fragment--parse-source source))
           (result (condition-case err
                       (jf/gptel-fragment-render fragment 'gpt)
                     (jf/gptel-fragment-unimplemented-backend
                      (cons :signalled err)))))
      ;; The render must not have returned a string; it must have signalled.
      (expect (car-safe result) :to-equal :signalled))))

(provide 'fragments-spec)
;;; fragments-spec.el ends here
