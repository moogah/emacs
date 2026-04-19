;;; skills-core-spec.el --- Regression tests for jf/gptel-skills core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Regression tests for the property-drawer-corruption bug fixed
;; 2026-04-19.
;;
;; The bug: jf/gptel-skills--detect-mentions ran re-search-forward
;; without save-match-data while invoked from after-change-functions
;; via jf/gptel-skills--update-overlays. When org-entry-put's UPDATE
;; branch deleted a property line, the resulting after-change cascade
;; clobbered org-entry-put's match-data, so its next
;; (goto-char (match-beginning 0)) jumped to the position of the last
;; @mention in the buffer. The property line was re-inserted there
;; instead of back inside the drawer — producing the characteristic
;; ":KEY: value@user" concatenation at EOF and a pseudo-drawer left
;; at point-min.
;;
;; Full analysis:
;; ~/org/roam/20260419111957-gptel_preset_property_corruption.org

;;; Code:

(require 'buttercup)
(require 'org)
(require 'gptel)
(require 'gptel-skills)

(describe "jf/gptel-skills--detect-mentions"

  (it "preserves the caller's match-data"
    ;; The function runs from after-change-functions; any
    ;; re-search-forward inside it must be wrapped in save-match-data
    ;; so the caller's match state survives the hook firing.
    (with-temp-buffer
      (insert "@user\n@assistant\n@somebody\n")
      (string-match "foo\\(bar\\)baz" "foobarbaz")
      (let ((before (match-data)))
        (jf/gptel-skills--detect-mentions)
        (expect (match-data) :to-equal before)))))

(describe "Property-drawer corruption regression"

  (it "leaves the top-level drawer valid after `org-entry-put' UPDATE"
    ;; Reproduces the smallest pre-fix failing case: gptel-mode
    ;; active, @mentions present in the buffer body, and a single
    ;; org-entry-put call that hits the UPDATE branch (existing
    ;; property). Pre-fix this corrupted the drawer; post-fix the
    ;; drawer stays valid.
    (let ((file (make-temp-file "skills-regression-" nil ".org"))
          ;; Fresh registry so update-overlays clears its non-empty
          ;; guard without depending on user-installed skills.
          (jf/gptel-skills--registry (make-hash-table :test 'equal)))
      (puthash "test-skill" '(:name "test-skill")
               jf/gptel-skills--registry)
      (unwind-protect
          (with-current-buffer (find-file-noselect file)
            (erase-buffer)
            (insert ":PROPERTIES:\n:KEY1: val1\n:END:\n"
                    "* Heading\n\n@user\nhi\n\n"
                    "@assistant\nresponse text here\n\n@user\n\n")
            (save-buffer)
            (let ((inhibit-message t))
              (gptel-mode 1))
            (org-entry-put (point-min) "KEY1" "newval")
            (let ((contents (buffer-substring-no-properties
                             (point-min) (point-max))))
              ;; Top drawer is valid and contains the new value.
              (expect contents :to-match
                      "\\`:PROPERTIES:\n:KEY1:[ \t]+newval\n:END:\n")
              ;; No "@<word>" concatenation anywhere — the smoking
              ;; gun of the original corruption.
              (expect contents :not :to-match ":KEY1:[^\n]*@")
              ;; Exactly one :PROPERTIES: drawer (no stacking).
              (expect (with-temp-buffer
                        (insert contents)
                        (goto-char (point-min))
                        (let ((n 0))
                          (while (re-search-forward "^:PROPERTIES:$" nil t)
                            (setq n (1+ n)))
                          n))
                      :to-equal 1)))
        (when (get-file-buffer file)
          (with-current-buffer (get-file-buffer file)
            (set-buffer-modified-p nil))
          (kill-buffer (get-file-buffer file)))
        (when (file-exists-p file) (delete-file file))))))

(provide 'skills-core-spec)
;;; skills-core-spec.el ends here
