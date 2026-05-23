;;; sibling-system-prompt-file-spec.el --- Sibling system-prompt file helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Focused unit tests for `jf/gptel--preset-source-file-extension' and
;; `jf/gptel--write-system-prompt-sibling-file' — the two standalone
;; helpers that materialise the per-session `system-prompt.<ext>'
;; sibling file (design.md §Decision 1, §Decision 2, §Decision 3 of
;; replace-system-prompt-heading-with-sibling-file).
;;
;; The extension lookup defaults defensively to "md" when the preset's
;; source file cannot be resolved.  The writer is a pure helper: it
;; never mutates buffer-local state, writes the preset's `:system'
;; body verbatim, and returns the basename so the caller can thread it
;; into the `:GPTEL_SYSTEM_PROMPT_FILE:' drawer key.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-commands)

(describe "jf/gptel--preset-source-file-extension"
  :var (tmp-presets-dir orig-presets-dir)

  (before-each
    (setq tmp-presets-dir (make-temp-file "jf-gptel-presets-" t))
    (setq orig-presets-dir jf/gptel-presets-directory)
    (setq jf/gptel-presets-directory tmp-presets-dir)
    (write-region "" nil (expand-file-name "coding.md" tmp-presets-dir)
                  nil 'silent))

  (after-each
    (setq jf/gptel-presets-directory orig-presets-dir)
    (when (and tmp-presets-dir (file-directory-p tmp-presets-dir))
      (delete-directory tmp-presets-dir t)))

  (it "returns \"md\" for a registered .md preset (symbol form)"
    (expect (jf/gptel--preset-source-file-extension 'coding)
            :to-equal "md"))

  (it "returns \"md\" for a registered .md preset (string form)"
    (expect (jf/gptel--preset-source-file-extension "coding")
            :to-equal "md"))

  (it "returns \"md\" as the defensive default for an unknown preset and logs a warning"
    (spy-on 'jf/gptel--log)
    (expect (jf/gptel--preset-source-file-extension 'nonexistent-preset)
            :to-equal "md")
    (expect 'jf/gptel--log :to-have-been-called)
    ;; The first call captures the warn level.
    (let ((level (nth 0 (spy-calls-args-for 'jf/gptel--log 0))))
      (expect level :to-equal 'warn)))

  (it "returns \"md\" and warns when the presets directory does not exist"
    (setq jf/gptel-presets-directory
          (expand-file-name "no-such-dir" tmp-presets-dir))
    (spy-on 'jf/gptel--log)
    (expect (jf/gptel--preset-source-file-extension 'coding)
            :to-equal "md")
    (expect 'jf/gptel--log :to-have-been-called))

  ;; Future: when `.org'-format preset support lands, this describe
  ;; block grows an `it "returns \"org\" for a registered .org preset"'
  ;; case.  Until the scanner regex widens, leaving this as a comment
  ;; documents the design's extension-agnosticism without a live test
  ;; for a code path that cannot be exercised.
  )

(describe "jf/gptel--write-system-prompt-sibling-file"
  :var (tmp-session-dir orig-presets-dir tmp-presets-dir)

  (before-each
    (setq tmp-session-dir (make-temp-file "jf-gptel-session-" t))
    (setq tmp-presets-dir (make-temp-file "jf-gptel-presets-" t))
    (setq orig-presets-dir jf/gptel-presets-directory)
    (setq jf/gptel-presets-directory tmp-presets-dir)
    (write-region "" nil (expand-file-name "coding.md" tmp-presets-dir)
                  nil 'silent))

  (after-each
    (setq jf/gptel-presets-directory orig-presets-dir)
    (when (and tmp-session-dir (file-directory-p tmp-session-dir))
      (delete-directory tmp-session-dir t))
    (when (and tmp-presets-dir (file-directory-p tmp-presets-dir))
      (delete-directory tmp-presets-dir t)))

  (it "writes system-prompt.md with the preset :system verbatim"
    (let ((body "You are a careful coding assistant.\nFollow the user's preferences.\n"))
      (jf/gptel--write-system-prompt-sibling-file
       tmp-session-dir 'coding (list :system body))
      (let ((path (expand-file-name "system-prompt.md" tmp-session-dir)))
        (expect (file-exists-p path) :to-be-truthy)
        (with-temp-buffer
          (insert-file-contents path)
          (expect (buffer-string) :to-equal body)))))

  (it "returns the basename on successful write"
    (expect (jf/gptel--write-system-prompt-sibling-file
             tmp-session-dir 'coding (list :system "non-empty body"))
            :to-equal "system-prompt.md"))

  (it "returns nil and does not write when :system is nil"
    (expect (jf/gptel--write-system-prompt-sibling-file
             tmp-session-dir 'coding (list :system nil))
            :to-be nil)
    (expect (file-exists-p (expand-file-name "system-prompt.md" tmp-session-dir))
            :to-be nil))

  (it "returns nil and does not write when :system is missing entirely"
    (expect (jf/gptel--write-system-prompt-sibling-file
             tmp-session-dir 'coding (list :model 'gpt-4))
            :to-be nil)
    (expect (file-exists-p (expand-file-name "system-prompt.md" tmp-session-dir))
            :to-be nil))

  (it "returns nil and does not write when :system is whitespace-only"
    (expect (jf/gptel--write-system-prompt-sibling-file
             tmp-session-dir 'coding (list :system "   \n\t  \n"))
            :to-be nil)
    (expect (file-exists-p (expand-file-name "system-prompt.md" tmp-session-dir))
            :to-be nil))

  (it "preserves exact whitespace and special characters from :system"
    (let ((body (concat "# Markdown heading\n\n"
                        "```bash\n"
                        "echo \"quoted\"\n"
                        "```\n\n"
                        "<tag>xml-like content</tag>\n"
                        "  leading-spaces preserved\n"
                        "trailing-spaces preserved   \n")))
      (jf/gptel--write-system-prompt-sibling-file
       tmp-session-dir 'coding (list :system body))
      (let ((path (expand-file-name "system-prompt.md" tmp-session-dir)))
        (with-temp-buffer
          (insert-file-contents path)
          (expect (buffer-string) :to-equal body)))))

  (it "does not mutate buffer-local state of the current buffer"
    (with-temp-buffer
      (let ((marker (buffer-name)))
        (jf/gptel--write-system-prompt-sibling-file
         tmp-session-dir 'coding (list :system "side-effect probe"))
        (expect (buffer-name) :to-equal marker)
        (expect (buffer-string) :to-equal "")))))

(provide 'sibling-system-prompt-file-spec)

;;; sibling-system-prompt-file-spec.el ends here
