;;; debug-ls-parsing-spec.el --- Debug ls command parsing -*- lexical-binding: t; -*-

;;; Commentary:
;; Check what bash-parser returns for ls commands to find the nil issue

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (semantic-dir test-dir)
       (scope-test-dir (expand-file-name ".." semantic-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'bash-parser-core)
  (require 'bash-parser-plugins))

(describe "Bash parser output for ls commands"

  (it "should return proper structure for ls command"
    (let* ((command "ls -la /opt/homebrew/bin")
           (parsed (jf/bash-parse command))
           (semantics (jf/bash-extract-semantics parsed))
           (domains (plist-get semantics :domains))
           (file-ops (alist-get :filesystem domains)))

      (message "========== DEBUG ==========")
      (message "Parsed: %S" parsed)
      (message "Semantics: %S" semantics)
      (message "Domains: %S" domains)
      (message "File-ops: %S" file-ops)
      (message "File-ops type: %s" (type-of file-ops))
      (message "File-ops length: %s" (length file-ops))
      (message "===========================")

      ;; file-ops should be a list or nil
      (expect (or (null file-ops)
                  (listp file-ops))
              :to-be t)

      ;; If file-ops is a list, each element should be a plist
      (when (and file-ops (listp file-ops))
        (dolist (op file-ops)
          (expect (plistp op) :to-be t)
          (expect (plist-get op :operation) :not :to-be nil)
          (expect (plist-get op :path) :not :to-be nil))))))

(provide 'debug-ls-parsing-spec)
;;; debug-ls-parsing-spec.el ends here
