;;; test-filesystem-plugin.el --- Tests for filesystem plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests for bash-parser filesystem plugin.
;; Tests token claiming and plugin integration.
;;
;; Test naming convention: test-filesystem-plugin-<scenario-slug>

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory load-file-name)))
(require 'bash-parser-plugins (expand-file-name "../../../bash-parser-plugins.el"
                                                (file-name-directory load-file-name)))
(require 'bash-parser-core (expand-file-name "../../../bash-parser-core.el"
                                             (file-name-directory load-file-name)))

;;; Filesystem Plugin Tests

(ert-deftest test-filesystem-plugin-basic-redirection ()
  "Test filesystem plugin extracts operations from simple redirection."
  (let* ((parsed (jf/bash-parse "cat input.txt > output.txt"))
         (result (jf/bash-plugin-filesystem parsed)))

    ;; Should return valid plugin result
    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :filesystem))

    ;; Should extract both operations
    (let ((operations (jf/bash-plugin-result-operations result)))
      (should (= (length operations) 2))

      ;; Should have read and write operations
      (should (seq-some (lambda (op) (eq (plist-get op :operation) :read))
                        operations))
      (should (seq-some (lambda (op) (eq (plist-get op :operation) :write))
                        operations)))

    ;; Should claim tokens
    (let ((claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))
      (should (> (length claimed-ids) 0)))))

(ert-deftest test-filesystem-plugin-positional-args ()
  "Test filesystem plugin extracts operations from positional arguments."
  (let* ((parsed (jf/bash-parse "rm file.txt"))
         (result (jf/bash-plugin-filesystem parsed)))

    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :filesystem))

    ;; Should extract delete operation
    (let ((operations (jf/bash-plugin-result-operations result)))
      (should (= (length operations) 1))

      (let ((op (car operations)))
        (should (eq (plist-get op :operation) :delete))
        (should (string= (plist-get op :file) "file.txt"))
        (should (string= (plist-get op :command) "rm"))))

    ;; Should claim command-name and file path tokens
    (let ((claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))
      (should (> (length claimed-ids) 0)))))

(ert-deftest test-filesystem-plugin-no-file-operations ()
  "Test filesystem plugin returns nil for non-file commands."
  (let* ((parsed (jf/bash-parse "echo hello"))
         (result (jf/bash-plugin-filesystem parsed)))

    ;; Should return result even if no operations (empty operations list)
    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :filesystem))

    ;; Operations list should be empty
    (let ((operations (jf/bash-plugin-result-operations result)))
      (should (= (length operations) 0)))))

(ert-deftest test-filesystem-plugin-exec-blocks ()
  "Test filesystem plugin extracts operations from find -exec blocks."
  (let* ((parsed (jf/bash-parse "find . -name '*.txt' -exec cat {} \\;"))
         (result (jf/bash-plugin-filesystem parsed)))

    (should (jf/bash-plugin-result-p result))
    (should (eq (jf/bash-plugin-result-domain result) :filesystem))

    ;; Should extract operations from exec block
    (let ((operations (jf/bash-plugin-result-operations result)))
      (should (> (length operations) 0))

      ;; Should have read operation from cat in exec block
      (should (seq-some (lambda (op)
                          (and (eq (plist-get op :operation) :read)
                               (eq (plist-get op :source) :exec-block)))
                        operations)))))

(ert-deftest test-filesystem-plugin-token-claiming-redirections ()
  "Test filesystem plugin claims redirection tokens."
  (let* ((parsed (jf/bash-parse "cat file.txt > output.txt"))
         (result (jf/bash-plugin-filesystem parsed))
         (tokens (plist-get parsed :tokens))
         (claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))

    ;; Find redirection token IDs
    (let ((redir-token-ids
           (mapcar (lambda (token) (plist-get token :id))
                   (seq-filter (lambda (token)
                                (eq (plist-get token :type) :redirection))
                              tokens))))

      ;; All redirection tokens should be claimed
      (dolist (redir-id redir-token-ids)
        (should (member redir-id claimed-ids))))))

(ert-deftest test-filesystem-plugin-token-claiming-command-name ()
  "Test filesystem plugin claims command-name tokens for file operations."
  (let* ((parsed (jf/bash-parse "cat file.txt"))
         (result (jf/bash-plugin-filesystem parsed))
         (tokens (plist-get parsed :tokens))
         (claimed-ids (jf/bash-plugin-result-claimed-token-ids result)))

    ;; Find command-name token ID
    (let ((cmd-token-id
           (plist-get (seq-find (lambda (token)
                                 (eq (plist-get token :type) :command-name))
                               tokens)
                      :id)))

      ;; Command-name token should be claimed
      (should (member cmd-token-id claimed-ids)))))

(ert-deftest test-filesystem-plugin-predicate-has-file-tokens ()
  "Test filesystem plugin predicate identifies file-related commands."
  ;; Commands with redirections
  (let ((parsed (jf/bash-parse "cat file.txt > output.txt")))
    (should (jf/bash-plugin-filesystem--has-file-tokens-p parsed)))

  ;; Known file commands
  (let ((parsed (jf/bash-parse "ls -la")))
    (should (jf/bash-plugin-filesystem--has-file-tokens-p parsed)))

  ;; Commands with positional args
  (let ((parsed (jf/bash-parse "cat file.txt")))
    (should (jf/bash-plugin-filesystem--has-file-tokens-p parsed)))

  ;; Non-file commands
  (let ((parsed (jf/bash-parse "echo hello")))
    (should-not (jf/bash-plugin-filesystem--has-file-tokens-p parsed))))

(ert-deftest test-filesystem-plugin-metadata ()
  "Test filesystem plugin includes extraction metadata."
  (let* ((parsed (jf/bash-parse "cat file.txt > output.txt"))
         (result (jf/bash-plugin-filesystem parsed))
         (metadata (jf/bash-plugin-result-metadata result)))

    ;; Should include operation count
    (should (plist-get metadata :operation-count))
    (should (numberp (plist-get metadata :operation-count)))

    ;; Should include extraction source
    (should (eq (plist-get metadata :extraction-source)
                'jf/bash-extract-file-operations))))

(ert-deftest test-filesystem-plugin-matches-existing-extraction ()
  "Test filesystem plugin produces same operations as existing extraction."
  (let* ((command "cat input.txt > output.txt")
         (parsed (jf/bash-parse command))
         ;; Extract using existing function
         (existing-ops (jf/bash-extract-file-operations parsed nil))
         ;; Extract using plugin
         (plugin-result (jf/bash-plugin-filesystem parsed))
         (plugin-ops (jf/bash-plugin-result-operations plugin-result)))

    ;; Should extract same number of operations
    (should (= (length existing-ops) (length plugin-ops)))

    ;; Operations should match (ignoring order)
    (dolist (existing-op existing-ops)
      (should (seq-some (lambda (plugin-op)
                          (and (eq (plist-get existing-op :operation)
                                   (plist-get plugin-op :operation))
                               (string= (plist-get existing-op :file)
                                        (plist-get plugin-op :file))
                               (eq (plist-get existing-op :source)
                                   (plist-get plugin-op :source))))
                        plugin-ops)))))

;;; Registration Tests

(ert-deftest test-filesystem-plugin-registration ()
  "Test filesystem plugin can be registered successfully."
  (setq jf/bash-semantic-plugins '())

  ;; Register the plugin
  (jf/bash-register-filesystem-plugin)

  ;; Should be in registry
  (should (= (length jf/bash-semantic-plugins) 1))

  (let ((plugin (car jf/bash-semantic-plugins)))
    (should (eq (plist-get plugin :name) 'filesystem))
    (should (= (plist-get plugin :priority) 100))
    (should (functionp (plist-get plugin :extractor)))
    (should (= (length (plist-get plugin :predicates)) 1))))

(ert-deftest test-filesystem-plugin-in-orchestrator ()
  "Test filesystem plugin works through orchestrator."
  (setq jf/bash-semantic-plugins '())

  ;; Register the plugin
  (jf/bash-register-filesystem-plugin)

  ;; Parse and extract through orchestrator
  (let* ((parsed (jf/bash-parse "cat file.txt > output.txt"))
         (result (jf/bash-extract-semantics parsed))
         (domains (plist-get result :domains)))

    ;; Should have filesystem domain
    (should (assq :filesystem domains))

    ;; Should have operations
    (let ((fs-ops (cdr (assq :filesystem domains))))
      (should (> (length fs-ops) 0))

      ;; Should have both read and write operations
      (should (seq-some (lambda (op) (eq (plist-get op :operation) :read))
                        fs-ops))
      (should (seq-some (lambda (op) (eq (plist-get op :operation) :write))
                        fs-ops)))))

(provide 'test-filesystem-plugin)
;;; test-filesystem-plugin.el ends here
