;;; bash-parser-contract-layers-spec.el --- Structural invariant tests for bash-parser → scope contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Contract property tests: 6 layers of structural invariants that guard the
;; bash-parser → scope validation boundary.  These tests derive cases from the
;; contract itself rather than manually enumerating scenarios.
;;
;; Layer 1: Source-of-truth symmetry (parser ↔ contract agreement)
;; Layer 2: Semantic classification (every operation has a scope mapping)
;; Layer 3: Permissive config (wide-open allows everything)
;; Layer 4: Restrictive config (empty denies everything)
;; Layer 5: Real command corpus (parser round-trip per operation type)
;; Layer 6: Error shape contract (denial plists have correct structure)
;;
;; Companion to bash-parser-integration-spec.el (scenario tests).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       ;; test/integration/ -> test/ -> scope/ -> gptel/ -> config/
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (config-dir (expand-file-name ".." gptel-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  ;; Contract infrastructure
  (add-to-list 'load-path contracts-dir)
  (require 'contract-core)
  (require 'contract-bash-parser)
  (contract--register-buttercup-matcher)
  ;; Scope validation (pulls in bash-parser-core, bash-parser-orchestrator)
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  ;; Helpers for scope config builders
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir)))

;;; Data Structures

(defconst contract-layers-spec--operation-classification
  '(;; Read-like: check read or write patterns
    (:read           . :read-like)
    (:read-directory . :read-like)
    (:read-metadata  . :read-like)
    (:match-pattern  . :read-like)
    ;; Write-like: check write patterns
    (:write          . :write-like)
    (:create         . :write-like)
    (:create-or-modify . :write-like)
    (:append         . :write-like)
    (:delete         . :write-like)
    ;; Modify-like: check modify or write patterns
    (:modify         . :modify-like)
    ;; Execute-like: check execute patterns
    (:execute        . :execute-like))
  "Maps each contract operation to its semantic category.
Categories determine which scope config keys allow the operation.")

(defconst contract-layers-spec--category-scope-keys
  '((:read-like    . (:read :write))      ; read OR write grants read access
    (:write-like   . (:write))             ; only write grants write access
    (:modify-like  . (:modify :write))     ; modify OR write grants modify access
    (:execute-like . (:execute)))          ; only execute grants execute access
  "Maps each semantic category to the scope config keys that allow it.
When multiple keys listed, any one is sufficient (OR semantics).")

(defconst contract-layers-spec--command-corpus
  '((:read           "cat /tmp/file.txt"           "cat")
    (:read-directory  "ls /tmp"                     "ls")
    (:read-metadata   "which python3"               "which")
    (:match-pattern   "find /tmp -name '*.txt'"     "find")
    (:write           "echo hello > /tmp/out.txt"   "echo")
    (:create          "mkdir /tmp/newdir"            "mkdir")
    (:create-or-modify "touch /tmp/file.txt"        "touch")
    (:append          "echo data >> /tmp/log.txt"   "echo")
    (:delete          "rm /tmp/file.txt"             "rm")
    (:modify          "chmod 755 /tmp/script.sh"    "chmod")
    (:execute         "bash /tmp/script.sh"         "bash"))
  "One representative command per operation type.
Format: (OPERATION COMMAND-STRING EXPECTED-COMMAND-NAME).
Used by Layer 5 for parser round-trip verification.")

;;; Fixture Builders

(defun contract-layers-spec--permissive-config ()
  "Build scope config that allows all path patterns.
Every operation type should be allowed for any path."
  (let ((scope-yml (helpers-spec-make-scope-yml
                    (helpers-spec--scope-with-paths
                     '("/**") '("/**") '("/**") '("/**") nil))))
    (unwind-protect
        (helpers-spec-load-scope-config scope-yml)
      (delete-file scope-yml))))

(defun contract-layers-spec--restrictive-config ()
  "Build scope config with empty path patterns.
Every operation type should be denied for any path."
  (let ((scope-yml (helpers-spec-make-scope-yml
                    (helpers-spec--scope-with-paths
                     nil nil nil nil nil))))
    (unwind-protect
        (helpers-spec-load-scope-config scope-yml)
      (delete-file scope-yml))))

;;; Test Layers

(describe "Contract property layers (structural invariants)"

  (let (permissive-config restrictive-config)

    (before-all
      (setq permissive-config (contract-layers-spec--permissive-config))
      (setq restrictive-config (contract-layers-spec--restrictive-config)))

;; -- Layer 1: Source-of-Truth Symmetry Guard --
(describe "Layer 1: source-of-truth symmetry"
  (it "contract/bash--valid-operations equals jf/bash-valid-operation-types"
    (let ((contract-ops (sort (copy-sequence contract/bash--valid-operations)
                              #'string< ))
          (parser-ops (sort (copy-sequence jf/bash-valid-operation-types)
                            #'string< )))
      (expect contract-ops :to-equal parser-ops))))

;; -- Layer 2: Semantic Classification Contract --
(describe "Layer 2: semantic classification"

  (it "every contract operation appears in the classification map"
    (let ((classified-ops (mapcar #'car contract-layers-spec--operation-classification)))
      (dolist (op contract/bash--valid-operations)
        (expect (memq op classified-ops) :to-be-truthy))))

  (it ":read-like operations are allowed with read patterns"
    (let* ((read-yml (helpers-spec-make-scope-yml
                      (helpers-spec--scope-with-paths
                       '("/**") nil nil nil nil)))
           (read-config (unwind-protect
                            (helpers-spec-load-scope-config read-yml)
                          (delete-file read-yml)))
           (paths-config (plist-get read-config :paths))
           (read-like-ops (mapcar #'car
                                  (cl-remove-if-not
                                   (lambda (entry) (eq (cdr entry) :read-like))
                                   contract-layers-spec--operation-classification))))
      (dolist (op read-like-ops)
        (expect (jf/gptel-scope--validate-operation op "/tmp/file.txt" paths-config)
                :to-be nil))))

  (it ":write-like operations are allowed with write patterns"
    (let* ((write-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        nil '("/**") nil nil nil)))
           (write-config (unwind-protect
                             (helpers-spec-load-scope-config write-yml)
                           (delete-file write-yml)))
           (paths-config (plist-get write-config :paths))
           (write-like-ops (mapcar #'car
                                   (cl-remove-if-not
                                    (lambda (entry) (eq (cdr entry) :write-like))
                                    contract-layers-spec--operation-classification))))
      (dolist (op write-like-ops)
        (expect (jf/gptel-scope--validate-operation op "/tmp/file.txt" paths-config)
                :to-be nil))))

  (it ":modify-like operations are allowed with modify patterns"
    (let* ((modify-yml (helpers-spec-make-scope-yml
                        (helpers-spec--scope-with-paths
                         nil nil nil '("/**") nil)))
           (modify-config (unwind-protect
                              (helpers-spec-load-scope-config modify-yml)
                            (delete-file modify-yml)))
           (paths-config (plist-get modify-config :paths))
           (modify-like-ops (mapcar #'car
                                    (cl-remove-if-not
                                     (lambda (entry) (eq (cdr entry) :modify-like))
                                     contract-layers-spec--operation-classification))))
      (dolist (op modify-like-ops)
        (expect (jf/gptel-scope--validate-operation op "/tmp/file.txt" paths-config)
                :to-be nil))))

  (it ":execute-like operations are allowed with execute patterns"
    (let* ((exec-yml (helpers-spec-make-scope-yml
                      (helpers-spec--scope-with-paths
                       nil nil '("/**") nil nil)))
           (exec-config (unwind-protect
                            (helpers-spec-load-scope-config exec-yml)
                          (delete-file exec-yml)))
           (paths-config (plist-get exec-config :paths))
           (exec-like-ops (mapcar #'car
                                  (cl-remove-if-not
                                   (lambda (entry) (eq (cdr entry) :execute-like))
                                   contract-layers-spec--operation-classification))))
      (dolist (op exec-like-ops)
        (expect (jf/gptel-scope--validate-operation op "/tmp/file.txt" paths-config)
                :to-be nil)))))

;; -- Layer 3: Permissive Config Property --
(describe "Layer 3: permissive config allows everything"
  (it "every contract operation is allowed with all-permissive paths"
    (let ((paths-config (plist-get permissive-config :paths))
          (failures nil))
      (dolist (op contract/bash--valid-operations)
        (let ((result (jf/gptel-scope--validate-operation
                       op "/tmp/file.txt" paths-config)))
          (when result
            (push (format "%s -> %S" op result) failures))))
      (expect failures :to-equal nil))))

;; -- Layer 4: Restrictive Config Property --
(describe "Layer 4: restrictive config denies everything"
  (it "every contract operation is denied with empty paths"
    (let ((paths-config (plist-get restrictive-config :paths))
          (passes nil))
      (dolist (op contract/bash--valid-operations)
        (let ((result (jf/gptel-scope--validate-operation
                       op "/tmp/file.txt" paths-config)))
          (when (null result)
            (push op passes))))
      (expect passes :to-equal nil))))

;; -- Layer 5: Real Command Corpus --
(describe "Layer 5: real command corpus round-trip"
  (dolist (entry contract-layers-spec--command-corpus)
    (let ((expected-op (nth 0 entry))
          (command (nth 1 entry))
          (expected-cmd-name (nth 2 entry)))
      (it (format "parses %S and extracts :%s operation"
                  command (substring (symbol-name expected-op) 1))
        (let* ((parsed (jf/bash-parse command))
               (semantics (jf/bash-extract-semantics parsed))
               (file-ops (alist-get :filesystem (plist-get semantics :domains))))
          ;; Parser should produce valid output
          (expect parsed :to-satisfy-contract #'contract/bash-parse-result--validate)
          (expect semantics :to-satisfy-contract #'contract/bash-semantics--validate)
          ;; Expected operation should appear in file-ops
          ;; (may be nil for commands whose file-ops depend on redirect extraction)
          (if file-ops
              (expect (cl-some (lambda (op)
                                 (eq (plist-get op :operation) expected-op))
                               file-ops)
                      :to-be-truthy)
            ;; No file-ops extracted — informative pending
            (expect file-ops :not :to-be nil)))))))

;; -- Layer 6: Error Shape Contract --
(describe "Layer 6: error shape contract"
  (it "every denied operation produces correctly shaped error plist"
    (let ((paths-config (plist-get restrictive-config :paths))
          (test-path "/tmp/file.txt")
          (shape-errors nil))
      (dolist (op contract/bash--valid-operations)
        (let ((result (jf/gptel-scope--validate-operation
                       op test-path paths-config)))
          (cond
           ;; Unexpectedly allowed — not a shape error, Layer 4 catches this
           ((null result) nil)
           ;; Check error plist shape
           (t
            (unless (stringp (plist-get result :error))
              (push (format "%s: :error not a string, got %S"
                            op (plist-get result :error))
                    shape-errors))
            (unless (and (stringp (plist-get result :path))
                         (string= (plist-get result :path) test-path))
              (push (format "%s: :path mismatch, expected %S got %S"
                            op test-path (plist-get result :path))
                    shape-errors))
            (unless (eq (plist-get result :operation) op)
              (push (format "%s: :operation mismatch, expected %S got %S"
                            op op (plist-get result :operation))
                    shape-errors))
            (unless (stringp (plist-get result :message))
              (push (format "%s: :message not a string, got %S"
                            op (plist-get result :message))
                    shape-errors))))))
      (expect shape-errors :to-equal nil))))))

(provide 'bash-parser-contract-layers-spec)

;;; bash-parser-contract-layers-spec.el ends here
