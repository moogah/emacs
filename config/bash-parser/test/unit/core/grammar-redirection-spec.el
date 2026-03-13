;;; grammar-redirection-spec.el --- Layer 0: Redirection extraction tests -*- lexical-binding: t; -*-

;; Tests the grammar-level redirection extraction in isolation.
;; Redirections are unconditional grammar constructs -- they are extracted
;; regardless of whether the command has a registered handler.
;;
;; Uses two approaches:
;; 1. Full orchestrator (jf/bash-extract-semantics) for simple commands
;; 2. Direct redirection extraction (jf/bash-extract-operations-from-redirections)
;;    and recursive engine (jf/bash--extract-file-operations-impl) for compound
;;    commands to avoid the cl-return token claiming issue

;;; Code:

(require 'cl-lib)
(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))
(require 'bash-parser-orchestrator)
(require 'bash-parser-semantics)

;; Load contract validation helpers
(require 'contract-test-helpers
         (expand-file-name "config/bash-parser/commands/test/contract-test-helpers.el"
                           jf/emacs-dir))

;;; Helpers

(defvar grammar-redirection-test--saved-handlers nil
  "Saved handler table to restore after each test.")

(defun grammar-redirection-test--find-op (ops &rest criteria)
  "Find first operation in OPS matching all CRITERIA key-value pairs.
CRITERIA is a plist of field-value pairs to match."
  (seq-find
   (lambda (op)
     (let ((match t)
           (remaining criteria))
       (while (and match remaining)
         (let ((key (car remaining))
               (val (cadr remaining)))
           (unless (equal (plist-get op key) val)
             (setq match nil))
           (setq remaining (cddr remaining))))
       match))
   ops))

(defun grammar-redirection-test--get-filesystem-ops (result)
  "Extract :filesystem operations from orchestrator RESULT."
  (let ((domains (plist-get result :domains)))
    (cdr (assq :filesystem domains))))

;;; Tests

(describe "Grammar redirection extraction"

  (before-each
    (setq grammar-redirection-test--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal)))

  (after-each
    (setq jf/bash-command-handlers grammar-redirection-test--saved-handlers))

  ;; --- Operator type coverage ---

  (describe "redirection operator types"

    (it "> produces :write operation"
      (let* ((parsed (jf/bash-parse "echo hello > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op fs-ops
                    :file "output.txt" :operation :write)))
          (expect op :not :to-be nil)
          (expect (plist-get op :source) :to-equal :redirection)
          (expect (plist-get op :confidence) :to-equal :high)
          (expect (plist-get op :command) :to-equal "echo"))))

    (it ">> produces :append operation"
      (let* ((parsed (jf/bash-parse "echo data >> log.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op fs-ops
                    :file "log.txt" :operation :append)))
          (expect op :not :to-be nil)
          (expect (plist-get op :source) :to-equal :redirection)
          (expect (plist-get op :confidence) :to-equal :high))))

    (it "< produces :read operation"
      (let* ((parsed (jf/bash-parse "sort < input.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op fs-ops
                    :file "input.txt" :operation :read)))
          (expect op :not :to-be nil)
          (expect (plist-get op :source) :to-equal :redirection)
          (expect (plist-get op :confidence) :to-equal :high))))

    (it "2> produces :write operation for stderr redirection"
      (let* ((parsed (jf/bash-parse "cmd 2> errors.log"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op fs-ops
                    :file "errors.log" :operation :write)))
          (expect op :not :to-be nil)
          (expect (plist-get op :source) :to-equal :redirection)
          (expect (plist-get op :confidence) :to-equal :high))))

    (it "all redirection operations have :metadata containing original redirect plist"
      (let* ((parsed (jf/bash-parse "echo hello > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op fs-ops
                    :file "output.txt" :operation :write)))
          (expect op :not :to-be nil)
          ;; :metadata should contain the original redirection plist
          (let ((metadata (plist-get op :metadata)))
            (expect metadata :not :to-be nil)
            (expect (plist-get metadata :operator) :to-equal ">"))))))

  ;; --- Compound context with redirections ---

  (describe "compound context with redirections"

    (it "extracts redirections from pipeline stages"
      ;; Use recursive engine directly for compound + handler commands
      (let* ((parsed (jf/bash-parse "cat input.txt | sort > sorted.txt"))
             (ops (jf/bash--extract-file-operations-impl parsed nil)))
        (expect ops :not :to-be nil)
        ;; Redirection on sort stage
        (expect (grammar-redirection-test--find-op ops
                  :file "sorted.txt" :operation :write :source :redirection)
                :not :to-be nil)))

    (it "extracts redirections from chain commands"
      ;; echo has no handler, so orchestrator works
      (let* ((parsed (jf/bash-parse "echo start > log.txt && echo done >> log.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        ;; First command: > log.txt => :write
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "log.txt" :operation :write)
                :not :to-be nil)
        ;; Second command: >> log.txt => :append
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "log.txt" :operation :append)
                :not :to-be nil)))

    (it "extracts redirections from both pipeline and chain combined"
      ;; Use recursive engine for compound commands
      (let* ((parsed (jf/bash-parse "cat data.txt | sort > sorted.txt && echo done > status.txt"))
             (ops (jf/bash--extract-file-operations-impl parsed nil)))
        (expect ops :not :to-be nil)
        ;; Pipeline redirection
        (expect (grammar-redirection-test--find-op ops
                  :file "sorted.txt" :operation :write :source :redirection)
                :not :to-be nil)
        ;; Chain redirection
        (expect (grammar-redirection-test--find-op ops
                  :file "status.txt" :operation :write :source :redirection)
                :not :to-be nil)))

    (it "extracts multiple redirections on single command"
      (let* ((parsed (jf/bash-parse "cmd < input.txt > output.txt 2> errors.log"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        ;; Input redirection: < input.txt => :read
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "input.txt" :operation :read)
                :not :to-be nil)
        ;; Output redirection: > output.txt => :write
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "output.txt" :operation :write)
                :not :to-be nil)
        ;; Stderr redirection: 2> errors.log => :write
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "errors.log" :operation :write)
                :not :to-be nil))))

  ;; --- Variable path resolution in redirections ---

  (describe "variable path resolution"

    (it "resolves known variable in redirection path"
      (let* ((parsed (jf/bash-parse "echo data > $OUTFILE"))
             (ops (jf/bash-extract-operations-from-redirections
                   parsed '((OUTFILE . "/workspace/output.txt")))))
        (expect ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op ops
                    :operation :write)))
          (expect op :not :to-be nil)
          (expect (plist-get op :file) :to-equal "/workspace/output.txt"))))

    (it "preserves unresolved variable with metadata"
      (let* ((parsed (jf/bash-parse "echo data > $UNKNOWN_VAR"))
             (ops (jf/bash-extract-operations-from-redirections parsed nil)))
        (expect ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op ops
                    :operation :write)))
          (expect op :not :to-be nil)
          ;; Unresolved variables should be marked
          (expect (plist-get op :unresolved) :to-be t)
          (expect (plist-get op :unresolved-vars) :not :to-be nil))))

    (it "resolves variable in append redirection"
      (let* ((parsed (jf/bash-parse "echo data >> $LOGFILE"))
             (ops (jf/bash-extract-operations-from-redirections
                   parsed '((LOGFILE . "/var/log/app.log")))))
        (expect ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op ops
                    :operation :append)))
          (expect op :not :to-be nil)
          (expect (plist-get op :file) :to-equal "/var/log/app.log"))))

    (it "resolves variable in input redirection"
      (let* ((parsed (jf/bash-parse "sort < $INFILE"))
             (ops (jf/bash-extract-operations-from-redirections
                   parsed '((INFILE . "/data/input.csv")))))
        (expect ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op ops
                    :operation :read)))
          (expect op :not :to-be nil)
          (expect (plist-get op :file) :to-equal "/data/input.csv")))))

  ;; --- Non-filesystem command with redirection (KEY bug scenario) ---

  (describe "non-filesystem command with redirection"

    (it "echo hello > file.txt extracts :write for file.txt"
      ;; This is the KEY bug scenario: echo has no filesystem handler,
      ;; but the redirection IS a filesystem operation
      (let* ((parsed (jf/bash-parse "echo hello > file.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (let ((op (grammar-redirection-test--find-op fs-ops
                    :file "file.txt" :operation :write :source :redirection)))
          (expect op :not :to-be nil)
          (expect (plist-get op :command) :to-equal "echo"))))

    (it "printf output > file.txt extracts :write despite printf having no handler"
      (let* ((parsed (jf/bash-parse "printf '%s\\n' data > result.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "result.txt" :operation :write :source :redirection)
                :not :to-be nil)))

    (it "unknown_tool > output.dat extracts :write for grammar-level redirect"
      (let* ((parsed (jf/bash-parse "unknown_tool arg1 arg2 > output.dat"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "output.dat" :operation :write :source :redirection)
                :not :to-be nil)))

    (it "date > timestamp.txt extracts :write for non-file command"
      ;; date produces output but has no file semantics
      ;; Redirection should still be extracted
      (let* ((parsed (jf/bash-parse "date > timestamp.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "timestamp.txt" :operation :write)
                :not :to-be nil)))

    (it "echo in chain with redirections extracts all redirections"
      (let* ((parsed (jf/bash-parse "echo start > log.txt && echo error > err.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "log.txt" :operation :write)
                :not :to-be nil)
        (expect (grammar-redirection-test--find-op fs-ops
                  :file "err.txt" :operation :write)
                :not :to-be nil))))

  ;; --- Contract validation ---

  (describe "contract compliance"

    (it "all redirection-sourced operations satisfy file-op contract"
      (let* ((parsed (jf/bash-parse "cmd < in.txt > out.txt 2> err.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        ;; Validate all operations against the contract
        (contract-test--validate-file-ops fs-ops "redirection contract compliance")))

    (it "redirection operations from chain satisfy file-op contract"
      (let* ((parsed (jf/bash-parse "echo a > f1.txt && echo b >> f2.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (grammar-redirection-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (contract-test--validate-file-ops fs-ops "chain redirection contract compliance")))))

(provide 'grammar-redirection-spec)
;;; grammar-redirection-spec.el ends here
