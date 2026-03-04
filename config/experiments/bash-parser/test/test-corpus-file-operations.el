;;; test-corpus-file-operations.el --- Corpus-based tests for file operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Corpus-based ERT tests for file operations extraction with embedded test corpus.

(require 'ert)
(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory load-file-name)))

;;; Test Corpus Data

(defvar jf/bash-file-operations-test-corpus
  '(
    ;; ============================================================
    ;; SIMPLE READ OPERATIONS
    ;; ============================================================
    (:id "read-001"
     :command "cat /workspace/foo.txt"
     :note "Simple file read"
     :expect-ops ((:file "/workspace/foo.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "read-002"
     :command "grep pattern file.txt"
     :note "Grep reads file (skips pattern argument)"
     :expect-ops ((:file "file.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "read-003"
     :command "head -n 10 /tmp/log.txt"
     :note "Head reads file"
     :expect-ops ((:file "/tmp/log.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "read-004"
     :command "tail -f server.log"
     :note "Tail reads file"
     :expect-ops ((:file "server.log"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "read-005"
     :command "wc -l data.txt"
     :note "Word count reads file"
     :expect-ops ((:file "data.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "read-006"
     :command "less /var/log/system.log"
     :note "Less reads file"
     :expect-ops ((:file "/var/log/system.log"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; SIMPLE WRITE OPERATIONS
    ;; ============================================================
    (:id "write-001"
     :command "touch newfile.txt"
     :note "Touch creates or updates file"
     :expect-ops ((:file "newfile.txt"
                   :operation :create-or-modify
                   :confidence :high
                   :source :positional-arg)))

    (:id "write-002"
     :command "echo 'content' > output.txt"
     :note "Redirection writes to file"
     :expect-ops ((:file "output.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    (:id "write-003"
     :command "cat input.txt >> output.txt"
     :note "Append redirection and read input"
     :expect-ops ((:file "input.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "output.txt"
                   :operation :append
                   :confidence :high
                   :source :redirection)))

    (:id "write-004"
     :command "tee output.txt"
     :note "Tee writes to file and stdout"
     :expect-ops ((:file "output.txt"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; DELETE OPERATIONS
    ;; ============================================================
    (:id "delete-001"
     :command "rm temp.txt"
     :note "Simple file deletion"
     :expect-ops ((:file "temp.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)))

    (:id "delete-002"
     :command "rm -rf /tmp/test-dir"
     :note "Recursive deletion (dangerous)"
     :expect-ops ((:file "/tmp/test-dir"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)))

    (:id "delete-003"
     :command "rmdir empty-directory"
     :note "Remove empty directory"
     :expect-ops ((:file "empty-directory"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)))

    (:id "delete-004"
     :command "rm file1.txt file2.txt file3.txt"
     :note "Delete multiple files"
     :expect-ops ((:file "file1.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)
                  (:file "file2.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)
                  (:file "file3.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; COPY OPERATIONS (READ source, WRITE destination)
    ;; ============================================================
    (:id "copy-001"
     :command "cp source.txt dest.txt"
     :note "Copy file (read source, write dest)"
     :expect-ops ((:file "source.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "dest.txt"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    (:id "copy-002"
     :command "cp -r src-dir dest-dir"
     :note "Recursive copy"
     :expect-ops ((:file "src-dir"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "dest-dir"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    (:id "copy-003"
     :command "cp file1.txt file2.txt file3.txt target-dir/"
     :note "Copy multiple files to directory"
     :expect-ops ((:file "file1.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "file2.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "file3.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "target-dir/"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; MOVE OPERATIONS (DELETE source, WRITE destination)
    ;; ============================================================
    (:id "move-001"
     :command "mv old.txt new.txt"
     :note "Move/rename file"
     :expect-ops ((:file "old.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)
                  (:file "new.txt"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    (:id "move-002"
     :command "mv file.txt /tmp/"
     :note "Move file to directory"
     :expect-ops ((:file "file.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)
                  (:file "/tmp/"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; MODIFY OPERATIONS (in-place edits)
    ;; ============================================================
    (:id "modify-001"
     :command "chmod 644 file.txt"
     :note "Chmod modifies permissions"
     :expect-ops ((:file "file.txt"
                   :operation :modify
                   :confidence :high
                   :source :positional-arg)))

    (:id "modify-002"
     :command "chown user:group file.txt"
     :note "Chown modifies ownership"
     :expect-ops ((:file "file.txt"
                   :operation :modify
                   :confidence :high
                   :source :positional-arg)))

    (:id "modify-003"
     :command "sed -i 's/foo/bar/g' file.txt"
     :note "Sed with -i modifies in-place"
     :expect-ops ((:file "file.txt"
                   :operation :modify
                   :confidence :high
                   :source :positional-arg)))

    (:id "modify-004"
     :command "sed 's/foo/bar/g' file.txt"
     :note "Sed without -i only reads (outputs to stdout)"
     :expect-ops ((:file "file.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; REDIRECTION OPERATIONS
    ;; ============================================================
    (:id "redirect-001"
     :command "grep pattern < input.txt"
     :note "Input redirection reads file"
     :expect-ops ((:file "input.txt"
                   :operation :read
                   :confidence :high
                   :source :redirection)))

    (:id "redirect-002"
     :command "command 2> error.log"
     :note "Stderr redirection writes file"
     :expect-ops ((:file "error.log"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    (:id "redirect-003"
     :command "git log > /dev/null 2>&1"
     :note "Multiple redirections"
     :expect-ops ((:file "/dev/null"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    (:id "redirect-004"
     :command "cat input.txt > output.txt 2> error.txt"
     :note "Read input, write output, write error"
     :expect-ops ((:file "input.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "output.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)
                  (:file "error.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    ;; ============================================================
    ;; GLOB PATTERNS
    ;; ============================================================
    (:id "glob-001"
     :command "rm *.txt"
     :note "Glob pattern in delete"
     :expect-ops ((:file "*.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg
                   :pattern t)))

    (:id "glob-002"
     :command "cat config/**/*.json"
     :note "Recursive glob pattern"
     :expect-ops ((:file "config/**/*.json"
                   :operation :read
                   :confidence :high
                   :source :positional-arg
                   :pattern t)))

    (:id "glob-003"
     :command "cp *.el backup/"
     :note "Copy with glob source"
     :expect-ops ((:file "*.el"
                   :operation :read
                   :confidence :high
                   :source :positional-arg
                   :pattern t)
                  (:file "backup/"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    (:id "glob-004"
     :command "ls test.{txt,md,json}"
     :note "Brace expansion pattern"
     :expect-ops ())  ;; ls doesn't modify files

    ;; ============================================================
    ;; PIPELINES
    ;; ============================================================
    (:id "pipeline-001"
     :command "cat file.txt | grep pattern > output.txt"
     :note "Pipeline with read and write"
     :expect-ops ((:file "file.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "output.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    (:id "pipeline-002"
     :command "cat input.txt | sort | uniq > unique.txt"
     :note "Three-stage pipeline"
     :expect-ops ((:file "input.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "unique.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    ;; ============================================================
    ;; COMMAND CHAINS
    ;; ============================================================
    (:id "chain-001"
     :command "rm temp.txt && touch new.txt"
     :note "Delete then create in chain"
     :expect-ops ((:file "temp.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)
                  (:file "new.txt"
                   :operation :create-or-modify
                   :confidence :high
                   :source :positional-arg)))

    (:id "chain-002"
     :command "cat file1.txt > combined.txt; cat file2.txt >> combined.txt"
     :note "Overwrite then append chain"
     :expect-ops ((:file "file1.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "combined.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)
                  (:file "file2.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "combined.txt"
                   :operation :append
                   :confidence :high
                   :source :redirection)))

    (:id "chain-003"
     :command "cp source.txt backup.txt && rm source.txt"
     :note "Backup then delete"
     :expect-ops ((:file "source.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "backup.txt"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)
                  (:file "source.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; FIND -EXEC BLOCKS
    ;; ============================================================
    (:id "find-001"
     :command "find . -name '*.log' -exec rm {} \\;"
     :note "Find and delete matching files"
     :expect-ops ((:file "."
                   :operation :read-directory
                   :confidence :high
                   :source :positional-arg)
                  (:file "{}"
                   :operation :delete
                   :confidence :high
                   :source :exec-block)))

    (:id "find-002"
     :command "find /tmp -type f -exec cat {} \\; > output.txt"
     :note "Find, read files, redirect to output"
     :expect-ops ((:file "/tmp"
                   :operation :read-directory
                   :confidence :high
                   :source :positional-arg)
                  (:file "{}"
                   :operation :read
                   :confidence :high
                   :source :exec-block)
                  (:file "output.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    (:id "find-003"
     :command "find . -name '*.txt' -exec grep pattern {} \\; -exec echo {} \\;"
     :note "Find with multiple exec blocks"
     :expect-ops ((:file "."
                   :operation :read-directory
                   :confidence :high
                   :source :positional-arg)
                  (:file "{}"
                   :operation :read
                   :confidence :high
                   :source :exec-block)))

    ;; ============================================================
    ;; VARIABLES - SIMPLE RESOLUTION
    ;; ============================================================
    (:id "variable-001"
     :command "cat $FILE"
     :note "Simple variable reference (unresolved)"
     :var-context nil
     :expect-ops ((:file "$FILE"
                   :operation :read
                   :confidence :high
                   :source :positional-arg
                   :unresolved t
                   :unresolved-vars ("FILE"))))

    (:id "variable-002"
     :command "cat $FILE"
     :note "Simple variable reference (resolved)"
     :var-context (("FILE" . "/workspace/data.txt"))
     :expect-ops ((:file "/workspace/data.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "variable-003"
     :command "rm ${TEMP_DIR}/file.txt"
     :note "Variable with braces (unresolved)"
     :var-context nil
     :expect-ops ((:file "${TEMP_DIR}/file.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg
                   :unresolved t
                   :unresolved-vars ("TEMP_DIR"))))

    (:id "variable-004"
     :command "rm ${TEMP_DIR}/file.txt"
     :note "Variable with braces (resolved)"
     :var-context (("TEMP_DIR" . "/tmp"))
     :expect-ops ((:file "/tmp/file.txt"
                   :operation :delete
                   :confidence :high
                   :source :positional-arg)))

    (:id "variable-005"
     :command "cp $SRC/$FILE $DEST/"
     :note "Multiple variables (partially resolved)"
     :var-context (("SRC" . "/workspace"))
     :expect-ops ((:file "/workspace/$FILE"
                   :operation :read
                   :confidence :high
                   :source :positional-arg
                   :unresolved t
                   :unresolved-vars ("FILE"))
                  (:file "$DEST/"
                   :operation :write
                   :confidence :high
                   :source :positional-arg
                   :unresolved t
                   :unresolved-vars ("DEST"))))

    ;; ============================================================
    ;; VARIABLES - ASSIGNMENT TRACKING IN CHAINS
    ;; ============================================================
    (:id "variable-chain-001"
     :command "DIR=/tmp && cat $DIR/file.txt"
     :note "Variable assignment and usage in chain"
     :var-context nil
     :expect-ops ((:file "/tmp/file.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "variable-chain-002"
     :command "A=/foo && B=$A/bar && cat $B/file.txt"
     :note "Sequential variable resolution"
     :var-context nil
     :expect-ops ((:file "/foo/bar/file.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "variable-chain-003"
     :command "WORKSPACE=/workspace && cat $WORKSPACE/data.txt > $WORKSPACE/output.txt"
     :note "Variable used multiple times after assignment"
     :var-context nil
     :expect-ops ((:file "/workspace/data.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "/workspace/output.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    ;; ============================================================
    ;; GIT COMMANDS (subcommand-specific semantics)
    ;; ============================================================
    (:id "git-001"
     :command "git add file.txt"
     :note "Git add reads file"
     :expect-ops ((:file "file.txt"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "git-002"
     :command "git add src/*.el"
     :note "Git add with glob pattern"
     :expect-ops ((:file "src/*.el"
                   :operation :read
                   :confidence :high
                   :source :positional-arg
                   :pattern t)))

    (:id "git-003"
     :command "git checkout file.txt"
     :note "Git checkout modifies file"
     :expect-ops ((:file "file.txt"
                   :operation :modify
                   :confidence :high
                   :source :positional-arg)))

    (:id "git-004"
     :command "git log > changes.txt"
     :note "Git log with redirection"
     :expect-ops ((:file "changes.txt"
                   :operation :write
                   :confidence :high
                   :source :redirection)))

    ;; ============================================================
    ;; DIRECTORY OPERATIONS
    ;; ============================================================
    (:id "directory-001"
     :command "mkdir newdir"
     :note "Create directory"
     :expect-ops ((:file "newdir"
                   :operation :create
                   :confidence :high
                   :source :positional-arg)))

    (:id "directory-002"
     :command "mkdir -p /workspace/deep/nested/dir"
     :note "Create nested directories"
     :expect-ops ((:file "/workspace/deep/nested/dir"
                   :operation :create
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; ARCHIVE OPERATIONS
    ;; ============================================================
    (:id "archive-001"
     :command "tar -czf archive.tar.gz files/"
     :note "Create tar archive (reads source, writes archive)"
     :expect-ops ((:file "files/"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "archive.tar.gz"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    (:id "archive-002"
     :command "tar -xzf archive.tar.gz"
     :note "Extract tar archive (reads archive, writes files)"
     :expect-ops ((:file "archive.tar.gz"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)))

    (:id "archive-003"
     :command "zip -r backup.zip src/"
     :note "Create zip archive"
     :expect-ops ((:file "src/"
                   :operation :read
                   :confidence :high
                   :source :positional-arg)
                  (:file "backup.zip"
                   :operation :write
                   :confidence :high
                   :source :positional-arg)))

    ;; ============================================================
    ;; NO FILE OPERATIONS
    ;; ============================================================
    (:id "no-ops-001"
     :command "echo hello"
     :note "Echo with no file operations"
     :expect-ops ())

    (:id "no-ops-002"
     :command "ls -la"
     :note "Ls lists but doesn't modify"
     :expect-ops ())

    (:id "no-ops-003"
     :command "pwd"
     :note "Pwd prints directory"
     :expect-ops ())

    (:id "no-ops-004"
     :command "git status"
     :note "Git status reads repo but not user files"
     :expect-ops ())

    ))


;;; Helper Functions

(defun jf/test-file-op-matches-p (actual expected)
  "Return t if ACTUAL operation matches EXPECTED operation plist.

Compares all specified keys in EXPECTED against ACTUAL.
EXPECTED may omit keys for partial matching."
  (cl-every (lambda (key)
              (equal (plist-get actual key)
                     (plist-get expected key)))
            ;; Get all keys from expected plist
            (cl-loop for (key _val) on expected by #'cddr
                     collect key)))

(defun jf/test-find-matching-op (expected-op actual-ops)
  "Find operation in ACTUAL-OPS that matches EXPECTED-OP.

Returns the matching operation or nil if not found."
  (cl-find-if (lambda (actual-op)
                (jf/test-file-op-matches-p actual-op expected-op))
              actual-ops))

(defun jf/test-run-corpus-case (test-case)
  "Run a single file operations test case from corpus.

TEST-CASE is a plist with :id, :command, :expect-ops, and optional :var-context."
  (let* ((test-id (plist-get test-case :id))
         (command (plist-get test-case :command))
         (expected-ops (plist-get test-case :expect-ops))
         (var-context (plist-get test-case :var-context))
         (note (plist-get test-case :note))
         (parsed (jf/bash-parse command))
         (actual-ops (if var-context
                        (jf/bash-extract-file-operations parsed var-context)
                      (jf/bash-extract-file-operations parsed))))

    ;; Verify parsing succeeded
    (unless (plist-get parsed :success)
      (error "Test %s: parsing failed for command: %s" test-id command))

    ;; Check operation count matches
    (unless (= (length expected-ops) (length actual-ops))
      (error "Test %s (%s): expected %d operations, got %d\nExpected: %S\nActual: %S"
             test-id note
             (length expected-ops) (length actual-ops)
             expected-ops actual-ops))

    ;; Check each expected operation is present
    (dolist (expected-op expected-ops)
      (let ((matching-op (jf/test-find-matching-op expected-op actual-ops)))
        (unless matching-op
          (error "Test %s (%s): expected operation not found\nExpected: %S\nActual ops: %S"
                 test-id note expected-op actual-ops))))))

;;; Test Generation Macro

(defmacro jf/bash-test-define-corpus-tests (corpus-var test-prefix runner-fn)
  "Define individual ERT tests for each case in CORPUS-VAR.

TEST-PREFIX is a string used for test naming (e.g., \"test-corpus-\").
RUNNER-FN is the function symbol that executes the test case.

Each test case in the corpus must have :id and :note properties.
Generates one `ert-deftest' per corpus case at macro expansion time."
  (declare (indent 1))
  `(progn
     ,@(mapcar
        (lambda (test-case)
          (let* ((test-id (plist-get test-case :id))
                 (note (or (plist-get test-case :note) ""))
                 (test-name (intern (concat test-prefix test-id))))
            `(ert-deftest ,test-name ()
               ,(format "%s: %s" test-id note)
               (,runner-fn
                (seq-find (lambda (tc) (equal (plist-get tc :id) ,test-id))
                          ,corpus-var)))))
        (symbol-value corpus-var))))

;;; Generated Corpus Tests

;; This single macro call replaces 60 manual test definitions.
;; Each corpus case in jf/bash-file-operations-test-corpus becomes
;; an individual ERT test with name test-corpus-{id}.
(jf/bash-test-define-corpus-tests
  jf/bash-file-operations-test-corpus
  "test-corpus-"
  jf/test-run-corpus-case)
;;; Interactive Test Runner

(defun jf/file-operations-corpus-run-all ()
  "Run all file operations corpus tests and display results."
  (interactive)
  (ert-run-tests-interactively "test-corpus-"))

(defun jf/file-operations-corpus-test-command (command &optional var-context)
  "Interactively test file operations extraction for COMMAND.

Optional VAR-CONTEXT is an alist of variable bindings."
  (interactive "sCommand to parse: ")
  (let* ((parsed (jf/bash-parse command))
         (ops (if var-context
                 (jf/bash-extract-file-operations parsed var-context)
               (jf/bash-extract-file-operations parsed))))
    (with-current-buffer (get-buffer-create "*file-operations-test*")
      (erase-buffer)
      (insert (format "Command: %s\n\n" command))
      (when var-context
        (insert (format "Variable Context: %S\n\n" var-context)))
      (insert "Parse Result:\n")
      (insert (pp-to-string parsed))
      (insert "\n\nFile Operations:\n")
      (if ops
          (dolist (op ops)
            (insert (format "\n  File: %s\n" (plist-get op :file)))
            (insert (format "  Operation: %s\n" (plist-get op :operation)))
            (insert (format "  Confidence: %s\n" (plist-get op :confidence)))
            (insert (format "  Source: %s\n" (plist-get op :source)))
            (when (plist-get op :pattern)
              (insert "  Pattern: t\n"))
            (when (plist-get op :unresolved)
              (insert (format "  Unresolved: %S\n" (plist-get op :unresolved-vars)))))
        (insert "  (no file operations)\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'test-corpus-file-operations)
;;; test-corpus-file-operations.el ends here
