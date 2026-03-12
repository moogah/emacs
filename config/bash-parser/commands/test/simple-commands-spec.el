;;; simple-commands-spec.el --- Tests for simple command handlers -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

;; Load all command handlers under test
(let ((commands-dir (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (cmd '("cat" "rm" "cp" "chmod" "zip" "head" "tail" "tee" "mv" "gzip" "gunzip" "source"))
    (load-file (expand-file-name (concat cmd ".el") commands-dir))))

;; Save the handler table after loading so registry-spec resets don't affect us
(defvar simple-cmd-test--saved-handlers jf/bash-command-handlers
  "Saved reference to handler table with all command handlers registered.")

;;; Helper

(defun simple-cmd-test--extract-ops (command-name &rest plist-args)
  "Call the handler for COMMAND-NAME with PLIST-ARGS merged into parsed-command.
Returns the :operations list from the filesystem domain."
  (let* ((parsed (append (list :command-name command-name) plist-args))
         ;; Temporarily ensure our saved handlers are active
         (jf/bash-command-handlers simple-cmd-test--saved-handlers)
         (result (jf/bash-extract-command-semantics parsed))
         (domains (plist-get result :domains)))
    (alist-get :filesystem domains)))

;;; Tests

(describe "Simple Read Commands"

  (describe "cat handler"
    (it "extracts :read for single file"
      (let ((ops (simple-cmd-test--extract-ops "cat" :positional-args '("file.txt"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "file.txt")
        (expect (plist-get (car ops) :operation) :to-equal :read)))

    (it "extracts :read for multiple files"
      (let ((ops (simple-cmd-test--extract-ops "cat" :positional-args '("a.txt" "b.txt" "c.txt"))))
        (expect (length ops) :to-equal 3)
        (expect (plist-get (nth 0 ops) :file) :to-equal "a.txt")
        (expect (plist-get (nth 1 ops) :file) :to-equal "b.txt")
        (expect (plist-get (nth 2 ops) :file) :to-equal "c.txt")))

    (it "returns empty operations for no args"
      (let ((ops (simple-cmd-test--extract-ops "cat" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Head/Tail Commands"

  (describe "head handler"
    (it "extracts :read skipping -n flag arg"
      (let ((ops (simple-cmd-test--extract-ops "head"
                   :positional-args '("10" "file.txt")
                   :flags '("-n"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "file.txt")))

    (it "extracts :read for file without flags"
      (let ((ops (simple-cmd-test--extract-ops "head"
                   :positional-args '("file.txt")
                   :flags nil)))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "file.txt")))

    (it "skips -c flag arg too"
      (let ((ops (simple-cmd-test--extract-ops "head"
                   :positional-args '("100" "data.log")
                   :flags '("-c"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "data.log"))))

  (describe "tail handler"
    (it "extracts :read skipping -n flag arg"
      (let ((ops (simple-cmd-test--extract-ops "tail"
                   :positional-args '("20" "log.txt")
                   :flags '("-n"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "log.txt")))))

(describe "Delete Commands"

  (describe "rm handler"
    (it "extracts :delete for single file"
      (let ((ops (simple-cmd-test--extract-ops "rm" :positional-args '("temp.txt"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :operation) :to-equal :delete)
        (expect (plist-get (car ops) :file) :to-equal "temp.txt")))

    (it "extracts :delete for multiple files"
      (let ((ops (simple-cmd-test--extract-ops "rm" :positional-args '("a.tmp" "b.tmp"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :operation) :to-equal :delete)
        (expect (plist-get (nth 1 ops) :operation) :to-equal :delete)))))

(describe "Multi-Operation Commands"

  (describe "cp handler"
    (it "extracts :read for sources and :write for destination"
      (let ((ops (simple-cmd-test--extract-ops "cp" :positional-args '("src.txt" "dest.txt"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "src.txt")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :file) :to-equal "dest.txt")
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write)))

    (it "handles multiple sources"
      (let ((ops (simple-cmd-test--extract-ops "cp" :positional-args '("a.txt" "b.txt" "dest/"))))
        (expect (length ops) :to-equal 3)
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 2 ops) :file) :to-equal "dest/")
        (expect (plist-get (nth 2 ops) :operation) :to-equal :write)))

    (it "returns empty for single arg (no destination)"
      (let ((ops (simple-cmd-test--extract-ops "cp" :positional-args '("only.txt"))))
        (expect ops :to-be nil))))

  (describe "mv handler"
    (it "extracts :delete for sources and :write for destination"
      (let ((ops (simple-cmd-test--extract-ops "mv" :positional-args '("old.txt" "new.txt"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :operation) :to-equal :delete)
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write)))))

(describe "Modify Commands"

  (describe "chmod handler"
    (it "extracts :modify skipping mode arg"
      (let ((ops (simple-cmd-test--extract-ops "chmod" :positional-args '("755" "script.sh"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "script.sh")
        (expect (plist-get (car ops) :operation) :to-equal :modify)))

    (it "handles multiple files after mode"
      (let ((ops (simple-cmd-test--extract-ops "chmod" :positional-args '("+x" "a.sh" "b.sh"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "a.sh")
        (expect (plist-get (nth 1 ops) :file) :to-equal "b.sh")))

    (it "returns empty for mode-only (no files)"
      (let ((ops (simple-cmd-test--extract-ops "chmod" :positional-args '("644"))))
        (expect ops :to-be nil)))))

(describe "Zip/Compression Commands"

  (describe "zip handler"
    (it "extracts :write for zip file and :read for sources"
      (let ((ops (simple-cmd-test--extract-ops "zip" :positional-args '("archive.zip" "a.txt" "b.txt"))))
        (expect (length ops) :to-equal 3)
        (expect (plist-get (nth 0 ops) :file) :to-equal "archive.zip")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :write)
        (expect (plist-get (nth 1 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 2 ops) :operation) :to-equal :read))))

  (describe "gzip handler"
    (it "extracts :read and :write with .gz suffix"
      (let ((ops (simple-cmd-test--extract-ops "gzip" :positional-args '("data.txt"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "data.txt")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :file) :to-equal "data.txt.gz")
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write))))

  (describe "gunzip handler"
    (it "extracts :read and :write stripping .gz suffix"
      (let ((ops (simple-cmd-test--extract-ops "gunzip" :positional-args '("data.txt.gz"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "data.txt.gz")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :file) :to-equal "data.txt")
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write)))))

(describe "Tee Command"

  (it "extracts :write without append flag"
    (let ((ops (simple-cmd-test--extract-ops "tee" :positional-args '("output.txt") :flags nil)))
      (expect (length ops) :to-equal 1)
      (expect (plist-get (car ops) :operation) :to-equal :write)))

  (it "extracts :append with -a flag"
    (let ((ops (simple-cmd-test--extract-ops "tee" :positional-args '("output.txt") :flags '("-a"))))
      (expect (length ops) :to-equal 1)
      (expect (plist-get (car ops) :operation) :to-equal :append)))

  (it "extracts :append with --append flag"
    (let ((ops (simple-cmd-test--extract-ops "tee" :positional-args '("output.txt") :flags '("--append"))))
      (expect (length ops) :to-equal 1)
      (expect (plist-get (car ops) :operation) :to-equal :append))))

(describe "Execute Commands"

  (describe "source handler"
    (it "extracts :execute for source command"
      (let ((ops (simple-cmd-test--extract-ops "source" :positional-args '("setup.sh"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "setup.sh")
        (expect (plist-get (car ops) :operation) :to-equal :execute)))

    (it "extracts :execute for dot command"
      (let ((ops (simple-cmd-test--extract-ops "." :positional-args '("env.sh"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "env.sh")
        (expect (plist-get (car ops) :operation) :to-equal :execute)))))

;;; simple-commands-spec.el ends here
