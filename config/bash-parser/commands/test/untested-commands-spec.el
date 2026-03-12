;;; untested-commands-spec.el --- Tests for 12 previously untested command handlers -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

;; Load contract validation helpers
(require 'contract-test-helpers
         (expand-file-name "contract-test-helpers.el"
                           (file-name-directory (or load-file-name buffer-file-name))))

;; Load all command handlers under test
(let ((commands-dir (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (cmd '("less" "wc" "chown" "chgrp" "mkdir" "rmdir" "touch" "exec" "ln" "bzip2" "bunzip2" "unzip"))
    (load-file (expand-file-name (concat cmd ".el") commands-dir))))

;; Save the handler table after loading so registry-spec resets don't affect us
(defvar untested-cmd-test--saved-handlers jf/bash-command-handlers
  "Saved reference to handler table with all command handlers registered.")

;;; Helper

(defun untested-cmd-test--extract-ops (command-name &rest plist-args)
  "Call the handler for COMMAND-NAME with PLIST-ARGS merged into parsed-command.
Returns the :operations list from the filesystem domain.
Validates each operation against the file-op contract."
  (let* ((parsed (append (list :command-name command-name) plist-args))
         ;; Temporarily ensure our saved handlers are active
         (jf/bash-command-handlers untested-cmd-test--saved-handlers)
         (result (jf/bash-extract-command-semantics parsed))
         (domains (plist-get result :domains))
         (ops (alist-get :filesystem domains)))
    (contract-test--validate-file-ops ops (format "%s handler" command-name))
    ops))

;;; Tests

(describe "Read Commands"

  (describe "less handler"
    (it "extracts :read for single file"
      (let ((ops (untested-cmd-test--extract-ops "less" :positional-args '("file.txt"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "file.txt")
        (expect (plist-get (car ops) :operation) :to-equal :read)))

    (it "extracts :read for multiple files"
      (let ((ops (untested-cmd-test--extract-ops "less" :positional-args '("a.txt" "b.txt" "c.txt"))))
        (expect (length ops) :to-equal 3)
        (expect (plist-get (nth 0 ops) :file) :to-equal "a.txt")
        (expect (plist-get (nth 1 ops) :file) :to-equal "b.txt")
        (expect (plist-get (nth 2 ops) :file) :to-equal "c.txt")))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "less" :positional-args nil)))
        (expect ops :to-be nil))))

  (describe "wc handler"
    (it "extracts :read for single file"
      (let ((ops (untested-cmd-test--extract-ops "wc" :positional-args '("data.txt"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "data.txt")
        (expect (plist-get (car ops) :operation) :to-equal :read)))

    (it "extracts :read for multiple files"
      (let ((ops (untested-cmd-test--extract-ops "wc" :positional-args '("a.txt" "b.txt"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "a.txt")
        (expect (plist-get (nth 1 ops) :file) :to-equal "b.txt")))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "wc" :positional-args nil)))
        (expect ops :to-be nil))))

  (describe "unzip handler"
    (it "extracts :read for archive file"
      (let ((ops (untested-cmd-test--extract-ops "unzip" :positional-args '("archive.zip"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "archive.zip")
        (expect (plist-get (car ops) :operation) :to-equal :read)))

    (it "extracts :read for multiple archives"
      (let ((ops (untested-cmd-test--extract-ops "unzip" :positional-args '("a.zip" "b.zip"))))
        (expect (length ops) :to-equal 2)))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "unzip" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Modify Commands"

  (describe "chown handler"
    (it "extracts :modify skipping owner arg"
      (let ((ops (untested-cmd-test--extract-ops "chown" :positional-args '("root:staff" "file.txt"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "file.txt")
        (expect (plist-get (car ops) :operation) :to-equal :modify)))

    (it "handles multiple files after owner"
      (let ((ops (untested-cmd-test--extract-ops "chown" :positional-args '("user:group" "a.txt" "b.txt"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "a.txt")
        (expect (plist-get (nth 1 ops) :file) :to-equal "b.txt")))

    (it "returns empty for owner-only (no files)"
      (let ((ops (untested-cmd-test--extract-ops "chown" :positional-args '("root"))))
        (expect ops :to-be nil)))

    (it "returns empty for no args"
      (let ((ops (untested-cmd-test--extract-ops "chown" :positional-args nil)))
        (expect ops :to-be nil))))

  (describe "chgrp handler"
    (it "extracts :modify skipping group arg"
      (let ((ops (untested-cmd-test--extract-ops "chgrp" :positional-args '("staff" "file.txt"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "file.txt")
        (expect (plist-get (car ops) :operation) :to-equal :modify)))

    (it "handles multiple files after group"
      (let ((ops (untested-cmd-test--extract-ops "chgrp" :positional-args '("admin" "a.sh" "b.sh" "c.sh"))))
        (expect (length ops) :to-equal 3)
        (expect (plist-get (nth 0 ops) :file) :to-equal "a.sh")
        (expect (plist-get (nth 1 ops) :file) :to-equal "b.sh")
        (expect (plist-get (nth 2 ops) :file) :to-equal "c.sh")))

    (it "returns empty for group-only (no files)"
      (let ((ops (untested-cmd-test--extract-ops "chgrp" :positional-args '("wheel"))))
        (expect ops :to-be nil)))

    (it "returns empty for no args"
      (let ((ops (untested-cmd-test--extract-ops "chgrp" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Create Commands"

  (describe "mkdir handler"
    (it "extracts :create for single directory"
      (let ((ops (untested-cmd-test--extract-ops "mkdir" :positional-args '("new-dir"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "new-dir")
        (expect (plist-get (car ops) :operation) :to-equal :create)))

    (it "extracts :create for multiple directories"
      (let ((ops (untested-cmd-test--extract-ops "mkdir" :positional-args '("dir1" "dir2" "dir3"))))
        (expect (length ops) :to-equal 3)
        (expect (plist-get (nth 0 ops) :file) :to-equal "dir1")
        (expect (plist-get (nth 1 ops) :file) :to-equal "dir2")
        (expect (plist-get (nth 2 ops) :file) :to-equal "dir3")))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "mkdir" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Delete Commands"

  (describe "rmdir handler"
    (it "extracts :delete for single directory"
      (let ((ops (untested-cmd-test--extract-ops "rmdir" :positional-args '("old-dir"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "old-dir")
        (expect (plist-get (car ops) :operation) :to-equal :delete)))

    (it "extracts :delete for multiple directories"
      (let ((ops (untested-cmd-test--extract-ops "rmdir" :positional-args '("tmp1" "tmp2"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "tmp1")
        (expect (plist-get (nth 1 ops) :file) :to-equal "tmp2")))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "rmdir" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Create-or-Modify Commands"

  (describe "touch handler"
    (it "extracts :create-or-modify for single file"
      (let ((ops (untested-cmd-test--extract-ops "touch" :positional-args '("newfile.txt"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "newfile.txt")
        (expect (plist-get (car ops) :operation) :to-equal :create-or-modify)))

    (it "extracts :create-or-modify for multiple files"
      (let ((ops (untested-cmd-test--extract-ops "touch" :positional-args '("a.txt" "b.txt"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :operation) :to-equal :create-or-modify)
        (expect (plist-get (nth 1 ops) :operation) :to-equal :create-or-modify)))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "touch" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Execute Commands"

  (describe "exec handler"
    (it "extracts :execute for first arg only"
      (let ((ops (untested-cmd-test--extract-ops "exec" :positional-args '("/usr/bin/python" "arg1" "arg2"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "/usr/bin/python")
        (expect (plist-get (car ops) :operation) :to-equal :execute)))

    (it "extracts :execute for single arg"
      (let ((ops (untested-cmd-test--extract-ops "exec" :positional-args '("bash"))))
        (expect (length ops) :to-equal 1)
        (expect (plist-get (car ops) :file) :to-equal "bash")
        (expect (plist-get (car ops) :operation) :to-equal :execute)))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "exec" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Multi-Operation Commands"

  (describe "ln handler"
    (it "extracts :read for source and :write for destination"
      (let ((ops (untested-cmd-test--extract-ops "ln" :positional-args '("target.txt" "link.txt"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "target.txt")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :file) :to-equal "link.txt")
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write)))

    (it "handles multiple sources"
      (let ((ops (untested-cmd-test--extract-ops "ln" :positional-args '("a.txt" "b.txt" "dest/"))))
        (expect (length ops) :to-equal 3)
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 2 ops) :file) :to-equal "dest/")
        (expect (plist-get (nth 2 ops) :operation) :to-equal :write)))

    (it "returns empty for single arg (no destination)"
      (let ((ops (untested-cmd-test--extract-ops "ln" :positional-args '("only.txt"))))
        (expect ops :to-be nil)))

    (it "returns empty for no args"
      (let ((ops (untested-cmd-test--extract-ops "ln" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Compression Commands"

  (describe "bzip2 handler"
    (it "extracts :read and :write with .bz2 suffix"
      (let ((ops (untested-cmd-test--extract-ops "bzip2" :positional-args '("data.txt"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "data.txt")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :file) :to-equal "data.txt.bz2")
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write)))

    (it "handles multiple files"
      (let ((ops (untested-cmd-test--extract-ops "bzip2" :positional-args '("a.log" "b.log"))))
        (expect (length ops) :to-equal 4)
        (expect (plist-get (nth 0 ops) :file) :to-equal "a.log")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :file) :to-equal "a.log.bz2")
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write)
        (expect (plist-get (nth 2 ops) :file) :to-equal "b.log")
        (expect (plist-get (nth 3 ops) :file) :to-equal "b.log.bz2")))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "bzip2" :positional-args nil)))
        (expect ops :to-be nil))))

  (describe "bunzip2 handler"
    (it "extracts :read and :write stripping .bz2 suffix"
      (let ((ops (untested-cmd-test--extract-ops "bunzip2" :positional-args '("data.txt.bz2"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "data.txt.bz2")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :file) :to-equal "data.txt")
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write)))

    (it "handles file without .bz2 suffix"
      (let ((ops (untested-cmd-test--extract-ops "bunzip2" :positional-args '("compressed"))))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (nth 0 ops) :file) :to-equal "compressed")
        (expect (plist-get (nth 0 ops) :operation) :to-equal :read)
        (expect (plist-get (nth 1 ops) :file) :to-equal "compressed")
        (expect (plist-get (nth 1 ops) :operation) :to-equal :write)))

    (it "handles multiple files"
      (let ((ops (untested-cmd-test--extract-ops "bunzip2" :positional-args '("a.bz2" "b.txt.bz2"))))
        (expect (length ops) :to-equal 4)
        (expect (plist-get (nth 0 ops) :file) :to-equal "a.bz2")
        (expect (plist-get (nth 1 ops) :file) :to-equal "a")
        (expect (plist-get (nth 2 ops) :file) :to-equal "b.txt.bz2")
        (expect (plist-get (nth 3 ops) :file) :to-equal "b.txt")))

    (it "returns empty operations for no args"
      (let ((ops (untested-cmd-test--extract-ops "bunzip2" :positional-args nil)))
        (expect ops :to-be nil)))))

(describe "Handler Registration"

  (it "registers less in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "less") :not :to-be nil)))

  (it "registers wc in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "wc") :not :to-be nil)))

  (it "registers chown in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "chown") :not :to-be nil)))

  (it "registers chgrp in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "chgrp") :not :to-be nil)))

  (it "registers mkdir in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "mkdir") :not :to-be nil)))

  (it "registers rmdir in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "rmdir") :not :to-be nil)))

  (it "registers touch in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "touch") :not :to-be nil)))

  (it "registers exec in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "exec") :not :to-be nil)))

  (it "registers ln in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "ln") :not :to-be nil)))

  (it "registers bzip2 in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "bzip2") :not :to-be nil)))

  (it "registers bunzip2 in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "bunzip2") :not :to-be nil)))

  (it "registers unzip in the handler registry"
    (let ((jf/bash-command-handlers untested-cmd-test--saved-handlers))
      (expect (jf/bash-lookup-command-handlers "unzip") :not :to-be nil))))

;;; untested-commands-spec.el ends here
