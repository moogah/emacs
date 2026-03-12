;;; flag-commands-spec.el --- Tests for flag-dependent command handlers -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

;; Load command handler files from parent directory
(let ((commands-dir (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name))))))
  (load-file (expand-file-name "sed.el" commands-dir))
  (load-file (expand-file-name "grep.el" commands-dir))
  (load-file (expand-file-name "interpreters.el" commands-dir))
  (load-file (expand-file-name "dd.el" commands-dir)))

;;; Helper

(defun flag-test--ops (result)
  "Extract operations list from handler RESULT."
  (plist-get result :operations))

(defun flag-test--first-op (result)
  "Extract first operation from handler RESULT."
  (car (flag-test--ops result)))

;;; sed

(describe "sed command handler"

  (describe "without -i flag"

    (it "returns :read for file args"
      (let ((result (jf/bash-command-sed--filesystem-handler
                     '(:command-name "sed" :positional-args ("s/foo/bar/" "file.txt") :flags ()))))
        (expect result :not :to-be nil)
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :read)
        (expect (plist-get (flag-test--first-op result) :file) :to-equal "file.txt")))

    (it "skips the sed expression (index 0)"
      (let* ((result (jf/bash-command-sed--filesystem-handler
                      '(:command-name "sed" :positional-args ("s/foo/bar/" "a.txt" "b.txt") :flags ())))
             (ops (flag-test--ops result)))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (car ops) :file) :to-equal "a.txt")
        (expect (plist-get (cadr ops) :file) :to-equal "b.txt")))

    (it "returns nil when only expression and no file args"
      (let ((result (jf/bash-command-sed--filesystem-handler
                     '(:command-name "sed" :positional-args ("s/foo/bar/") :flags ()))))
        (expect result :to-be nil))))

  (describe "with -i flag"

    (it "returns :modify for file args"
      (let ((result (jf/bash-command-sed--filesystem-handler
                     '(:command-name "sed" :positional-args ("s/foo/bar/" "file.txt") :flags ("-i")))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :modify)))

    (it "returns :modify with --in-place flag"
      (let ((result (jf/bash-command-sed--filesystem-handler
                     '(:command-name "sed" :positional-args ("s/foo/bar/" "file.txt") :flags ("--in-place")))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :modify))))

  (describe "result structure"

    (it "includes :domain :filesystem"
      (let ((result (jf/bash-command-sed--filesystem-handler
                     '(:command-name "sed" :positional-args ("s/foo/bar/" "file.txt") :flags ()))))
        (expect (plist-get result :domain) :to-equal :filesystem)))

    (it "includes :command in each operation"
      (let ((result (jf/bash-command-sed--filesystem-handler
                     '(:command-name "sed" :positional-args ("s/foo/bar/" "file.txt") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :command) :to-equal "sed")))

    (it "includes :confidence :high"
      (let ((result (jf/bash-command-sed--filesystem-handler
                     '(:command-name "sed" :positional-args ("s/foo/bar/" "file.txt") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :confidence) :to-equal :high)))))

;;; grep

(describe "grep command handler"

  (describe "without -l flag"

    (it "returns :read for file args"
      (let ((result (jf/bash-command-grep--filesystem-handler
                     '(:command-name "grep" :positional-args ("pattern" "file.txt") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :read)
        (expect (plist-get (flag-test--first-op result) :file) :to-equal "file.txt")))

    (it "skips the pattern (index 0)"
      (let* ((result (jf/bash-command-grep--filesystem-handler
                      '(:command-name "grep" :positional-args ("pattern" "a.txt" "b.txt") :flags ())))
             (ops (flag-test--ops result)))
        (expect (length ops) :to-equal 2))))

  (describe "with -l flag"

    (it "returns :match-pattern for file args"
      (let ((result (jf/bash-command-grep--filesystem-handler
                     '(:command-name "grep" :positional-args ("pattern" "file.txt") :flags ("-l")))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :match-pattern)))

    (it "returns :match-pattern with --files-with-matches flag"
      (let ((result (jf/bash-command-grep--filesystem-handler
                     '(:command-name "grep" :positional-args ("pattern" "file.txt") :flags ("--files-with-matches")))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :match-pattern))))

  (describe "returns nil when only pattern and no file args"

    (it "returns nil"
      (let ((result (jf/bash-command-grep--filesystem-handler
                     '(:command-name "grep" :positional-args ("pattern") :flags ()))))
        (expect result :to-be nil))))

  (describe "alias registration"

    (it "registers handler for grep"
      (expect (jf/bash-lookup-command-handlers "grep") :not :to-be nil))

    (it "registers handler for egrep"
      (expect (jf/bash-lookup-command-handlers "egrep") :not :to-be nil))

    (it "registers handler for fgrep"
      (expect (jf/bash-lookup-command-handlers "fgrep") :not :to-be nil)))

  (describe "preserves command name"

    (it "uses egrep as :command when invoked as egrep"
      (let ((result (jf/bash-command-grep--filesystem-handler
                     '(:command-name "egrep" :positional-args ("pattern" "file.txt") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :command) :to-equal "egrep")))))

;;; interpreters

(describe "interpreter command handler"

  (describe "without inline flags"

    (it "returns :execute for python script"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "python" :positional-args ("script.py") :flags ()))))
        (expect result :not :to-be nil)
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :execute)
        (expect (plist-get (flag-test--first-op result) :file) :to-equal "script.py")))

    (it "returns :execute for node script"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "node" :positional-args ("app.js") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :execute)))

    (it "returns :execute for bash script"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "bash" :positional-args ("setup.sh") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :execute)))

    (it "returns :execute for ruby script"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "ruby" :positional-args ("script.rb") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :execute)))

    (it "returns :execute for perl script"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "perl" :positional-args ("script.pl") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :execute))))

  (describe "with inline flags"

    (it "returns nil for python -c"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "python" :positional-args ("print('hello')") :flags ("-c")))))
        (expect result :to-be nil)))

    (it "returns nil for python -m"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "python" :positional-args ("json.tool") :flags ("-m")))))
        (expect result :to-be nil)))

    (it "returns nil for node -e"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "node" :positional-args ("console.log(1)") :flags ("-e")))))
        (expect result :to-be nil)))

    (it "returns nil for node --eval"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "node" :positional-args ("console.log(1)") :flags ("--eval")))))
        (expect result :to-be nil)))

    (it "returns nil for node -p"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "node" :positional-args ("1+1") :flags ("-p")))))
        (expect result :to-be nil)))

    (it "returns nil for node --print"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "node" :positional-args ("1+1") :flags ("--print")))))
        (expect result :to-be nil)))

    (it "returns nil for bash -c"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "bash" :positional-args ("echo hi") :flags ("-c")))))
        (expect result :to-be nil)))

    (it "returns nil for sh -c"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "sh" :positional-args ("echo hi") :flags ("-c")))))
        (expect result :to-be nil)))

    (it "returns nil for zsh -c"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "zsh" :positional-args ("echo hi") :flags ("-c")))))
        (expect result :to-be nil)))

    (it "returns nil for ruby -e"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "ruby" :positional-args ("puts 'hi'") :flags ("-e")))))
        (expect result :to-be nil)))

    (it "returns nil for perl -e"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "perl" :positional-args ("print 'hi'") :flags ("-e")))))
        (expect result :to-be nil)))

    (it "returns nil for perl -E"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "perl" :positional-args ("say 'hi'") :flags ("-E")))))
        (expect result :to-be nil)))

    (it "returns nil for php -r"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "php" :positional-args ("echo 'hi';") :flags ("-r")))))
        (expect result :to-be nil))))

  (describe "with no positional args and no inline flags"

    (it "returns nil"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "python" :positional-args nil :flags ()))))
        (expect result :to-be nil))))

  (describe "registration"

    (it "registers all interpreter commands"
      (dolist (cmd '("python" "python3" "node" "bash" "sh" "zsh" "ruby" "perl" "php"))
        (expect (jf/bash-lookup-command-handlers cmd) :not :to-be nil))))

  (describe "preserves command name"

    (it "uses python3 as :command when invoked as python3"
      (let ((result (jf/bash-command-interpreter--filesystem-handler
                     '(:command-name "python3" :positional-args ("script.py") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :command) :to-equal "python3")))))

;;; dd

(describe "dd command handler"

  (describe "if= argument"

    (it "extracts :read operation from if= arg"
      (let ((result (jf/bash-command-dd--filesystem-handler
                     '(:command-name "dd" :positional-args ("if=/dev/sda") :flags ()))))
        (expect result :not :to-be nil)
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :read)
        (expect (plist-get (flag-test--first-op result) :file) :to-equal "/dev/sda"))))

  (describe "of= argument"

    (it "extracts :write operation from of= arg"
      (let ((result (jf/bash-command-dd--filesystem-handler
                     '(:command-name "dd" :positional-args ("of=/dev/sdb") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :operation) :to-equal :write)
        (expect (plist-get (flag-test--first-op result) :file) :to-equal "/dev/sdb"))))

  (describe "both if= and of="

    (it "extracts both :read and :write operations"
      (let* ((result (jf/bash-command-dd--filesystem-handler
                      '(:command-name "dd" :positional-args ("if=/dev/sda" "of=disk.img" "bs=4M") :flags ())))
             (ops (flag-test--ops result)))
        (expect (length ops) :to-equal 2)
        (expect (plist-get (car ops) :operation) :to-equal :read)
        (expect (plist-get (car ops) :file) :to-equal "/dev/sda")
        (expect (plist-get (cadr ops) :operation) :to-equal :write)
        (expect (plist-get (cadr ops) :file) :to-equal "disk.img"))))

  (describe "no if= or of= args"

    (it "returns nil when no file args"
      (let ((result (jf/bash-command-dd--filesystem-handler
                     '(:command-name "dd" :positional-args ("bs=4M" "count=100") :flags ()))))
        (expect result :to-be nil)))

    (it "returns nil with empty positional args"
      (let ((result (jf/bash-command-dd--filesystem-handler
                     '(:command-name "dd" :positional-args nil :flags ()))))
        (expect result :to-be nil))))

  (describe "ignores empty values"

    (it "ignores if= with empty value"
      (let ((result (jf/bash-command-dd--filesystem-handler
                     '(:command-name "dd" :positional-args ("if=") :flags ()))))
        (expect result :to-be nil)))

    (it "ignores of= with empty value"
      (let ((result (jf/bash-command-dd--filesystem-handler
                     '(:command-name "dd" :positional-args ("of=") :flags ()))))
        (expect result :to-be nil))))

  (describe "result structure"

    (it "includes :command dd"
      (let ((result (jf/bash-command-dd--filesystem-handler
                     '(:command-name "dd" :positional-args ("if=/dev/sda") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :command) :to-equal "dd")))

    (it "includes :confidence :high"
      (let ((result (jf/bash-command-dd--filesystem-handler
                     '(:command-name "dd" :positional-args ("if=/dev/sda") :flags ()))))
        (expect (plist-get (flag-test--first-op result) :confidence) :to-equal :high)))

    (it "registers handler for dd"
      (expect (jf/bash-lookup-command-handlers "dd") :not :to-be nil))))

;;; flag-commands-spec.el ends here
