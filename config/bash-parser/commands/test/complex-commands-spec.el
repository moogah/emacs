;;; complex-commands-spec.el --- Tests for complex command handlers -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

;; Load contract validation helpers
(require 'contract-test-helpers
         (expand-file-name "contract-test-helpers.el"
                           (file-name-directory (or load-file-name buffer-file-name))))

;; Load command handler files via load-file (filenames don't match feature names)
(let ((commands-dir (expand-file-name "config/bash-parser/commands/" jf/emacs-dir)))
  (dolist (file '("git.el" "tar.el" "find.el" "ls.el" "docker.el" "go.el"))
    (load-file (expand-file-name file commands-dir))))

;; Save the handler table after loading so registry-spec resets don't affect us
(defvar complex-cmd-test--saved-handlers jf/bash-command-handlers
  "Saved reference to handler table with all command handlers registered.")

;;; Helper functions

(defun complex-test--get-ops (result)
  "Extract operations list from handler RESULT.
Validates each operation against the file-op contract."
  (let ((ops (plist-get result :operations)))
    (contract-test--validate-handler-result result "complex command handler")
    ops))

(defun complex-test--get-op (result index)
  "Get operation at INDEX from handler RESULT."
  (nth index (complex-test--get-ops result)))

(defun complex-test--op-file (op)
  "Get :file from operation OP."
  (plist-get op :file))

(defun complex-test--op-operation (op)
  "Get :operation from operation OP."
  (plist-get op :operation))

;;; Tests

(describe "Git command handler"

  (describe "registration"
    (it "is registered in the handler registry"
      (let ((jf/bash-command-handlers complex-cmd-test--saved-handlers))
        (expect (jf/bash-lookup-command-handlers "git") :not :to-be nil))))

  (describe "simple subcommands"
    (it "classifies git add files as :read"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("add" "file1.txt" "file2.txt")))))
        (expect (plist-get result :domain) :to-equal :filesystem)
        (expect (length (complex-test--get-ops result)) :to-equal 2)
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :read)
        (expect (complex-test--op-file (complex-test--get-op result 0)) :to-equal "file1.txt")))

    (it "classifies git diff files as :read"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("diff" "a.txt" "b.txt")))))
        (expect (length (complex-test--get-ops result)) :to-equal 2)
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :read)))

    (it "classifies git checkout files as :modify"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("checkout" "branch-name")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :modify)))

    (it "classifies git clean files as :delete"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("clean" "-fd")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :delete)))

    (it "classifies git rm files as :delete"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("rm" "old.txt")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :delete)))

    (it "classifies git show files as :read"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("show" "HEAD:file.txt")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :read)))

    (it "classifies git ls-files as :read"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("ls-files" "*.el")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :read)))

    (it "classifies git merge as :modify"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("merge" "feature")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :modify)))

    (it "classifies git pull as :modify"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("pull" "origin" "main")))))
        (expect (length (complex-test--get-ops result)) :to-equal 2)
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :modify)))

    (it "classifies git rebase as :modify"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("rebase" "main")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :modify)))

    (it "classifies git reset as :modify"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("reset" "HEAD~1")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :modify)))

    (it "classifies git restore as :modify"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("restore" "file.txt")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :modify))))

  (describe "indexed subcommands"
    (it "classifies git apply with index 0 as :read"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("apply" "patch.diff" "extra")))))
        (expect (length (complex-test--get-ops result)) :to-equal 1)
        (expect (complex-test--op-file (complex-test--get-op result 0)) :to-equal "patch.diff")
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :read)))

    (it "classifies git clone last arg as :write"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("clone" "https://repo.git" "/dest")))))
        (expect (length (complex-test--get-ops result)) :to-equal 1)
        (expect (complex-test--op-file (complex-test--get-op result 0)) :to-equal "/dest")
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :write))))

  (describe "git mv (multi-operation)"
    (it "classifies sources as :delete and dest as :write"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("mv" "old.txt" "new.txt")))))
        (expect (length (complex-test--get-ops result)) :to-equal 2)
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :delete)
        (expect (complex-test--op-file (complex-test--get-op result 0)) :to-equal "old.txt")
        (expect (complex-test--op-operation (complex-test--get-op result 1)) :to-equal :write)
        (expect (complex-test--op-file (complex-test--get-op result 1)) :to-equal "new.txt")))

    (it "handles multiple sources with mv"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("mv" "a.txt" "b.txt" "dir/")))))
        (expect (length (complex-test--get-ops result)) :to-equal 3)
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :delete)
        (expect (complex-test--op-operation (complex-test--get-op result 1)) :to-equal :delete)
        (expect (complex-test--op-operation (complex-test--get-op result 2)) :to-equal :write))))

  (describe "git worktree add"
    (it "classifies worktree add target as :create"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("worktree" "add" "/path/to/wt")))))
        (expect (length (complex-test--get-ops result)) :to-equal 1)
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :create)
        (expect (complex-test--op-file (complex-test--get-op result 0)) :to-equal "/path/to/wt"))))

  (describe "nil returns"
    (it "returns nil for unknown subcommand"
      (expect (jf/bash-command-git--filesystem-handler
               '(:command-name "git" :positional-args ("status")))
              :to-be nil))

    (it "returns nil when no subcommand given"
      (expect (jf/bash-command-git--filesystem-handler
               '(:command-name "git" :positional-args nil))
              :to-be nil))

    (it "returns nil for subcommand with no file args"
      (expect (jf/bash-command-git--filesystem-handler
               '(:command-name "git" :positional-args ("add")))
              :to-be nil)))

  (describe "result structure"
    (it "includes :claimed-token-ids nil"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("add" "f.txt")))))
        (expect (plist-get result :claimed-token-ids) :to-be nil)))

    (it "includes :metadata nil"
      (let ((result (jf/bash-command-git--filesystem-handler
                     '(:command-name "git" :positional-args ("add" "f.txt")))))
        (expect (plist-get result :metadata) :to-be nil)))))


(describe "Tar command handler"

  (describe "registration"
    (it "is registered in the handler registry"
      (let ((jf/bash-command-handlers complex-cmd-test--saved-handlers))
        (expect (jf/bash-lookup-command-handlers "tar") :not :to-be nil))))

  (describe "create mode (-c)"
    (it "classifies archive as :write and sources as :read"
      (let ((result (jf/bash-command-tar--filesystem-handler
                     '(:command-name "tar"
                       :args ("-czf" "archive.tar.gz" "src/")
                       :flags ("-czf")
                       :positional-args ("src/")))))
        (expect (plist-get result :domain) :to-equal :filesystem)
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 2)
          ;; Archive is :write
          (expect (complex-test--op-file (nth 0 ops)) :to-equal "archive.tar.gz")
          (expect (complex-test--op-operation (nth 0 ops)) :to-equal :write)
          ;; Source is :read
          (expect (complex-test--op-file (nth 1 ops)) :to-equal "src/")
          (expect (complex-test--op-operation (nth 1 ops)) :to-equal :read)))))

  (describe "extract mode (-x)"
    (it "classifies archive as :read and targets as :write"
      (let ((result (jf/bash-command-tar--filesystem-handler
                     '(:command-name "tar"
                       :args ("-xf" "archive.tar.gz" "file.txt")
                       :flags ("-xf")
                       :positional-args ("file.txt")))))
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 2)
          (expect (complex-test--op-operation (nth 0 ops)) :to-equal :read)
          (expect (complex-test--op-file (nth 0 ops)) :to-equal "archive.tar.gz")
          (expect (complex-test--op-operation (nth 1 ops)) :to-equal :write)))))

  (describe "list mode (-t)"
    (it "classifies archive as :read only"
      (let ((result (jf/bash-command-tar--filesystem-handler
                     '(:command-name "tar"
                       :args ("-tf" "archive.tar.gz")
                       :flags ("-tf")
                       :positional-args nil))))
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 1)
          (expect (complex-test--op-operation (nth 0 ops)) :to-equal :read)
          (expect (complex-test--op-file (nth 0 ops)) :to-equal "archive.tar.gz")))))

  (describe "nil returns"
    (it "returns nil when no mode flag present"
      (expect (jf/bash-command-tar--filesystem-handler
               '(:command-name "tar"
                 :args ("-f" "archive.tar.gz")
                 :flags ("-f")
                 :positional-args nil))
              :to-be nil))

    (it "returns nil when no archive file specified"
      (expect (jf/bash-command-tar--filesystem-handler
               '(:command-name "tar"
                 :args ("-c")
                 :flags ("-c")
                 :positional-args ("file.txt")))
              :to-be nil))))


(describe "Find command handler"

  (describe "registration"
    (it "is registered in the handler registry"
      (let ((jf/bash-command-handlers complex-cmd-test--saved-handlers))
        (expect (jf/bash-lookup-command-handlers "find") :not :to-be nil))))

  (describe "search directory"
    (it "classifies first positional arg as :read-directory"
      (let ((result (jf/bash-command-find--filesystem-handler
                     '(:command-name "find"
                       :positional-args ("/tmp")
                       :flags nil))))
        (expect (plist-get result :domain) :to-equal :filesystem)
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 1)
          (expect (complex-test--op-file (nth 0 ops)) :to-equal "/tmp")
          (expect (complex-test--op-operation (nth 0 ops)) :to-equal :read-directory)))))

  (describe "-name pattern"
    (it "classifies -name arg as :match-pattern"
      (let ((result (jf/bash-command-find--filesystem-handler
                     '(:command-name "find"
                       :positional-args ("." "*.el")
                       :flags ("-name")))))
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 2)
          (expect (complex-test--op-operation (nth 0 ops)) :to-equal :read-directory)
          (expect (complex-test--op-operation (nth 1 ops)) :to-equal :match-pattern)
          (expect (complex-test--op-file (nth 1 ops)) :to-equal "*.el"))))

    (it "handles -type flag before -name correctly"
      (let ((result (jf/bash-command-find--filesystem-handler
                     '(:command-name "find"
                       :positional-args ("/src" "f" "*.py")
                       :flags ("-type" "-name")))))
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 2)
          (expect (complex-test--op-file (nth 1 ops)) :to-equal "*.py")
          (expect (complex-test--op-operation (nth 1 ops)) :to-equal :match-pattern)))))

  (describe "nil returns"
    (it "returns nil with no positional args"
      (expect (jf/bash-command-find--filesystem-handler
               '(:command-name "find" :positional-args nil :flags nil))
              :to-be nil))))


(describe "Ls command handler"

  (describe "registration"
    (it "is registered in the handler registry"
      (let ((jf/bash-command-handlers complex-cmd-test--saved-handlers))
        (expect (jf/bash-lookup-command-handlers "ls") :not :to-be nil))))

  (describe "no args"
    (it "returns nil when no positional args"
      (expect (jf/bash-command-ls--filesystem-handler
               '(:command-name "ls" :positional-args nil))
              :to-be nil)))

  (describe "glob patterns"
    (it "classifies * glob as :match-pattern"
      (let ((result (jf/bash-command-ls--filesystem-handler
                     '(:command-name "ls" :positional-args ("*.txt")))))
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 1)
          (expect (complex-test--op-operation (nth 0 ops)) :to-equal :match-pattern)
          (expect (complex-test--op-file (nth 0 ops)) :to-equal "*.txt"))))

    (it "classifies ? glob as :match-pattern"
      (let ((result (jf/bash-command-ls--filesystem-handler
                     '(:command-name "ls" :positional-args ("file?.txt")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :match-pattern)))

    (it "classifies [] glob as :match-pattern"
      (let ((result (jf/bash-command-ls--filesystem-handler
                     '(:command-name "ls" :positional-args ("file[0-9].txt")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :match-pattern))))

  (describe "literal dot"
    (it "classifies . as :read-directory"
      (let ((result (jf/bash-command-ls--filesystem-handler
                     '(:command-name "ls" :positional-args (".")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :read-directory)
        (expect (complex-test--op-file (complex-test--get-op result 0)) :to-equal "."))))

  (describe "plain paths"
    (it "classifies plain path as :read-directory"
      (let ((result (jf/bash-command-ls--filesystem-handler
                     '(:command-name "ls" :positional-args ("somefile.txt")))))
        (expect result :not :to-be nil)
        (expect (plist-get (car (plist-get result :operations)) :operation)
                :to-equal :read-directory)
        (expect (plist-get (car (plist-get result :operations)) :file)
                :to-equal "somefile.txt")))))


(describe "Docker command handler"

  (describe "registration"
    (it "is registered in the handler registry"
      (let ((jf/bash-command-handlers complex-cmd-test--saved-handlers))
        (expect (jf/bash-lookup-command-handlers "docker") :not :to-be nil))))

  (describe "docker cp"
    (it "classifies source as :read and dest as :write"
      (let ((result (jf/bash-command-docker--filesystem-handler
                     '(:command-name "docker"
                       :positional-args ("cp" "/local/file.txt" "container:/remote/file.txt")))))
        (expect (plist-get result :domain) :to-equal :filesystem)
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 2)
          (expect (complex-test--op-file (nth 0 ops)) :to-equal "/local/file.txt")
          (expect (complex-test--op-operation (nth 0 ops)) :to-equal :read)
          (expect (complex-test--op-file (nth 1 ops)) :to-equal "container:/remote/file.txt")
          (expect (complex-test--op-operation (nth 1 ops)) :to-equal :write))))

    (it "includes container paths as-is"
      (let ((result (jf/bash-command-docker--filesystem-handler
                     '(:command-name "docker"
                       :positional-args ("cp" "mycontainer:/app/data" "/local/")))))
        (expect (complex-test--op-file (complex-test--get-op result 0)) :to-equal "mycontainer:/app/data"))))

  (describe "nil returns"
    (it "returns nil for non-cp subcommand"
      (expect (jf/bash-command-docker--filesystem-handler
               '(:command-name "docker" :positional-args ("run" "myimage")))
              :to-be nil))

    (it "returns nil for cp with insufficient args"
      (expect (jf/bash-command-docker--filesystem-handler
               '(:command-name "docker" :positional-args ("cp" "only-one")))
              :to-be nil))))


(describe "Go command handler"

  (describe "registration"
    (it "is registered in the handler registry"
      (let ((jf/bash-command-handlers complex-cmd-test--saved-handlers))
        (expect (jf/bash-lookup-command-handlers "go") :not :to-be nil))))

  (describe "go run"
    (it "classifies target as :execute"
      (let ((result (jf/bash-command-go--filesystem-handler
                     '(:command-name "go" :positional-args ("run" "main.go")))))
        (expect (plist-get result :domain) :to-equal :filesystem)
        (let ((ops (complex-test--get-ops result)))
          (expect (length ops) :to-equal 1)
          (expect (complex-test--op-file (nth 0 ops)) :to-equal "main.go")
          (expect (complex-test--op-operation (nth 0 ops)) :to-equal :execute)))))

  (describe "go test"
    (it "classifies target as :execute"
      (let ((result (jf/bash-command-go--filesystem-handler
                     '(:command-name "go" :positional-args ("test" "./...")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :execute)
        (expect (complex-test--op-file (complex-test--get-op result 0)) :to-equal "./..."))))

  (describe "go build"
    (it "classifies target as :read"
      (let ((result (jf/bash-command-go--filesystem-handler
                     '(:command-name "go" :positional-args ("build" "main.go")))))
        (expect (complex-test--op-operation (complex-test--get-op result 0)) :to-equal :read))))

  (describe "nil returns"
    (it "returns nil for unknown subcommand"
      (expect (jf/bash-command-go--filesystem-handler
               '(:command-name "go" :positional-args ("fmt" "./...")))
              :to-be nil))

    (it "returns nil when no target provided"
      (expect (jf/bash-command-go--filesystem-handler
               '(:command-name "go" :positional-args ("run")))
              :to-be nil))))


(describe "Registry integration"

  (describe "extract-command-semantics"
    (it "extracts git operations through the registry"
      (let* ((jf/bash-command-handlers complex-cmd-test--saved-handlers)
             (result (jf/bash-extract-command-semantics
                      '(:command-name "git" :positional-args ("add" "test.txt")))))
        (let ((fs-ops (alist-get :filesystem (plist-get result :domains))))
          (expect fs-ops :not :to-be nil)
          (expect (length fs-ops) :to-equal 1)
          (expect (plist-get (car fs-ops) :file) :to-equal "test.txt")
          (expect (plist-get (car fs-ops) :operation) :to-equal :read))))

    (it "extracts docker operations through the registry"
      (let* ((jf/bash-command-handlers complex-cmd-test--saved-handlers)
             (result (jf/bash-extract-command-semantics
                      '(:command-name "docker" :positional-args ("cp" "a.txt" "b.txt")))))
        (let ((fs-ops (alist-get :filesystem (plist-get result :domains))))
          (expect fs-ops :not :to-be nil)
          (expect (length fs-ops) :to-equal 2))))))

;;; complex-commands-spec.el ends here
