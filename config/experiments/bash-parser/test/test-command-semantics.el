;;; test-command-semantics.el --- ERT tests for bash command semantics database -*- lexical-binding: t; -*-

;; Tests validate the command semantics database (jf/bash-command-file-semantics)
;; and lookup function (jf/bash-lookup-command-semantics).

;;; Code:

(require 'test-helper (expand-file-name "test-helper.el"
                                        (file-name-directory load-file-name)))

;;; Simple Read Commands

(ert-deftest test-semantics-lookup-simple-read-command ()
  "Scenario: bash-command-semantics § 'Lookup simple read command'"
  (let ((result (jf/bash-lookup-command-semantics "cat")))
    (should result)
    (let ((ops (plist-get result :operations)))
      (should (listp ops))
      (should (> (length ops) 0))
      (let ((op (car ops)))
        (should (eq (plist-get op :operation) :read))
        (should (eq (plist-get op :source) :positional-args))))))

(ert-deftest test-semantics-lookup-head-command ()
  "Scenario: bash-command-semantics § 'File reading commands' - head"
  (let ((result (jf/bash-lookup-command-semantics "head")))
    (should result)
    ;; head now uses custom handler for flag argument skipping
    (should (eq (plist-get result :operations) :custom))
    (should (eq (plist-get result :handler) 'jf/bash--extract-head-tail-operations))))

(ert-deftest test-semantics-lookup-tail-command ()
  "Scenario: bash-command-semantics § 'File reading commands' - tail"
  (let ((result (jf/bash-lookup-command-semantics "tail")))
    (should result)
    ;; tail now uses custom handler for flag argument skipping
    (should (eq (plist-get result :operations) :custom))
    (should (eq (plist-get result :handler) 'jf/bash--extract-head-tail-operations))))

(ert-deftest test-semantics-lookup-less-command ()
  "Scenario: bash-command-semantics § 'File reading commands' - less"
  (let* ((result (jf/bash-lookup-command-semantics "less"))
         (ops (plist-get result :operations)))
    (should result)
    (should (= (length ops) 1))
    (should (eq (plist-get (car ops) :operation) :read))))

(ert-deftest test-semantics-lookup-wc-command ()
  "Scenario: bash-command-semantics § 'File reading commands' - wc"
  (let* ((result (jf/bash-lookup-command-semantics "wc"))
         (ops (plist-get result :operations)))
    (should result)
    (should (= (length ops) 1))
    (should (eq (plist-get (car ops) :operation) :read))))

;;; Commands with Skip Rules

(ert-deftest test-semantics-lookup-command-with-skip-rules ()
  "Scenario: bash-command-semantics § 'Lookup command with skip rules'"
  (let* ((result (jf/bash-lookup-command-semantics "grep"))
         (ops (plist-get result :operations)))
    (should result)
    ;; grep now uses flag-dependent operations
    (should (eq ops :flag-dependent))
    ;; Check the default handler (no flags)
    (let* ((flag-handlers (plist-get result :flag-handlers))
           (default-handler (cdr (assoc '() flag-handlers)))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :read))
      (should (equal (plist-get op :skip-indices) '(0))))))

(ert-deftest test-semantics-lookup-egrep-skip-rules ()
  "Scenario: bash-command-semantics § 'Lookup command with skip rules' - egrep variant"
  (let* ((result (jf/bash-lookup-command-semantics "egrep"))
         (ops (plist-get result :operations)))
    (should result)
    ;; egrep now uses flag-dependent operations
    (should (eq ops :flag-dependent))
    ;; Check the default handler (no flags)
    (let* ((flag-handlers (plist-get result :flag-handlers))
           (default-handler (cdr (assoc '() flag-handlers)))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :read))
      (should (equal (plist-get op :skip-indices) '(0))))))

(ert-deftest test-semantics-lookup-chmod-skip-rules ()
  "Scenario: bash-command-semantics § 'File manipulation commands' - chmod with skip"
  (let* ((result (jf/bash-lookup-command-semantics "chmod"))
         (ops (plist-get result :operations))
         (op (car ops)))
    (should result)
    (should (eq (plist-get op :operation) :modify))
    (should (equal (plist-get op :skip-indices) '(0)))))

(ert-deftest test-semantics-lookup-chown-skip-rules ()
  "Scenario: bash-command-semantics § 'File manipulation commands' - chown with skip"
  (let* ((result (jf/bash-lookup-command-semantics "chown"))
         (ops (plist-get result :operations))
         (op (car ops)))
    (should result)
    (should (eq (plist-get op :operation) :modify))
    (should (equal (plist-get op :skip-indices) '(0)))))

;;; Unknown Commands

(ert-deftest test-semantics-lookup-unknown-command ()
  "Scenario: bash-command-semantics § 'Lookup unknown command'"
  (let ((result (jf/bash-lookup-command-semantics "nonexistent-command-xyz")))
    (should-not result)))

;;; Multi-operation Commands

(ert-deftest test-semantics-multi-operation-cp ()
  "Scenario: bash-command-semantics § 'Copy command with read and write operations'"
  (let* ((result (jf/bash-lookup-command-semantics "cp"))
         (ops (plist-get result :operations)))
    (should (= (length ops) 2))
    ;; First op: read sources (indices 0 to N-2)
    (let ((read-op (nth 0 ops)))
      (should (eq (plist-get read-op :operation) :read))
      (should (eq (plist-get read-op :source) :positional-args))
      (should (equal (plist-get read-op :indices) '(0 . -2))))
    ;; Second op: write destination (index -1)
    (let ((write-op (nth 1 ops)))
      (should (eq (plist-get write-op :operation) :write))
      (should (eq (plist-get write-op :source) :positional-args))
      (should (= (plist-get write-op :index) -1)))))

(ert-deftest test-semantics-multi-operation-mv ()
  "Scenario: bash-command-semantics § 'Move command with delete and write operations'"
  (let* ((result (jf/bash-lookup-command-semantics "mv"))
         (ops (plist-get result :operations)))
    (should (= (length ops) 2))
    ;; First op: delete sources
    (let ((delete-op (nth 0 ops)))
      (should (eq (plist-get delete-op :operation) :delete))
      (should (equal (plist-get delete-op :indices) '(0 . -2))))
    ;; Second op: write destination
    (let ((write-op (nth 1 ops)))
      (should (eq (plist-get write-op :operation) :write))
      (should (= (plist-get write-op :index) -1)))))

(ert-deftest test-semantics-multi-operation-ln ()
  "Scenario: bash-command-semantics § 'File manipulation commands' - ln multi-op"
  (let* ((result (jf/bash-lookup-command-semantics "ln"))
         (ops (plist-get result :operations)))
    (should (= (length ops) 2))
    ;; First op: read sources
    (should (eq (plist-get (nth 0 ops) :operation) :read))
    ;; Second op: write destination
    (should (eq (plist-get (nth 1 ops) :operation) :write))))

;;; Subcommand-based Commands

(ert-deftest test-semantics-git-add-subcommand ()
  "Scenario: bash-command-semantics § 'Git add subcommand'"
  (let ((result (jf/bash-lookup-command-semantics "git")))
    (should result)
    (should (eq (plist-get result :operations) :complex))
    (let* ((handlers (plist-get result :subcommand-handlers))
           (add-spec (alist-get 'add handlers)))
      (should add-spec)
      (should (= (length add-spec) 1))
      (should (eq (plist-get (car add-spec) :operation) :read)))))

(ert-deftest test-semantics-git-checkout-subcommand ()
  "Scenario: bash-command-semantics § 'Git checkout subcommand'"
  (let ((result (jf/bash-lookup-command-semantics "git")))
    (should result)
    (should (eq (plist-get result :operations) :complex))
    (let* ((handlers (plist-get result :subcommand-handlers))
           (checkout-spec (alist-get 'checkout handlers)))
      (should checkout-spec)
      (should (= (length checkout-spec) 1))
      (should (eq (plist-get (car checkout-spec) :operation) :modify)))))

(ert-deftest test-semantics-git-rm-subcommand ()
  "Scenario: bash-command-semantics § 'Subcommand-specific semantics' - git rm"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (rm-spec (alist-get 'rm handlers)))
    (should rm-spec)
    (should (eq (plist-get (car rm-spec) :operation) :delete))))

(ert-deftest test-semantics-git-diff-subcommand ()
  "Scenario: bash-command-semantics § 'Subcommand-specific semantics' - git diff"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (diff-spec (alist-get 'diff handlers)))
    (should diff-spec)
    (should (eq (plist-get (car diff-spec) :operation) :read))))

(ert-deftest test-semantics-git-clean-subcommand ()
  "Scenario: bash-command-semantics § 'Git clean subcommand'"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (clean-spec (alist-get 'clean handlers)))
    (should clean-spec)
    (should (eq (plist-get (car clean-spec) :operation) :delete))))

(ert-deftest test-semantics-git-apply-subcommand ()
  "Scenario: bash-command-semantics § 'Git apply subcommand'"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (apply-spec (alist-get 'apply handlers)))
    (should apply-spec)
    (should (eq (plist-get (car apply-spec) :operation) :read))))

(ert-deftest test-semantics-git-clone-subcommand ()
  "Scenario: bash-command-semantics § 'Git clone subcommand'"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (clone-spec (alist-get 'clone handlers)))
    (should clone-spec)
    (should (eq (plist-get (car clone-spec) :operation) :write))))

(ert-deftest test-semantics-git-worktree-subcommand ()
  "Scenario: bash-command-semantics § 'Git worktree subcommand'"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (worktree-spec (alist-get 'worktree handlers)))
    (should worktree-spec)
    (should (eq (plist-get worktree-spec :operations) :complex))
    (let* ((wt-handlers (plist-get worktree-spec :subcommand-handlers))
           (add-spec (alist-get 'add wt-handlers)))
      (should add-spec)
      (should (eq (plist-get (car add-spec) :operation) :create)))))

(ert-deftest test-semantics-git-merge-subcommand ()
  "Scenario: bash-command-semantics § 'Git merge subcommand'"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (merge-spec (alist-get 'merge handlers)))
    (should merge-spec)
    (should (eq (plist-get (car merge-spec) :operation) :modify))))

(ert-deftest test-semantics-git-pull-subcommand ()
  "Scenario: bash-command-semantics § 'Git pull subcommand'"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (pull-spec (alist-get 'pull handlers)))
    (should pull-spec)
    (should (eq (plist-get (car pull-spec) :operation) :modify))))

(ert-deftest test-semantics-git-rebase-subcommand ()
  "Scenario: bash-command-semantics § 'Git rebase subcommand'"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (rebase-spec (alist-get 'rebase handlers)))
    (should rebase-spec)
    (should (eq (plist-get (car rebase-spec) :operation) :modify))))

(ert-deftest test-semantics-git-reset-subcommand ()
  "Scenario: bash-command-semantics § 'Git reset subcommand'"
  (let* ((result (jf/bash-lookup-command-semantics "git"))
         (handlers (plist-get result :subcommand-handlers))
         (reset-spec (alist-get 'reset handlers)))
    (should reset-spec)
    (should (eq (plist-get (car reset-spec) :operation) :modify))))

(ert-deftest test-semantics-docker-cp-subcommand ()
  "Scenario: bash-command-semantics § 'Subcommand-specific semantics' - docker cp"
  (let* ((result (jf/bash-lookup-command-semantics "docker"))
         (handlers (plist-get result :subcommand-handlers))
         (cp-spec (alist-get 'cp handlers)))
    (should cp-spec)
    (should (= (length cp-spec) 2))
    (should (eq (plist-get (nth 0 cp-spec) :operation) :read))
    (should (eq (plist-get (nth 1 cp-spec) :operation) :write))))

;;; Operation Type Classification

(ert-deftest test-semantics-read-operation-classification ()
  "Scenario: bash-command-semantics § 'Read operation classification'"
  (let ((result (jf/bash-lookup-command-semantics "cat")))
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :read))))

(ert-deftest test-semantics-write-operation-classification ()
  "Scenario: bash-command-semantics § 'Write operation classification'"
  (let* ((result (jf/bash-lookup-command-semantics "tee"))
         (flag-handlers (plist-get result :flag-handlers))
         (default-spec (alist-get nil flag-handlers))
         (op (car default-spec)))
    (should (eq (plist-get op :operation) :write))))

(ert-deftest test-semantics-delete-operation-classification ()
  "Scenario: bash-command-semantics § 'Delete operation classification'"
  (let ((result (jf/bash-lookup-command-semantics "rm")))
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :delete))))

(ert-deftest test-semantics-modify-operation-classification ()
  "Scenario: bash-command-semantics § 'Modify operation classification'"
  (let ((result (jf/bash-lookup-command-semantics "chmod")))
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :modify))))

(ert-deftest test-semantics-create-operation-classification ()
  "Scenario: bash-command-semantics § 'Operation type classification' - create"
  (let ((result (jf/bash-lookup-command-semantics "mkdir")))
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :create))))

(ert-deftest test-semantics-create-or-modify-operation-classification ()
  "Scenario: bash-command-semantics § 'Operation type classification' - create-or-modify"
  (let ((result (jf/bash-lookup-command-semantics "touch")))
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :create-or-modify))))

(ert-deftest test-semantics-execute-operation-classification ()
  "Scenario: bash-command-semantics § 'Operation type classification' - execute"
  (let* ((result (jf/bash-lookup-command-semantics "python"))
         (ops-spec (plist-get result :operations)))
    ;; Python is now flag-dependent
    (should (eq ops-spec :flag-dependent))
    (let* ((flag-handlers (plist-get result :flag-handlers))
           ;; Get the default handler (nil trigger, last in list)
           (default-handler (cdr (car (last flag-handlers))))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :execute)))))

;;; Core Command Coverage

(ert-deftest test-semantics-core-command-touch ()
  "Scenario: bash-command-semantics § 'File writing commands' - touch"
  (let ((result (jf/bash-lookup-command-semantics "touch")))
    (should result)
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :create-or-modify))))

(ert-deftest test-semantics-core-command-mkdir ()
  "Scenario: bash-command-semantics § 'Core command coverage' - mkdir"
  (let ((result (jf/bash-lookup-command-semantics "mkdir")))
    (should result)
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :create))))

(ert-deftest test-semantics-core-command-rmdir ()
  "Scenario: bash-command-semantics § 'File deletion commands' - rmdir"
  (let ((result (jf/bash-lookup-command-semantics "rmdir")))
    (should result)
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :delete))))

(ert-deftest test-semantics-core-command-find ()
  "Scenario: bash-command-semantics § 'Core command coverage' - find"
  (let ((result (jf/bash-lookup-command-semantics "find")))
    (should result)
    ;; find now uses custom handler, check that it's properly configured
    (should (eq (plist-get result :operations) :custom))
    (should (eq (plist-get result :handler) 'jf/bash--extract-find-operations))))

(ert-deftest test-semantics-core-command-tee ()
  "Scenario: bash-command-semantics § 'File writing commands' - tee"
  (let ((result (jf/bash-lookup-command-semantics "tee")))
    (should result)
    ;; tee now uses flag-dependent operations
    (should (eq (plist-get result :operations) :flag-dependent))))

(ert-deftest test-semantics-tee-without-flags ()
  "Scenario: bash-command-semantics § 'Tee without flags returns write operation'"
  (let* ((result (jf/bash-lookup-command-semantics "tee"))
         (flag-handlers (plist-get result :flag-handlers))
         (default-handler (cdr (assoc '() flag-handlers)))
         (op (car default-handler)))
    (should (eq (plist-get op :operation) :write))
    (should (eq (plist-get op :source) :positional-args))))

(ert-deftest test-semantics-tee-with-append-flag ()
  "Scenario: bash-command-semantics § 'Tee with -a flag returns append operation'"
  (let* ((result (jf/bash-lookup-command-semantics "tee"))
         (flag-handlers (plist-get result :flag-handlers))
         (append-spec (alist-get '("-a" "--append") flag-handlers nil nil #'equal))
         (op (car append-spec)))
    (should append-spec)
    (should (eq (plist-get op :operation) :append))
    (should (eq (plist-get op :source) :positional-args))))

(ert-deftest test-semantics-tee-with-append-long-flag ()
  "Scenario: bash-command-semantics § 'Tee with --append flag returns append operation'"
  (let* ((result (jf/bash-lookup-command-semantics "tee"))
         (flag-handlers (plist-get result :flag-handlers))
         ;; The handler is the same for both -a and --append
         (append-spec (alist-get '("-a" "--append") flag-handlers nil nil #'equal))
         (op (car append-spec)))
    (should append-spec)
    (should (eq (plist-get op :operation) :append))
    (should (eq (plist-get op :source) :positional-args))))

(ert-deftest test-semantics-core-command-chgrp ()
  "Scenario: bash-command-semantics § 'File manipulation commands' - chgrp"
  (let ((result (jf/bash-lookup-command-semantics "chgrp")))
    (should result)
    (should (eq (plist-get (car (plist-get result :operations)) :operation) :modify))))

;;; Positional Argument Indexing

(ert-deftest test-semantics-single-index-specification ()
  "Scenario: bash-command-semantics § 'Single index specification'"
  (let* ((result (jf/bash-lookup-command-semantics "cp"))
         (ops (plist-get result :operations))
         (write-op (nth 1 ops)))
    (should (= (plist-get write-op :index) -1))))

(ert-deftest test-semantics-last-argument-specification ()
  "Scenario: bash-command-semantics § 'Last argument specification'"
  (let* ((result (jf/bash-lookup-command-semantics "mv"))
         (ops (plist-get result :operations))
         (write-op (nth 1 ops)))
    (should (= (plist-get write-op :index) -1))))

(ert-deftest test-semantics-range-specification ()
  "Scenario: bash-command-semantics § 'Range specification'"
  (let* ((result (jf/bash-lookup-command-semantics "cp"))
         (ops (plist-get result :operations))
         (read-op (nth 0 ops)))
    (should (equal (plist-get read-op :indices) '(0 . -2)))))

;;; Flag-dependent Operations

(ert-deftest test-semantics-flag-dependent-sed ()
  "Scenario: bash-command-semantics § 'Flag-dependent operations' - sed"
  (let ((result (jf/bash-lookup-command-semantics "sed")))
    (should result)
    (should (eq (plist-get result :operations) :flag-dependent))
    (let ((handlers (plist-get result :flag-handlers)))
      (should handlers)
      ;; Check in-place flag handler
      (let ((in-place-spec (alist-get '("-i" "--in-place") handlers nil nil #'equal)))
        (should in-place-spec)
        (should (eq (plist-get (car in-place-spec) :operation) :modify)))
      ;; Check default handler (no flags)
      (let ((default-spec (alist-get nil handlers)))
        (should default-spec)
        (should (eq (plist-get (car default-spec) :operation) :read))))))

(ert-deftest test-semantics-flag-dependent-tar ()
  "Scenario: bash-command-semantics § 'Custom handler operations' - tar"
  (let ((result (jf/bash-lookup-command-semantics "tar")))
    (should result)
    (should (eq (plist-get result :operations) :custom))
    (let ((handler (plist-get result :handler)))
      (should handler)
      (should (eq handler 'jf/bash--extract-tar-operations))
      (should (fboundp handler)))))

;;; Named Arguments (dd-style)

(ert-deftest test-semantics-named-args-dd ()
  "Scenario: bash-command-semantics § 'Core command coverage' - dd named args"
  (let* ((result (jf/bash-lookup-command-semantics "dd"))
         (ops (plist-get result :operations)))
    (should (= (length ops) 2))
    ;; First op: read from if=
    (let ((read-op (nth 0 ops)))
      (should (eq (plist-get read-op :operation) :read))
      (should (eq (plist-get read-op :source) :named-args))
      (should (equal (plist-get read-op :names) '("if"))))
    ;; Second op: write to of=
    (let ((write-op (nth 1 ops)))
      (should (eq (plist-get write-op :operation) :write))
      (should (eq (plist-get write-op :source) :named-args))
      (should (equal (plist-get write-op :names) '("of"))))))

;;; Script Execution Commands

(ert-deftest test-semantics-python-interpreter ()
  "Scenario: bash-command-semantics § 'Script execution' - python interpreter"
  (let* ((result (jf/bash-lookup-command-semantics "python"))
         (ops-spec (plist-get result :operations)))
    (should result)
    ;; Python is now flag-dependent to handle -c flag
    (should (eq ops-spec :flag-dependent))
    (let* ((flag-handlers (plist-get result :flag-handlers))
           ;; Get the default handler (nil trigger, last in list)
           (default-handler (cdr (car (last flag-handlers))))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :execute))
      (should (eq (plist-get op :source) :positional-args))
      (should (= (plist-get op :index) 0)))))

(ert-deftest test-semantics-python3-interpreter ()
  "Scenario: bash-command-semantics § 'Script execution' - python3 interpreter"
  (let* ((result (jf/bash-lookup-command-semantics "python3"))
         (ops-spec (plist-get result :operations)))
    (should result)
    ;; Python3 is now flag-dependent to handle -c flag
    (should (eq ops-spec :flag-dependent))
    (let* ((flag-handlers (plist-get result :flag-handlers))
           ;; Get the default handler (nil trigger, last in list)
           (default-handler (cdr (car (last flag-handlers))))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :execute))
      (should (= (plist-get op :index) 0)))))

(ert-deftest test-semantics-node-interpreter ()
  "Scenario: bash-command-semantics § 'Script execution' - node interpreter"
  (let* ((result (jf/bash-lookup-command-semantics "node"))
         (ops-spec (plist-get result :operations)))
    (should result)
    ;; Node is now flag-dependent to handle -e flag
    (should (eq ops-spec :flag-dependent))
    (let* ((flag-handlers (plist-get result :flag-handlers))
           ;; Get the default handler (nil trigger, last in list)
           (default-handler (cdr (car (last flag-handlers))))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :execute))
      (should (= (plist-get op :index) 0)))))

(ert-deftest test-semantics-bash-interpreter ()
  "Scenario: bash-command-semantics § 'Script execution' - bash interpreter"
  (let* ((result (jf/bash-lookup-command-semantics "bash"))
         (ops-spec (plist-get result :operations)))
    (should result)
    ;; Bash is now flag-dependent to handle -c flag
    (should (eq ops-spec :flag-dependent))
    (let* ((flag-handlers (plist-get result :flag-handlers))
           ;; Get the default handler (nil trigger, last in list)
           (default-handler (cdr (car (last flag-handlers))))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :execute))
      (should (= (plist-get op :index) 0)))))

(ert-deftest test-semantics-sh-interpreter ()
  "Scenario: bash-command-semantics § 'Script execution' - sh interpreter"
  (let* ((result (jf/bash-lookup-command-semantics "sh"))
         (ops-spec (plist-get result :operations)))
    (should result)
    ;; Sh is now flag-dependent to handle -c flag
    (should (eq ops-spec :flag-dependent))
    (let* ((flag-handlers (plist-get result :flag-handlers))
           ;; Get the default handler (nil trigger, last in list)
           (default-handler (cdr (car (last flag-handlers))))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :execute)))))

(ert-deftest test-semantics-zsh-interpreter ()
  "Scenario: bash-command-semantics § 'Script execution' - zsh interpreter"
  (let* ((result (jf/bash-lookup-command-semantics "zsh"))
         (ops-spec (plist-get result :operations)))
    (should result)
    ;; Zsh is now flag-dependent to handle -c flag
    (should (eq ops-spec :flag-dependent))
    (let* ((flag-handlers (plist-get result :flag-handlers))
           ;; Get the default handler (nil trigger, last in list)
           (default-handler (cdr (car (last flag-handlers))))
           (op (car default-handler)))
      (should (eq (plist-get op :operation) :execute)))))

(ert-deftest test-semantics-source-builtin ()
  "Scenario: bash-command-semantics § 'Script execution' - source builtin"
  (let* ((result (jf/bash-lookup-command-semantics "source"))
         (ops (plist-get result :operations))
         (op (car ops)))
    (should result)
    (should (eq (plist-get op :operation) :execute))
    (should (= (plist-get op :index) 0))))

(ert-deftest test-semantics-dot-builtin ()
  "Scenario: bash-command-semantics § 'Script execution' - dot builtin"
  (let* ((result (jf/bash-lookup-command-semantics "."))
         (ops (plist-get result :operations))
         (op (car ops)))
    (should result)
    (should (eq (plist-get op :operation) :execute))
    (should (= (plist-get op :index) 0))))

;;; Go Subcommands

(ert-deftest test-semantics-go-run-subcommand ()
  "Scenario: bash-command-semantics § 'Go subcommands' - go run executes code"
  (let* ((result (jf/bash-lookup-command-semantics "go"))
         (handlers (plist-get result :subcommand-handlers))
         (run-spec (alist-get 'run handlers)))
    (should result)
    (should (eq (plist-get result :operations) :complex))
    (should run-spec)
    (should (= (length run-spec) 1))
    (let ((op (car run-spec)))
      (should (eq (plist-get op :operation) :execute))
      (should (eq (plist-get op :source) :positional-args))
      (should (= (plist-get op :index) 0)))))

(ert-deftest test-semantics-go-test-subcommand ()
  "Scenario: bash-command-semantics § 'Go subcommands' - go test executes tests"
  (let* ((result (jf/bash-lookup-command-semantics "go"))
         (handlers (plist-get result :subcommand-handlers))
         (test-spec (alist-get 'test handlers)))
    (should test-spec)
    (should (= (length test-spec) 1))
    (let ((op (car test-spec)))
      (should (eq (plist-get op :operation) :execute))
      (should (= (plist-get op :index) 0)))))

(ert-deftest test-semantics-go-build-subcommand ()
  "Scenario: bash-command-semantics § 'Go subcommands' - go build reads source"
  (let* ((result (jf/bash-lookup-command-semantics "go"))
         (handlers (plist-get result :subcommand-handlers))
         (build-spec (alist-get 'build handlers)))
    (should build-spec)
    (should (= (length build-spec) 1))
    (let ((op (car build-spec)))
      (should (eq (plist-get op :operation) :read))
      (should (= (plist-get op :index) 0)))))

;;; Test Suite Summary

(defun test-semantics-run-all ()
  "Run all command semantics tests and report results."
  (interactive)
  (ert "^test-semantics-"))

(provide 'test-command-semantics)
;;; test-command-semantics.el ends here
