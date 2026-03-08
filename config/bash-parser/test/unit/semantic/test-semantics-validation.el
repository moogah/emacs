;;; test-semantics-validation.el --- Tests for semantics database validation -*- lexical-binding: t; -*-

(require 'ert)
(require 'bash-parser-semantics)

;;; Test valid database

(ert-deftest test-validate-semantics-database-valid ()
  "Test that the actual semantics database is valid."
  (let ((errors (jf/bash-validate-semantics-database)))
    (should (null errors))))

;;; Test empty subcommand handler detection

(ert-deftest test-validate-empty-subcommand-handler ()
  "Test detection of empty subcommand handlers."
  (let ((jf/bash-command-file-semantics
         '((npm . (:operations :complex
                   :subcommand-handlers ((install . ())
                                        (run . ((:source :positional-args :operation :read)))))))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (consp errors))
      (should (string-match-p "npm.*subcommand 'install'.*empty" (car errors))))))

(ert-deftest test-validate-missing-subcommand-handlers ()
  "Test detection of :complex without :subcommand-handlers."
  (let ((jf/bash-command-file-semantics
         '((cargo . (:operations :complex)))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (consp errors))
      (should (string-match-p "cargo.*:complex.*no :subcommand-handlers" (car errors))))))

;;; Test custom handler validation

(ert-deftest test-validate-missing-custom-handler ()
  "Test detection of :custom without :handler."
  (let ((jf/bash-command-file-semantics
         '((foo . (:operations :custom)))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (consp errors))
      (should (string-match-p "foo.*:custom.*no :handler" (car errors))))))

(ert-deftest test-validate-undefined-handler-function ()
  "Test detection of undefined custom handler function."
  (let ((jf/bash-command-file-semantics
         '((bar . (:operations :custom
                   :handler jf/bash--nonexistent-handler)))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (consp errors))
      (should (string-match-p "bar.*handler function.*not defined" (car errors))))))

(ert-deftest test-validate-defined-handler-function ()
  "Test that defined custom handler functions pass validation."
  (let ((jf/bash-command-file-semantics
         '((find . (:operations :custom
                    :handler jf/bash--extract-find-operations)))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (null errors)))))

;;; Test flag-dependent validation

(ert-deftest test-validate-missing-flag-handlers ()
  "Test detection of :flag-dependent without :flag-handlers."
  (let ((jf/bash-command-file-semantics
         '((sed . (:operations :flag-dependent)))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (consp errors))
      (should (string-match-p "sed.*:flag-dependent.*no :flag-handlers" (car errors))))))

;;; Test invalid operation types

(ert-deftest test-validate-invalid-operation-type-simple ()
  "Test detection of invalid operation type in simple command."
  (let ((jf/bash-command-file-semantics
         '((foo . (:operations ((:source :positional-args :operation :writes)))))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (consp errors))
      (should (string-match-p "foo.*invalid operation type ':writes'" (car errors))))))

(ert-deftest test-validate-invalid-operation-type-flag-dependent ()
  "Test detection of invalid operation type in flag-dependent command."
  (let ((jf/bash-command-file-semantics
         '((sed . (:operations :flag-dependent
                   :flag-handlers ((("-i") . ((:source :positional-args :operation :modifies)))))))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (consp errors))
      (should (string-match-p "sed.*invalid operation type ':modifies'" (car errors))))))

(ert-deftest test-validate-valid-operation-types ()
  "Test that all valid operation types pass validation."
  (let ((jf/bash-command-file-semantics
         '((cat . (:operations ((:source :positional-args :operation :read))))
           (touch . (:operations ((:source :positional-args :operation :create-or-modify))))
           (rm . (:operations ((:source :positional-args :operation :delete))))
           (chmod . (:operations ((:source :positional-args :operation :modify))))
           (mkdir . (:operations ((:source :positional-args :operation :create))))
           (cp . (:operations ((:source :positional-args :operation :write))))
           (bash . (:operations ((:source :positional-args :operation :execute))))
           (grep . (:operations ((:source :positional-args :operation :match-pattern))))
           (find . (:operations ((:source :positional-args :operation :read-directory))))
           (which . (:operations ((:source :positional-args :operation :read-metadata))))
           (tee . (:operations ((:source :positional-args :operation :append)))))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (null errors)))))

;;; Test multiple errors

(ert-deftest test-validate-multiple-errors ()
  "Test that multiple validation errors are all reported."
  (let ((jf/bash-command-file-semantics
         '((npm . (:operations :complex
                   :subcommand-handlers ((install . ()))))
           (foo . (:operations :custom))
           (bar . (:operations ((:source :positional-args :operation :writes)))))))
    (let ((errors (jf/bash-validate-semantics-database)))
      (should (= (length errors) 3))
      (should (seq-some (lambda (e) (string-match-p "npm.*empty" e)) errors))
      (should (seq-some (lambda (e) (string-match-p "foo.*no :handler" e)) errors))
      (should (seq-some (lambda (e) (string-match-p "bar.*:writes" e)) errors)))))

(provide 'test-semantics-validation)
;;; test-semantics-validation.el ends here
