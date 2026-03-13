;;; token-claiming-spec.el --- Unit tests for jf/bash--claim-tokens-for-operations -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests that jf/bash--claim-tokens-for-operations correctly claims tokens
;; for all operation source types without throwing errors.
;;
;; Hypothesis: cl-return inside dolist without cl-block causes the function
;; to throw "No catch for tag: --cl-block-nil--" when processing operations
;; with :source :positional-arg, :exec-block, or other non-redirection sources.

;;; Code:

(require 'cl-lib)
(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))
(require 'bash-parser-orchestrator)

(describe "jf/bash--claim-tokens-for-operations"

  (describe "does not throw for any operation source type"

    (it "handles :source :positional-arg without error"
      (let ((operations '((:file "file.txt"
                           :operation :read
                           :source :positional-arg
                           :command "cat")))
            (parsed-command (list :command-name "cat"
                                 :tokens '((:id 0 :type :command-name :value "cat")
                                           (:id 1 :type :positional-arg :value "file.txt"))
                                 :type :simple)))
        (expect (jf/bash--claim-tokens-for-operations operations parsed-command)
                :not :to-throw)))

    (it "handles :source :exec-block without error"
      (let ((operations '((:file "script.sh"
                           :operation :execute
                           :source :exec-block
                           :command "find")))
            (parsed-command (list :command-name "find"
                                 :tokens '((:id 0 :type :command-name :value "find")
                                           (:id 1 :type :positional-arg :value ".")
                                           (:id 2 :type :positional-arg :value "script.sh"))
                                 :type :simple)))
        (expect (jf/bash--claim-tokens-for-operations operations parsed-command)
                :not :to-throw)))

    (it "handles :source :flag-arg without error"
      (let ((operations '((:file "output.txt"
                           :operation :write
                           :source :flag-arg
                           :command "sed")))
            (parsed-command (list :command-name "sed"
                                 :tokens '((:id 0 :type :command-name :value "sed")
                                           (:id 1 :type :flag :value "-i")
                                           (:id 2 :type :positional-arg :value "output.txt"))
                                 :type :simple)))
        (expect (jf/bash--claim-tokens-for-operations operations parsed-command)
                :not :to-throw)))

    (it "handles :source :redirection without error"
      (let ((operations '((:file "output.txt"
                           :operation :write
                           :source :redirection)))
            (parsed-command (list :command-name "echo"
                                 :tokens '((:id 0 :type :command-name :value "echo")
                                           (:id 1 :type :positional-arg :value "hello")
                                           (:id 2 :type :redirection :value ">")
                                           (:id 3 :type :positional-arg :value "output.txt"))
                                 :type :simple)))
        (expect (jf/bash--claim-tokens-for-operations operations parsed-command)
                :not :to-throw))))

  (describe "returns correct claimed token IDs"

    (it "claims redirection tokens"
      (let ((operations '((:file "output.txt"
                           :operation :write
                           :source :redirection)))
            (parsed-command (list :command-name "echo"
                                 :tokens '((:id 0 :type :command-name :value "echo")
                                           (:id 1 :type :positional-arg :value "hello")
                                           (:id 2 :type :redirection :value ">")
                                           (:id 3 :type :positional-arg :value "output.txt"))
                                 :type :simple)))
        (let ((claimed (jf/bash--claim-tokens-for-operations operations parsed-command)))
          (expect (member 2 claimed) :to-be-truthy))))

    (it "claims command-name and file tokens for positional-arg operations"
      (let ((operations '((:file "file.txt"
                           :operation :read
                           :source :positional-arg
                           :command "cat")))
            (parsed-command (list :command-name "cat"
                                 :tokens '((:id 0 :type :command-name :value "cat")
                                           (:id 1 :type :positional-arg :value "file.txt"))
                                 :type :simple)))
        (let ((claimed (jf/bash--claim-tokens-for-operations operations parsed-command)))
          (expect (member 0 claimed) :to-be-truthy)
          (expect (member 1 claimed) :to-be-truthy))))))

(provide 'token-claiming-spec)
;;; token-claiming-spec.el ends here
