;;; handler-merge-spec.el --- Integration tests for handler-merge in orchestrator -*- lexical-binding: t; -*-

;; Tests that command handler results are correctly merged into the
;; orchestrator's output by jf/bash-extract-semantics.

(require 'cl-lib)
(require 'bash-parser-plugins)
(require 'bash-parser-semantics)

;; Load contract validation helpers
(let* ((contracts-dir (expand-file-name "config/test/contracts/" jf/emacs-dir)))
  (add-to-list 'load-path contracts-dir))
(require 'contract-core)
(require 'contract-bash-parser)

;; Load the shared contract-test-helpers from commands/test/
(require 'contract-test-helpers
         (expand-file-name "config/bash-parser/commands/test/contract-test-helpers.el"
                           jf/emacs-dir))

;;; Helpers

(defvar handler-merge-test--saved-handlers nil
  "Saved handler table to restore after each test.")

(defun handler-merge-test--make-handler (domain ops &optional claimed-ids)
  "Create a handler returning DOMAIN with OPS and optional CLAIMED-IDS."
  (lambda (_parsed-command)
    (let ((result (list :domain domain :operations ops)))
      (when claimed-ids
        (setq result (plist-put result :claimed-token-ids claimed-ids)))
      result)))

;;; Tests

(describe "Orchestrator Handler Merge"

  (before-each
    (setq handler-merge-test--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal)))

  (after-each
    (setq jf/bash-command-handlers handler-merge-test--saved-handlers))

  (describe "handler results appearing in orchestrator output"

    (it "includes handler :domains in orchestrator result"
      (let ((handler (handler-merge-test--make-handler
                      :filesystem
                      '((:file "test.txt" :operation :read :confidence :high)))))
        (jf/bash-register-command-handler
         :command "cat" :domain :filesystem :handler handler)
        (let* ((parsed (list :command-name "cat"
                             :positional-args '("test.txt")
                             :tokens '()
                             :parse-complete t))
               (result (jf/bash-extract-semantics parsed))
               (domains (plist-get result :domains))
               (fs-ops (cdr (assq :filesystem domains))))
          (contract-test--validate-domains domains "handler-merge single handler")
          (expect fs-ops :not :to-be nil)
          (expect (length fs-ops) :to-equal 1)
          (expect (plist-get (car fs-ops) :file) :to-equal "test.txt")
          (expect (plist-get (car fs-ops) :operation) :to-equal :read))))

    (it "includes handler claimed-token-ids in orchestrator coverage"
      (let ((handler (handler-merge-test--make-handler
                      :filesystem
                      '((:file "a.txt" :operation :read :confidence :high))
                      '(1 2))))
        (jf/bash-register-command-handler
         :command "cat" :domain :filesystem :handler handler)
        (let* ((parsed (list :command-name "cat"
                             :tokens '((:id 1 :text "cat")
                                       (:id 2 :text "a.txt")
                                       (:id 3 :text "extra"))
                             :parse-complete t))
               (result (jf/bash-extract-semantics parsed))
               (domains (plist-get result :domains)))
          (contract-test--validate-domains domains "handler-merge token coverage")
          (let* ((coverage (plist-get result :coverage))
                 (claimed (plist-get coverage :claimed-tokens))
                 (unclaimed (plist-get coverage :unclaimed-tokens)))
            ;; Handler claimed 2 of 3 tokens
            (expect claimed :to-equal 2)
            ;; Only token 3 should be unclaimed
            (expect (length unclaimed) :to-equal 1)
            (expect (plist-get (car unclaimed) :id) :to-equal 3)))))

    (it "includes multi-domain handler results in orchestrator output"
      (let ((fs-handler (handler-merge-test--make-handler
                         :filesystem
                         '((:file "data.csv" :operation :read :confidence :high))))
            (auth-handler (handler-merge-test--make-handler
                           :authentication
                           '((:provider :aws :context nil)))))
        (jf/bash-register-command-handler
         :command "aws" :domain :filesystem :handler fs-handler)
        (jf/bash-register-command-handler
         :command "aws" :domain :authentication :handler auth-handler)
        (let* ((parsed (list :command-name "aws"
                             :positional-args '("s3" "cp")
                             :tokens '()
                             :parse-complete t))
               (result (jf/bash-extract-semantics parsed))
               (domains (plist-get result :domains)))
          (contract-test--validate-domains domains "handler-merge multi-domain")
          (expect (assq :filesystem domains) :not :to-be nil)
          (expect (assq :authentication domains) :not :to-be nil))))))

;;; handler-merge-spec.el ends here
