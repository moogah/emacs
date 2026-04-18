;;; token-emission-spec.el --- Layer 0: Parser token verification -*- lexical-binding: t; -*-

;; Verifies the parser produces correct tokens/redirections for commands
;; whose extraction path is under investigation.  This layer establishes
;; that the parser itself is NOT the source of any extraction gaps.

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))

(describe "Layer 0: parser token emission"

  (describe "ls /tmp"
    (let ((parsed (jf/bash-parse "ls /tmp")))
      (it "has command-name ls"
        (expect (plist-get parsed :command-name) :to-equal "ls"))
      (it "has positional-arg /tmp"
        (expect (plist-get parsed :positional-args) :to-equal '("/tmp")))
      (it "has a :command-name token"
        (expect (cl-some (lambda (tok)
                           (and (eq (plist-get tok :type) :command-name)
                                (equal (plist-get tok :value) "ls")))
                         (plist-get parsed :tokens))
                :to-be-truthy))
      (it "has a :positional-arg token for /tmp"
        (expect (cl-some (lambda (tok)
                           (and (eq (plist-get tok :type) :positional-arg)
                                (equal (plist-get tok :value) "/tmp")))
                         (plist-get parsed :tokens))
                :to-be-truthy))))

  (describe "echo hello > /tmp/out.txt"
    (let ((parsed (jf/bash-parse "echo hello > /tmp/out.txt")))
      (it "has command-name echo"
        (expect (plist-get parsed :command-name) :to-equal "echo"))
      (it "has :redirections with operator >"
        (let ((redirs (plist-get parsed :redirections)))
          (expect redirs :not :to-be nil)
          (expect (cl-some (lambda (r)
                             (equal (plist-get r :operator) ">"))
                           redirs)
                  :to-be-truthy)))
      (it "has redirection destination /tmp/out.txt"
        (let ((redirs (plist-get parsed :redirections)))
          (expect (cl-some (lambda (r)
                             (equal (plist-get r :destination) "/tmp/out.txt"))
                           redirs)
                  :to-be-truthy)))
      (it "has a :redirection token"
        (expect (cl-some (lambda (tok)
                           (eq (plist-get tok :type) :redirection))
                         (plist-get parsed :tokens))
                :to-be-truthy))))

  (describe "echo data >> /tmp/log.txt"
    (let ((parsed (jf/bash-parse "echo data >> /tmp/log.txt")))
      (it "has command-name echo"
        (expect (plist-get parsed :command-name) :to-equal "echo"))
      (it "has :redirections with operator >>"
        (let ((redirs (plist-get parsed :redirections)))
          (expect redirs :not :to-be nil)
          (expect (cl-some (lambda (r)
                             (equal (plist-get r :operator) ">>"))
                           redirs)
                  :to-be-truthy)))
      (it "has redirection destination /tmp/log.txt"
        (let ((redirs (plist-get parsed :redirections)))
          (expect (cl-some (lambda (r)
                             (equal (plist-get r :destination) "/tmp/log.txt"))
                           redirs)
                  :to-be-truthy)))
      (it "has a :redirection token"
        (expect (cl-some (lambda (tok)
                           (eq (plist-get tok :type) :redirection))
                         (plist-get parsed :tokens))
                :to-be-truthy)))))

(provide 'token-emission-spec)
;;; token-emission-spec.el ends here
