;;; registry-spec.el --- Tests for handler registry -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

;;; Helper functions for tests

(defun registry-test--make-handler (domain ops &optional claimed-ids)
  "Create a test handler that returns DOMAIN with OPS and optional CLAIMED-IDS."
  (lambda (_parsed-command)
    (let ((result (list :domain domain :operations ops)))
      (when claimed-ids
        (setq result (plist-put result :claimed-token-ids claimed-ids)))
      result)))

(defun registry-test--make-nil-handler ()
  "Create a test handler that returns nil."
  (lambda (_parsed-command) nil))

(defun registry-test--make-error-handler ()
  "Create a test handler that signals an error."
  (lambda (_parsed-command)
    (error "Test handler error")))

;;; Tests

(describe "Handler Registry"

  (describe "jf/bash-command-handlers initialization"

    (it "is a hash table"
      (expect (hash-table-p jf/bash-command-handlers) :to-be t))

    (it "uses equal test for string keys"
      (expect (hash-table-test jf/bash-command-handlers) :to-equal 'equal)))

  (describe "jf/bash-register-command-handler"

    (before-each
      (setq jf/bash-command-handlers (make-hash-table :test 'equal)))

    (it "registers a handler for a command and domain"
      (jf/bash-register-command-handler
       :command "cat"
       :domain :filesystem
       :handler #'identity)
      (let ((domains (gethash "cat" jf/bash-command-handlers)))
        (expect domains :not :to-be nil)
        (expect (hash-table-p domains) :to-be t)
        (expect (gethash :filesystem domains) :to-contain #'identity)))

    (it "signals error when :command is missing"
      (expect (jf/bash-register-command-handler
               :domain :filesystem
               :handler #'identity)
              :to-throw 'error))

    (it "signals error when :domain is missing"
      (expect (jf/bash-register-command-handler
               :command "cat"
               :handler #'identity)
              :to-throw 'error))

    (it "signals error when :handler is missing"
      (expect (jf/bash-register-command-handler
               :command "cat"
               :domain :filesystem)
              :to-throw 'error))

    (it "registers multiple handlers for the same domain"
      (let ((handler-a #'car)
            (handler-b #'cdr))
        (jf/bash-register-command-handler
         :command "aws" :domain :filesystem :handler handler-a)
        (jf/bash-register-command-handler
         :command "aws" :domain :filesystem :handler handler-b)
        (let* ((domains (gethash "aws" jf/bash-command-handlers))
               (handlers (gethash :filesystem domains)))
          (expect (length handlers) :to-equal 2)
          (expect handlers :to-equal (list handler-a handler-b)))))

    (it "registers handlers for multiple domains"
      (jf/bash-register-command-handler
       :command "aws" :domain :filesystem :handler #'car)
      (jf/bash-register-command-handler
       :command "aws" :domain :authentication :handler #'cdr)
      (let ((domains (gethash "aws" jf/bash-command-handlers)))
        (expect (gethash :filesystem domains) :not :to-be nil)
        (expect (gethash :authentication domains) :not :to-be nil))))

  (describe "jf/bash-lookup-command-handlers"

    (before-each
      (setq jf/bash-command-handlers (make-hash-table :test 'equal)))

    (it "returns domain hash table for registered command"
      (jf/bash-register-command-handler
       :command "cat" :domain :filesystem :handler #'identity)
      (let ((result (jf/bash-lookup-command-handlers "cat")))
        (expect result :not :to-be nil)
        (expect (hash-table-p result) :to-be t)))

    (it "returns nil for unregistered command"
      (expect (jf/bash-lookup-command-handlers "nonexistent") :to-be nil))

    (it "preserves handler registration order"
      (let ((handler-a #'car)
            (handler-b #'cdr)
            (handler-c #'list))
        (jf/bash-register-command-handler
         :command "aws" :domain :filesystem :handler handler-a)
        (jf/bash-register-command-handler
         :command "aws" :domain :filesystem :handler handler-b)
        (jf/bash-register-command-handler
         :command "aws" :domain :filesystem :handler handler-c)
        (let* ((domains (jf/bash-lookup-command-handlers "aws"))
               (handlers (gethash :filesystem domains)))
          (expect handlers :to-equal (list handler-a handler-b handler-c))))))

  (describe "jf/bash-extract-command-semantics"

    (before-each
      (setq jf/bash-command-handlers (make-hash-table :test 'equal)))

    (it "extracts semantics for single-domain command"
      (let ((handler (registry-test--make-handler
                      :filesystem
                      '((:file "test.txt" :operation :read)))))
        (jf/bash-register-command-handler
         :command "cat" :domain :filesystem :handler handler)
        (let ((result (jf/bash-extract-command-semantics
                       '(:command-name "cat" :positional-args ("test.txt")))))
          (expect (plist-get result :domains) :not :to-be nil)
          (let ((fs-ops (alist-get :filesystem (plist-get result :domains))))
            (expect (length fs-ops) :to-equal 1)
            (expect (plist-get (car fs-ops) :file) :to-equal "test.txt")
            (expect (plist-get (car fs-ops) :operation) :to-equal :read)))))

    (it "extracts semantics for multi-domain command"
      (let ((fs-handler (registry-test--make-handler
                         :filesystem
                         '((:file "local.txt" :operation :read))))
            (auth-handler (registry-test--make-handler
                           :authentication
                           '((:provider :aws :context nil)))))
        (jf/bash-register-command-handler
         :command "aws" :domain :filesystem :handler fs-handler)
        (jf/bash-register-command-handler
         :command "aws" :domain :authentication :handler auth-handler)
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "aws" :positional-args ("s3" "cp"))))
               (domains (plist-get result :domains)))
          (expect (alist-get :filesystem domains) :not :to-be nil)
          (expect (alist-get :authentication domains) :not :to-be nil))))

    (it "isolates handler errors and continues"
      (let ((error-handler (registry-test--make-error-handler))
            (good-handler (registry-test--make-handler
                           :filesystem
                           '((:file "test.txt" :operation :read)))))
        ;; Register error handler first, then good handler for different domain
        (jf/bash-register-command-handler
         :command "test-cmd" :domain :broken :handler error-handler)
        (jf/bash-register-command-handler
         :command "test-cmd" :domain :filesystem :handler good-handler)
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "test-cmd")))
               (domains (plist-get result :domains)))
          ;; The good handler's results should still be present
          (expect (alist-get :filesystem domains) :not :to-be nil)
          ;; The broken domain should not appear
          (expect (alist-get :broken domains) :to-be nil))))

    (it "merges operations from multiple handlers in same domain"
      (let ((handler-a (registry-test--make-handler
                        :filesystem
                        '((:file "a.txt" :operation :read))))
            (handler-b (registry-test--make-handler
                        :filesystem
                        '((:file "b.txt" :operation :write)))))
        (jf/bash-register-command-handler
         :command "multi" :domain :filesystem :handler handler-a)
        (jf/bash-register-command-handler
         :command "multi" :domain :filesystem :handler handler-b)
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "multi")))
               (fs-ops (alist-get :filesystem (plist-get result :domains))))
          (expect (length fs-ops) :to-equal 2)
          (expect (plist-get (car fs-ops) :file) :to-equal "a.txt")
          (expect (plist-get (cadr fs-ops) :file) :to-equal "b.txt"))))

    (it "collects and deduplicates claimed token IDs"
      (let ((handler-a (registry-test--make-handler
                        :filesystem
                        '((:file "a.txt" :operation :read))
                        '(1 2 3)))
            (handler-b (registry-test--make-handler
                        :authentication
                        '((:provider :aws))
                        '(2 3 4))))
        (jf/bash-register-command-handler
         :command "aws" :domain :filesystem :handler handler-a)
        (jf/bash-register-command-handler
         :command "aws" :domain :authentication :handler handler-b)
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "aws")))
               (claimed (plist-get result :claimed-token-ids)))
          ;; Should contain 1, 2, 3, 4 (deduplicated)
          (expect (length claimed) :to-equal 4)
          (expect (member 1 claimed) :not :to-be nil)
          (expect (member 4 claimed) :not :to-be nil))))

    (it "returns empty domains for unregistered command"
      (let ((result (jf/bash-extract-command-semantics
                     '(:command-name "unknown-cmd"))))
        (expect (plist-get result :domains) :to-be nil)
        (expect (plist-get result :claimed-token-ids) :to-be nil)))

    (it "dispatches only to the matching command's handlers"
      (let ((cat-handler (registry-test--make-handler
                          :filesystem
                          '((:file "cat-file.txt" :operation :read))))
            (rm-handler (registry-test--make-handler
                         :filesystem
                         '((:file "rm-file.txt" :operation :delete)))))
        (jf/bash-register-command-handler
         :command "cat" :domain :filesystem :handler cat-handler)
        (jf/bash-register-command-handler
         :command "rm" :domain :filesystem :handler rm-handler)
        ;; Dispatch for "cat" should only get cat-handler results
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "cat" :positional-args ("foo.txt"))))
               (fs-ops (alist-get :filesystem (plist-get result :domains))))
          (expect (length fs-ops) :to-equal 1)
          (expect (plist-get (car fs-ops) :file) :to-equal "cat-file.txt"))))

    (it "passes the full parsed-command to the handler"
      (let* ((received-arg nil)
             (spy-handler (lambda (parsed-command)
                            (setq received-arg parsed-command)
                            (list :domain :filesystem
                                  :operations '((:file "x" :operation :read))))))
        (jf/bash-register-command-handler
         :command "spy" :domain :filesystem :handler spy-handler)
        (jf/bash-extract-command-semantics
         '(:command-name "spy" :positional-args ("a" "b") :flags ("-n")))
        (expect received-arg :not :to-be nil)
        (expect (plist-get received-arg :command-name) :to-equal "spy")
        (expect (plist-get received-arg :positional-args) :to-equal '("a" "b"))
        (expect (plist-get received-arg :flags) :to-equal '("-n"))))

    (it "returns consistent plist structure even with no handlers"
      (let ((result (jf/bash-extract-command-semantics
                     '(:command-name "no-handlers"))))
        ;; Should always return a plist with :domains and :claimed-token-ids
        (expect (plist-member result :domains) :not :to-be nil)
        (expect (plist-member result :claimed-token-ids) :not :to-be nil)))

    (it "skips handlers that return nil"
      (let ((nil-handler (registry-test--make-nil-handler))
            (good-handler (registry-test--make-handler
                           :filesystem
                           '((:file "test.txt" :operation :read)))))
        (jf/bash-register-command-handler
         :command "test-cmd" :domain :filesystem :handler nil-handler)
        (jf/bash-register-command-handler
         :command "test-cmd" :domain :filesystem :handler good-handler)
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "test-cmd")))
               (fs-ops (alist-get :filesystem (plist-get result :domains))))
          (expect (length fs-ops) :to-equal 1))))))

;;; registry-spec.el ends here
