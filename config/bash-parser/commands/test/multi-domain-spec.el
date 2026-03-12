;;; multi-domain-spec.el --- Tests for multi-domain command handlers -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

;;; Helper to reset registry between tests

(defun multi-domain-test--reset-registry ()
  "Reset the command handler registry and re-register handlers."
  (setq jf/bash-command-handlers (make-hash-table :test 'equal))
  ;; Re-load the handler files to re-register
  (load (expand-file-name "config/bash-parser/commands/aws.el" jf/emacs-dir) nil t)
  (load (expand-file-name "config/bash-parser/commands/curl.el" jf/emacs-dir) nil t))

;;; Tests

(describe "Multi-Domain Command Handlers"

  (before-each
    (multi-domain-test--reset-registry))

  (describe "AWS CLI"

    (describe "filesystem handler"

      (it "detects local file as :read source in s3 cp upload"
        (let ((result (jf/bash-command-aws--filesystem-handler
                       '(:command-name "aws"
                         :args ("s3" "cp" "local.txt" "s3://bucket/")
                         :positional-args ("s3" "cp" "local.txt" "s3://bucket/")
                         :flags nil))))
          (expect result :not :to-be nil)
          (expect (plist-get result :domain) :to-equal :filesystem)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :file) :to-equal "local.txt")
            (expect (plist-get (car ops) :operation) :to-equal :read))))

      (it "detects local file as :write dest in s3 cp download"
        (let ((result (jf/bash-command-aws--filesystem-handler
                       '(:command-name "aws"
                         :args ("s3" "cp" "s3://bucket/file.txt" "local.txt")
                         :positional-args ("s3" "cp" "s3://bucket/file.txt" "local.txt")
                         :flags nil))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :file) :to-equal "local.txt")
            (expect (plist-get (car ops) :operation) :to-equal :write))))

      (it "detects local dir as :read source in s3 sync"
        (let ((result (jf/bash-command-aws--filesystem-handler
                       '(:command-name "aws"
                         :args ("s3" "sync" "local-dir/" "s3://bucket/")
                         :positional-args ("s3" "sync" "local-dir/" "s3://bucket/")
                         :flags nil))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :file) :to-equal "local-dir/")
            (expect (plist-get (car ops) :operation) :to-equal :read))))

      (it "detects local file as :delete in s3 mv"
        (let ((result (jf/bash-command-aws--filesystem-handler
                       '(:command-name "aws"
                         :args ("s3" "mv" "local.txt" "s3://bucket/")
                         :positional-args ("s3" "mv" "local.txt" "s3://bucket/")
                         :flags nil))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :file) :to-equal "local.txt")
            (expect (plist-get (car ops) :operation) :to-equal :delete))))

      (it "skips s3:// paths as remote"
        (let ((result (jf/bash-command-aws--filesystem-handler
                       '(:command-name "aws"
                         :args ("s3" "cp" "s3://src-bucket/file" "s3://dest-bucket/file")
                         :positional-args ("s3" "cp" "s3://src-bucket/file" "s3://dest-bucket/file")
                         :flags nil))))
          (expect result :to-be nil)))

      (it "returns nil for non-s3 subcommands"
        (let ((result (jf/bash-command-aws--filesystem-handler
                       '(:command-name "aws"
                         :args ("ec2" "describe-instances")
                         :positional-args ("ec2" "describe-instances")
                         :flags nil))))
          (expect result :to-be nil)))

      (it "handles both local source and dest"
        (let ((result (jf/bash-command-aws--filesystem-handler
                       '(:command-name "aws"
                         :args ("s3" "cp" "source.txt" "dest.txt")
                         :positional-args ("s3" "cp" "source.txt" "dest.txt")
                         :flags nil))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 2)))))

    (describe "authentication handler"

      (it "extracts --profile value"
        (let ((result (jf/bash-command-aws--auth-handler
                       '(:command-name "aws"
                         :args ("--profile" "prod" "s3" "ls")
                         :positional-args ("s3" "ls")
                         :flags ("--profile")))))
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (plist-get op :provider) :to-equal :aws)
            (expect (cdr (assoc :profile context)) :to-equal "prod"))))

      (it "extracts --region value"
        (let ((result (jf/bash-command-aws--auth-handler
                       '(:command-name "aws"
                         :args ("--region" "us-east-1" "s3" "ls")
                         :positional-args ("s3" "ls")
                         :flags ("--region")))))
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (cdr (assoc :region context)) :to-equal "us-east-1"))))

      (it "extracts both --profile and --region"
        (let ((result (jf/bash-command-aws--auth-handler
                       '(:command-name "aws"
                         :args ("--profile" "prod" "--region" "eu-west-1" "s3" "ls")
                         :positional-args ("s3" "ls")
                         :flags ("--profile" "--region")))))
          (let* ((ops (plist-get result :operations))
                 (op (car ops))
                 (context (plist-get op :context)))
            (expect (cdr (assoc :profile context)) :to-equal "prod")
            (expect (cdr (assoc :region context)) :to-equal "eu-west-1"))))

      (it "returns authentication domain even without profile/region"
        (let ((result (jf/bash-command-aws--auth-handler
                       '(:command-name "aws"
                         :args ("s3" "ls")
                         :positional-args ("s3" "ls")
                         :flags nil))))
          (expect (plist-get result :domain) :to-equal :authentication)
          (let* ((ops (plist-get result :operations))
                 (op (car ops)))
            (expect (plist-get op :provider) :to-equal :aws)))))

    (describe "network handler"

      (it "returns network domain for any aws command"
        (let ((result (jf/bash-command-aws--network-handler
                       '(:command-name "aws"
                         :args ("s3" "ls")
                         :positional-args ("s3" "ls")
                         :flags nil))))
          (expect (plist-get result :domain) :to-equal :network)
          (let* ((ops (plist-get result :operations))
                 (op (car ops)))
            (expect (plist-get op :protocol) :to-equal :https)
            (expect (plist-get op :endpoint) :to-equal "amazonaws.com"))))

      (it "returns network domain for ec2 commands"
        (let ((result (jf/bash-command-aws--network-handler
                       '(:command-name "aws"
                         :args ("ec2" "describe-instances")
                         :positional-args ("ec2" "describe-instances")
                         :flags nil))))
          (expect (plist-get result :domain) :to-equal :network))))

    (describe "multi-domain integration"

      (it "registers three separate handlers for aws"
        (let ((domains (jf/bash-lookup-command-handlers "aws")))
          (expect domains :not :to-be nil)
          (expect (gethash :filesystem domains) :not :to-be nil)
          (expect (gethash :authentication domains) :not :to-be nil)
          (expect (gethash :network domains) :not :to-be nil)))

      (it "extracts all three domains via registry"
        (let ((result (jf/bash-extract-command-semantics
                       '(:command-name "aws"
                         :args ("--profile" "prod" "s3" "cp" "local.txt" "s3://bucket/")
                         :positional-args ("s3" "cp" "local.txt" "s3://bucket/")
                         :flags ("--profile")))))
          (let ((domains (plist-get result :domains)))
            (expect (alist-get :filesystem domains) :not :to-be nil)
            (expect (alist-get :authentication domains) :not :to-be nil)
            (expect (alist-get :network domains) :not :to-be nil))))

      (it "isolates handler failure from other domains"
        ;; Override filesystem handler to error, auth and network should still work
        (let ((domains (jf/bash-lookup-command-handlers "aws")))
          (puthash :filesystem
                   (list (lambda (_cmd) (error "Simulated failure")))
                   domains))
        (let* ((result (jf/bash-extract-command-semantics
                        '(:command-name "aws"
                          :args ("s3" "cp" "local.txt" "s3://bucket/")
                          :positional-args ("s3" "cp" "local.txt" "s3://bucket/")
                          :flags nil)))
               (domains (plist-get result :domains)))
          ;; Filesystem should be absent (handler errored)
          (expect (alist-get :filesystem domains) :to-be nil)
          ;; Auth and network should still be present
          (expect (alist-get :authentication domains) :not :to-be nil)
          (expect (alist-get :network domains) :not :to-be nil)))))

  (describe "curl"

    (describe "filesystem handler"

      (it "detects -o output file as :write"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("-o" "output.html" "https://example.com")
                         :positional-args ("output.html" "https://example.com")
                         :flags ("-o")))))
          (expect result :not :to-be nil)
          (expect (plist-get result :domain) :to-equal :filesystem)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :file) :to-equal "output.html")
            (expect (plist-get (car ops) :operation) :to-equal :write))))

      (it "detects --output file as :write"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("--output" "result.json" "https://api.example.com")
                         :positional-args ("result.json" "https://api.example.com")
                         :flags ("--output")))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (plist-get (car ops) :file) :to-equal "result.json")
            (expect (plist-get (car ops) :operation) :to-equal :write))))

      (it "detects -d @file as :read with @ stripped"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("-d" "@data.json" "https://api.example.com")
                         :positional-args ("@data.json" "https://api.example.com")
                         :flags ("-d")))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :file) :to-equal "data.json")
            (expect (plist-get (car ops) :operation) :to-equal :read))))

      (it "detects --data @file as :read"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("--data" "@payload.xml" "https://api.example.com")
                         :positional-args ("@payload.xml" "https://api.example.com")
                         :flags ("--data")))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (plist-get (car ops) :file) :to-equal "payload.xml"))))

      (it "ignores -d without @ prefix (inline data)"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("-d" "key=value" "https://api.example.com")
                         :positional-args ("key=value" "https://api.example.com")
                         :flags ("-d")))))
          (expect result :to-be nil)))

      (it "detects -T upload file as :read"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("-T" "upload.bin" "https://example.com/upload")
                         :positional-args ("upload.bin" "https://example.com/upload")
                         :flags ("-T")))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :file) :to-equal "upload.bin")
            (expect (plist-get (car ops) :operation) :to-equal :read))))

      (it "detects --upload-file as :read"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("--upload-file" "file.dat" "https://example.com/put")
                         :positional-args ("file.dat" "https://example.com/put")
                         :flags ("--upload-file")))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (plist-get (car ops) :file) :to-equal "file.dat")
            (expect (plist-get (car ops) :operation) :to-equal :read))))

      (it "detects multiple file operations in one command"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("-d" "@data.json" "-o" "response.json" "https://api.example.com")
                         :positional-args ("@data.json" "response.json" "https://api.example.com")
                         :flags ("-d" "-o")))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 2))))

      (it "returns nil when no file operations present"
        (let ((result (jf/bash-command-curl--filesystem-handler
                       '(:command-name "curl"
                         :args ("https://example.com")
                         :positional-args ("https://example.com")
                         :flags nil))))
          (expect result :to-be nil))))

    (describe "network handler"

      (it "extracts https URL"
        (let ((result (jf/bash-command-curl--network-handler
                       '(:command-name "curl"
                         :args ("https://example.com/api")
                         :positional-args ("https://example.com/api")
                         :flags nil))))
          (expect result :not :to-be nil)
          (expect (plist-get result :domain) :to-equal :network)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :protocol) :to-equal :https)
            (expect (plist-get (car ops) :endpoint) :to-equal "https://example.com/api"))))

      (it "extracts http URL"
        (let ((result (jf/bash-command-curl--network-handler
                       '(:command-name "curl"
                         :args ("http://example.com")
                         :positional-args ("http://example.com")
                         :flags nil))))
          (let ((ops (plist-get result :operations)))
            (expect (plist-get (car ops) :protocol) :to-equal :http))))

      (it "extracts URL from among flags"
        (let ((result (jf/bash-command-curl--network-handler
                       '(:command-name "curl"
                         :args ("-o" "out.html" "https://example.com")
                         :positional-args ("out.html" "https://example.com")
                         :flags ("-o")))))
          (expect result :not :to-be nil)
          (let ((ops (plist-get result :operations)))
            (expect (length ops) :to-equal 1)
            (expect (plist-get (car ops) :endpoint) :to-equal "https://example.com"))))

      (it "returns nil when no URL present"
        (let ((result (jf/bash-command-curl--network-handler
                       '(:command-name "curl"
                         :args ("--help")
                         :positional-args nil
                         :flags ("--help")))))
          (expect result :to-be nil))))

    (describe "multi-domain integration"

      (it "registers two separate handlers for curl"
        (let ((domains (jf/bash-lookup-command-handlers "curl")))
          (expect domains :not :to-be nil)
          (expect (gethash :filesystem domains) :not :to-be nil)
          (expect (gethash :network domains) :not :to-be nil)))

      (it "extracts both domains via registry"
        (let ((result (jf/bash-extract-command-semantics
                       '(:command-name "curl"
                         :args ("-o" "output.html" "https://example.com")
                         :positional-args ("output.html" "https://example.com")
                         :flags ("-o")))))
          (let ((domains (plist-get result :domains)))
            (expect (alist-get :filesystem domains) :not :to-be nil)
            (expect (alist-get :network domains) :not :to-be nil))))

      (it "returns only network domain when no file ops"
        (let ((result (jf/bash-extract-command-semantics
                       '(:command-name "curl"
                         :args ("https://example.com")
                         :positional-args ("https://example.com")
                         :flags nil))))
          (let ((domains (plist-get result :domains)))
            (expect (alist-get :filesystem domains) :to-be nil)
            (expect (alist-get :network domains) :not :to-be nil))))))

  (describe "domain independence"

    (it "one command's handlers do not affect another command"
      (let ((aws-domains (jf/bash-lookup-command-handlers "aws"))
            (curl-domains (jf/bash-lookup-command-handlers "curl")))
        ;; AWS has 3 domains, curl has 2
        (expect (gethash :authentication aws-domains) :not :to-be nil)
        (expect (gethash :authentication curl-domains) :to-be nil)))

    (it "handler errors in one domain do not prevent other domains"
      ;; Replace curl filesystem handler with error
      (let ((domains (jf/bash-lookup-command-handlers "curl")))
        (puthash :filesystem
                 (list (lambda (_cmd) (error "Test error")))
                 domains))
      (let* ((result (jf/bash-extract-command-semantics
                      '(:command-name "curl"
                        :args ("-o" "out.html" "https://example.com")
                        :positional-args ("out.html" "https://example.com")
                        :flags ("-o"))))
             (domains (plist-get result :domains)))
        ;; Filesystem errored out
        (expect (alist-get :filesystem domains) :to-be nil)
        ;; Network still works
        (expect (alist-get :network domains) :not :to-be nil)))))

;;; multi-domain-spec.el ends here
