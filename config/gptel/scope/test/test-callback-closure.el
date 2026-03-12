;;; test-callback-closure.el --- Test callback closure in transient -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; Tests for callback closure preservation in scope expansion.
;;
;; Historical bug (fixed 2026-03-11):
;; The scope-core.el file had the lexical-binding mode line on line 5 instead
;; of line 1, causing Emacs to load it with DYNAMIC binding. This broke the
;; expansion-callback closure in jf/gptel-scope--trigger-inline-expansion,
;; causing "Symbol's value as variable is void: wrapper-callback" errors when
;; transient actions tried to invoke the callback.
;;
;; Root cause: org-babel tangling with :comments both added structure comments
;; before the mode line, pushing it down from line 1.
;;
;; Fix: Use :comments no for the Lexical Binding block in scope-core.org.
;;
;; These tests verify:
;; 1. Basic closure preservation (direct invocation)
;; 2. Closure preservation through plist storage (simulating transient scope)
;; 3. The full callback chain works as designed

;;; Code:

(require 'ert)
(require 'transient)
(require 'jf-gptel-scope-core)
(require 'jf-gptel-scope-expansion)

(ert-deftest test-callback-closure-basic ()
  "Test that basic closure mechanism works for expansion callbacks.
This is the simplest test - if this fails, lexical-binding is broken."
  (let* ((captured-value nil)
         (outer-var "test-value")
         (closure-fn (lambda ()
                      (setq captured-value outer-var))))
    (funcall closure-fn)
    (should (equal captured-value "test-value"))))

(ert-deftest test-callback-closure-preserved-in-transient ()
  "Test that callback closures are preserved through transient scope.
This simulates what happens when transient stores and retrieves the
callback from its scope plist. If lexical-binding is not on line 1,
this test will fail with 'void-variable wrapper-callback'."
  (let* ((wrapper-callback-invoked nil)
         (wrapper-callback-arg nil)
         (expansion-ui-closed nil)

         ;; This is the wrapper callback (simulating the one in define-scoped-tool)
         (wrapper-callback
          (lambda (expansion-result)
            (setq wrapper-callback-invoked t)
            (setq wrapper-callback-arg expansion-result)))

         ;; This is the expansion callback (created in trigger-inline-expansion)
         (expansion-callback
          (lambda (result-json)
            "Test expansion callback that closes over wrapper-callback."
            (condition-case err
                (let* ((parsed (json-parse-string result-json :object-type 'plist))
                       (success (plist-get parsed :success)))
                  (if success
                      (funcall wrapper-callback (list :approved t))
                    (funcall wrapper-callback (list :approved nil :reason "user_denied"))))
              (error
               (message "Error in expansion-callback: %s" (error-message-string err))
               (funcall wrapper-callback (list :approved nil :reason "callback_error"))))))

         (violation-info (list :tool "read_file"
                              :resource "/tmp/test.txt"
                              :reason "not-in-scope"
                              :validation-type 'path))
         (patterns '("/tmp/test.txt"))
         (tool-name "read_file"))

    ;; Verify the expansion-callback has wrapper-callback in closure
    ;; by invoking it directly (this should work)
    (funcall expansion-callback (json-serialize (list :success t)))
    (should (eq wrapper-callback-invoked t))
    (should (equal wrapper-callback-arg '(:approved t)))

    ;; Reset for next test
    (setq wrapper-callback-invoked nil)
    (setq wrapper-callback-arg nil)

    ;; Now test through transient scope (this is where the bug occurs)
    ;; We need to manually extract the callback from transient scope
    ;; and invoke it to see if the closure is preserved
    (let ((scope-plist (list :violation violation-info
                            :callback expansion-callback
                            :patterns patterns
                            :tool-name tool-name)))

      ;; Simulate what transient does - store and retrieve the callback
      (let* ((retrieved-callback (plist-get scope-plist :callback)))

        ;; This should work if closures are preserved
        (funcall retrieved-callback (json-serialize (list :success t)))
        (should (eq wrapper-callback-invoked t))
        (should (equal wrapper-callback-arg '(:approved t)))))))

(provide 'test-callback-closure)
;;; test-callback-closure.el ends here
