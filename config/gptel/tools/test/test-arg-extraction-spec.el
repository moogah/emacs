;;; test-arg-extraction-spec.el --- Test gptel argument extraction logic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; Tests for the BUG where async scoped tools fail with wrong-number-of-arguments
;;
;; This file tests the argument extraction mechanism that gptel uses to convert
;; from plist-style tool calls (:args (:filepath "value")) to positional arguments.
;;
;; Based on gptel-request.el lines 1727-1731:
;;   (mapcar
;;    (lambda (arg)
;;      (let ((key (intern (concat ":" (plist-get arg :name)))))
;;        (plist-get args key)))
;;    (gptel-tool-args tool-spec))

;;; Code:

(require 'buttercup)

(describe "gptel argument extraction for async tools"

  (it "extracts single argument from plist correctly"
    (let* ((args-spec '((:name "filepath" :type string :description "Path")))
           (args-plist '(:filepath "~/.machine-role"))
           (extracted-values
            (mapcar
             (lambda (arg)
               (let ((key (intern (concat ":" (plist-get arg :name)))))
                 (plist-get args-plist key)))
             args-spec)))
      (expect extracted-values :to-equal '("~/.machine-role"))))

  (it "extracts multiple arguments in correct order"
    (let* ((args-spec '((:name "command" :type string :description "Command")
                        (:name "directory" :type string :description "Directory")))
           (args-plist '(:command "ls -la" :directory "/tmp"))
           (extracted-values
            (mapcar
             (lambda (arg)
               (let ((key (intern (concat ":" (plist-get arg :name)))))
                 (plist-get args-plist key)))
             args-spec)))
      (expect extracted-values :to-equal '("ls -la" "/tmp"))))

  (it "handles missing arguments"
    (let* ((args-spec '((:name "filepath" :type string :description "Path")))
           (args-plist '(:other "value"))
           (extracted-values
            (mapcar
             (lambda (arg)
               (let ((key (intern (concat ":" (plist-get arg :name)))))
                 (plist-get args-plist key)))
             args-spec)))
      (expect extracted-values :to-equal '(nil))))

  (it "demonstrates correct apply usage with callback"
    (let* ((captured-args nil)
           (test-fn (lambda (callback filepath)
                      (setq captured-args (list callback filepath))
                      (funcall callback "result")))
           (callback-fn (lambda (result) result))
           (arg-values '("~/.machine-role"))
           (result (apply test-fn callback-fn arg-values)))

      ;; The function should receive both arguments
      (expect (length captured-args) :to-be 2)
      (expect (car captured-args) :to-be callback-fn)
      (expect (cadr captured-args) :to-equal "~/.machine-role")
      (expect result :to-equal "result")))

  (it "shows wrong-number-of-arguments when arg-values is wrong"
    (let* ((test-fn (lambda (callback filepath)
                      (funcall callback "result")))
           (callback-fn (lambda (result) result)))

      ;; If we pass a plist as a single arg instead of extracted values
      (expect
       (apply test-fn callback-fn '((:filepath "~/.machine-role")))
       :to-throw 'wrong-type-argument)))  ; plist can't be used as a string

  (it "reproduces wrong-number-of-arguments when missing callback"
    (let* ((test-fn (lambda (callback filepath)
                      (funcall callback "result"))))

      ;; If callback is somehow not passed
      (expect
       (funcall test-fn '(:filepath "~/.machine-role"))
       :to-throw 'wrong-number-of-arguments))))

(provide 'test-arg-extraction-spec)
;;; test-arg-extraction-spec.el ends here
