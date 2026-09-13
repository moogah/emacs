;;; org-topic-with-id-spec.el --- jf/gptel-org-new-topic-with-id -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural specs for `jf/gptel-org-new-topic-with-id' (change
;; workspace-notes-in-vault, design OQ-A): a pure-gptel wrapper around
;; the third-party `gptel-org-utils-new-topic' that also stamps an org
;; `:ID:' on the new topic heading, so "mark a topic" and "fork a
;; conversation" are one gesture and the topic is a link target.
;;
;; The command is defined in config/gptel/gptel.org (tangled to
;; gptel.el), which the test Emacs loads through init.el; the spec
;; lives here with the other gptel.org-level specs rather than under a
;; subsystem test directory.
;;
;; Runs the real `gptel-org-utils-new-topic' and real `org-id-get-create'
;; against a temp `.org' file (org-id insists on a file-visiting
;; buffer).  Nothing is stubbed.  One spec covers the non-file case:
;; the command must refuse up front with `user-error' rather than
;; insert the topic and then fail inside org-id.

;;; Code:

(require 'buttercup)
(require 'org)
(require 'org-id)

(defmacro jf-gptel-topic-test--with-org-file (buffer-var &rest body)
  "Visit a fresh temp `.org' file in `org-mode', bind BUFFER-VAR, run BODY.
The buffer is marked unmodified and killed, and the file deleted,
afterwards -- even on error."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((file (make-temp-file "gptel-topic-id-" nil ".org"))
          (enable-local-variables nil)
          (,buffer-var nil))
     (unwind-protect
         (progn
           (setq ,buffer-var (find-file-noselect file))
           (with-current-buffer ,buffer-var
             (org-mode)
             ,@body))
       (when (buffer-live-p ,buffer-var)
         (with-current-buffer ,buffer-var
           (set-buffer-modified-p nil))
         (kill-buffer ,buffer-var))
       (when (file-exists-p file)
         (delete-file file)))))

(describe "jf/gptel-org-new-topic-with-id"

  (it "is defined and interactive"
    (expect (fboundp 'jf/gptel-org-new-topic-with-id) :to-be t)
    (expect (commandp 'jf/gptel-org-new-topic-with-id) :to-be-truthy))

  (it "creates a level-1 heading carrying GPTEL_TOPIC and an :ID:, and returns that ID"
    (jf-gptel-topic-test--with-org-file buf
      (let ((id (jf/gptel-org-new-topic-with-id "Retry design")))
        (expect (stringp id) :to-be t)
        (expect id :to-match "\\S-")
        (save-excursion
          (goto-char (point-min))
          (expect (re-search-forward "^\\* Retry design$" nil t) :to-be-truthy)
          (expect (org-current-level) :to-equal 1)
          ;; The upstream topic property is untouched (whitespace ->
          ;; "-", as gptel-org-utils-new-topic writes it).
          (expect (org-entry-get (point) "GPTEL_TOPIC") :to-equal "Retry-design")
          ;; The :ID: lands on the topic heading and is what we returned.
          (expect (org-entry-get (point) "ID") :to-equal id)))))

  (it "stamps the ID on the topic heading, not on the Next Question subheading"
    (jf-gptel-topic-test--with-org-file buf
      (jf/gptel-org-new-topic-with-id "Retry design")
      (save-excursion
        (goto-char (point-min))
        (expect (re-search-forward "^\\*\\* Next Question$" nil t) :to-be-truthy)
        (expect (org-entry-get (point) "ID" nil t) :to-be nil))))

  (it "leaves point where gptel-org-utils-new-topic leaves it (below the subheading)"
    (jf-gptel-topic-test--with-org-file buf
      (jf/gptel-org-new-topic-with-id "Retry design")
      (expect (org-at-heading-p) :to-be nil)
      (save-excursion
        (org-back-to-heading t)
        (expect (looking-at "\\*\\* Next Question") :to-be-truthy))))

  (it "signals user-error and leaves the buffer untouched when it visits no file"
    (with-temp-buffer
      (org-mode)
      (insert "* Existing heading\n")
      (set-buffer-modified-p nil)
      (let ((before (buffer-string))
            (org-id-overriding-file-name nil))
        (expect (buffer-file-name) :to-be nil)
        (expect (jf/gptel-org-new-topic-with-id "Scratch topic")
                :to-throw 'user-error)
        (expect (buffer-string) :to-equal before)
        (expect (buffer-modified-p) :to-be nil))))

  (it "gives each topic its own ID"
    (jf-gptel-topic-test--with-org-file buf
      (let ((first (jf/gptel-org-new-topic-with-id "Topic one"))
            (second (jf/gptel-org-new-topic-with-id "Topic two")))
        (expect first :not :to-equal second)
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "^\\* Topic one$")
          (expect (org-entry-get (point) "ID") :to-equal first)
          (re-search-forward "^\\* Topic two$")
          (expect (org-entry-get (point) "ID") :to-equal second))))))

(provide 'org-topic-with-id-spec)
;;; org-topic-with-id-spec.el ends here
