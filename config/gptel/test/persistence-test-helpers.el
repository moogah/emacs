;;; persistence-test-helpers.el --- Behavioral test helpers for gptel persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Shared test infrastructure for behavioral testing of gptel persistence.
;;
;; Two macros:
;;   `with-captured-io'  — Write-side: captures file writes, dir creation, symlinks.
;;   `with-seeded-files'  — Read-side: seeds known file content for reads.
;;
;; Plus assertion helpers for inspecting captured state.
;;
;; Design: mock at the Emacs primitive boundary via cl-letf so all custom
;; persistence code runs for real.  Mocks are scoped to the body form only.

;;; Code:

(require 'cl-lib)

(defmacro with-captured-io (&rest body)
  "Execute BODY with file I/O primitives intercepted for write-side testing.

Binds the following variables within BODY:
  `captured-files'    — hash table mapping path (string) to content (string)
  `captured-dirs'     — list of directory paths created
  `captured-symlinks' — hash table mapping link path to target path
  `captured-deletes'  — list of deleted file paths

File-existence checks (`file-directory-p', `file-exists-p') reflect only
what has been captured during this execution — they return nil for paths
not yet written.  Override per-test by wrapping specific checks if you
need \"already exists\" scenarios.

All mocks are scoped via `cl-letf' and automatically restored on exit."
  (declare (indent defun) (debug t))
  `(let ((captured-files (make-hash-table :test 'equal))
         (captured-dirs '())
         (captured-symlinks (make-hash-table :test 'equal))
         (captured-deletes '()))
     (cl-letf (((symbol-function 'write-region)
                (lambda (start end filename &optional append _visit &rest _args)
                  (let ((content (cond
                                  ((stringp start) start)
                                  ((null start) (buffer-substring-no-properties (point-min) (point-max)))
                                  (t (buffer-substring-no-properties start end)))))
                    (if append
                        (let ((existing (gethash filename captured-files "")))
                          (puthash filename (concat existing content) captured-files))
                      (puthash filename content captured-files)))))

               ((symbol-function 'make-directory)
                (lambda (dir &optional _parents)
                  (push (directory-file-name (expand-file-name dir)) captured-dirs)))

               ((symbol-function 'make-symbolic-link)
                (lambda (target linkname &optional _ok-if-already-exists)
                  (puthash linkname target captured-symlinks)))

               ((symbol-function 'delete-file)
                (lambda (filename &optional _trash)
                  (push filename captured-deletes)
                  (remhash filename captured-files)))

               ((symbol-function 'file-directory-p)
                (lambda (path)
                  (member (directory-file-name (expand-file-name path)) captured-dirs)))

               ((symbol-function 'file-exists-p)
                (lambda (path)
                  (or (gethash path captured-files)
                      (member (directory-file-name (expand-file-name path)) captured-dirs)
                      nil))))
       ,@body)))

(defmacro with-seeded-files (file-alist &rest body)
  "Execute BODY with FILE-ALIST providing mock file content for reads.

FILE-ALIST is a list of (PATH . CONTENT) cons cells.  For paths in the
alist, `insert-file-contents' inserts CONTENT and `file-exists-p' returns t.
For unlisted paths, both functions delegate to the real filesystem.

Mocks are scoped via `cl-letf' and automatically restored on exit."
  (declare (indent 1) (debug t))
  `(let ((seeded-files ,file-alist)
         (real-insert-file-contents (symbol-function 'insert-file-contents))
         (real-file-exists-p (symbol-function 'file-exists-p)))
     (cl-letf (((symbol-function 'insert-file-contents)
                (lambda (filename &optional visit beg end replace)
                  (let ((seeded (assoc filename seeded-files)))
                    (if seeded
                        (progn
                          (when replace (erase-buffer))
                          (insert (cdr seeded))
                          (list filename (length (cdr seeded))))
                      (funcall real-insert-file-contents
                               filename visit beg end replace)))))

               ((symbol-function 'file-exists-p)
                (lambda (path)
                  (if (assoc path seeded-files)
                      t
                    (funcall real-file-exists-p path)))))
       ,@body)))

(defun captured-file-content (captured-files path)
  "Get content written to PATH from CAPTURED-FILES hash table.
Returns nil if PATH was not written."
  (gethash path captured-files))

(defun captured-file-yaml (captured-files path)
  "Get content written to PATH from CAPTURED-FILES, parsed as YAML.
Returns the parsed Emacs Lisp structure, or nil if PATH was not written.
Requires `yaml' package to be available."
  (when-let ((content (gethash path captured-files)))
    (yaml-parse-string content :object-type 'alist)))

(defun captured-dir-p (captured-dirs path)
  "Return non-nil if PATH was created as a directory in CAPTURED-DIRS."
  (member (directory-file-name (expand-file-name path)) captured-dirs))

(defun captured-symlink-target (captured-symlinks linkname)
  "Get the target of symlink LINKNAME from CAPTURED-SYMLINKS hash table.
Returns nil if LINKNAME was not created."
  (gethash linkname captured-symlinks))

(defun captured-delete-p (captured-deletes path)
  "Return non-nil if PATH was deleted during captured I/O."
  (member path captured-deletes))

(defmacro with-parent-session (session-dir session-id branch-dir &rest body)
  "Execute BODY in a buffer simulating a parent persistent session.
Sets buffer-local `jf/gptel--session-dir', `jf/gptel--session-id',
`jf/gptel--branch-dir', and `jf/gptel--branch-name'.
Cleans up the buffer and registry on exit.

Within BODY, `parent-buffer' is bound to the parent buffer."
  (declare (indent 3) (debug t))
  `(let ((parent-buffer (generate-new-buffer "*gptel-test-parent*")))
     (unwind-protect
         (with-current-buffer parent-buffer
           (setq-local jf/gptel--session-dir ,session-dir)
           (setq-local jf/gptel--session-id ,session-id)
           (setq-local jf/gptel--branch-dir ,branch-dir)
           (setq-local jf/gptel--branch-name "main")
           ;; Insert some content so overlay creation works (avoids bobp edge case)
           (insert "\n\n")
           ,@body)
       (when (buffer-live-p parent-buffer)
         (kill-buffer parent-buffer)))))

(defmacro with-gptel-boundary-mocks (&rest body)
  "Execute BODY with gptel upstream functions mocked and call-tracking.

Binds the following variables within BODY:
  `gptel-preset-calls'        — list of (NAME) calls to gptel-get-preset
  `gptel-apply-preset-calls'  — list of (NAME SETTER) calls to gptel--apply-preset
  `gptel-mode-calls'          — list of (&optional ARG) calls to gptel-mode
  `gptel-request-calls'       — list of plists capturing gptel-request invocations
  `gptel-request-buffer'      — buffer that was current when gptel-request was called

The gptel-get-preset mock returns a minimal valid preset plist.
The gptel--apply-preset mock calls the SETTER with `gptel-model' to simulate
preset application (enough for downstream code that checks buffer-local vars).
The gptel-request mock captures all keyword args without making network calls."
  (declare (indent defun) (debug t))
  `(let ((gptel-preset-calls '())
         (gptel-apply-preset-calls '())
         (gptel-mode-calls '())
         (gptel-request-calls '())
         (gptel-request-buffer nil))
     (cl-letf (((symbol-function 'gptel-get-preset)
                (lambda (name)
                  (push (list name) gptel-preset-calls)
                  ;; Return minimal valid preset
                  '((gptel-model . "test-model"))))

               ((symbol-function 'gptel--apply-preset)
                (lambda (name setter)
                  (push (list name setter) gptel-apply-preset-calls)
                  ;; Call setter to simulate preset application
                  (when setter
                    (funcall setter 'gptel-model "test-model"))))

               ((symbol-function 'gptel-mode)
                (lambda (&optional arg)
                  (push (list arg) gptel-mode-calls)))

               ((symbol-function 'markdown-mode)
                (lambda () nil))

               ((symbol-function 'set-visited-file-name)
                (lambda (filename &optional _no-query _along-with-file)
                  (setq buffer-file-name filename)))

               ((symbol-function 'gptel-request)
                (lambda (&optional prompt &rest args)
                  (setq gptel-request-buffer (current-buffer))
                  (push (append (list :prompt prompt) args) gptel-request-calls)
                  nil)))
       ,@body)))

(provide 'persistence-test-helpers)
;;; persistence-test-helpers.el ends here
