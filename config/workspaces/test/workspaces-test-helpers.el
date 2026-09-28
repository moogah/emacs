;;; workspaces-test-helpers.el --- Core test helpers for the workspaces specs -*- lexical-binding: t; -*-

;; Stand-ins that CORE workspaces specs share.  This file is NOT a spec:
;; the runner discovers `*-spec.el' only, so nothing here executes unless
;; a spec loads it, by path, as the specs load their fixtures:
;;
;;   (load (expand-file-name "workspaces-test-helpers.el"
;;                           (file-name-directory
;;                            (or load-file-name buffer-file-name))))
;;
;; It depends on nothing but Emacs itself, so a core spec can load it
;; without depending on a consumer's fixtures.  The portable-bookmarks
;; fixtures (portable-bookmarks-fixtures.el) load it too, so the consumer
;; specs reach the same helpers.  The counting validator in
;; portable-bookmarks-spec.el ("finds no inline stand-in in any spec
;; file") fails the suite when a spec defines its own copy of one of
;; these instead of using it.

(require 'cl-lib)


;;; A fake remote (TRAMP) file-name handler

(defvar workspaces-test-fake-remote-asked nil
  "Operations the fake remote handler was asked, newest first.
Reset on entry to `workspaces-test-with-fake-remote' and left in place
on exit, so a spec may read it after the macro returns.")

(defvar workspaces-test-fake-remote-touches nil
  "Operations the fake remote handler refused, newest first.
A refused operation is a filesystem touch of a remote name: under real
TRAMP it would open a connection.  Reset and kept like
`workspaces-test-fake-remote-asked'.")

(defun workspaces-test-fake-remote-handler (prefix allow)
  "Return a file-name handler that plays TRAMP for names under PREFIX.

The handler records every operation it is asked in
`workspaces-test-fake-remote-asked'.  `file-remote-p' answers PREFIX
from the name alone, as TRAMP does without its CONNECTED argument.  An
operation in ALLOW (a list of pure file-NAME operations such as
`expand-file-name') is answered by Emacs's default implementation, with
no handler in play.  Every other operation is recorded in
`workspaces-test-fake-remote-touches' and signals `file-error', standing
in for a connection attempt to an unreachable host."
  (lambda (operation &rest args)
    (push operation workspaces-test-fake-remote-asked)
    (cond
     ((eq operation 'file-remote-p) prefix)
     ((memq operation allow)
      (let ((file-name-handler-alist nil))
        (apply operation args)))
     (t
      (push operation workspaces-test-fake-remote-touches)
      (signal 'file-error
              (list "Fake remote host contacted" (symbol-name operation)
                    (car args)))))))

(defmacro workspaces-test-with-fake-remote (spec &rest body)
  "Run BODY with names under a prefix served by a fake remote handler.

SPEC is PREFIX, a string such as \"/wsx-remote:\", or (PREFIX :allow
OPERATIONS).  Names that begin with PREFIX go to
`workspaces-test-fake-remote-handler': by default ONLY `file-remote-p'
is answered and every other operation signals, so \"the remote host is
never contacted\" is what BODY is checked against.  OPERATIONS is the
explicit list of pure file-NAME operations BODY needs to pass through;
list only what the spec actually needs.

`workspaces-test-fake-remote-asked' and
`workspaces-test-fake-remote-touches' are reset on entry and survive
exit.  The handler goes in FRONT of `file-name-handler-alist', so it,
not TRAMP, serves a prefix TRAMP would also claim; every other name
keeps its usual handlers.  Returns BODY's value."
  (declare (indent 1) (debug (form body)))
  (let* ((spec (if (consp spec) spec (list spec)))
         (prefix (car spec))
         (allow (plist-get (cdr spec) :allow))
         (prefix-var (make-symbol "prefix")))
    `(let ((,prefix-var ,prefix))
       (setq workspaces-test-fake-remote-asked nil
             workspaces-test-fake-remote-touches nil)
       (let ((file-name-handler-alist
              (cons (cons (concat "\\`" (regexp-quote ,prefix-var))
                          (workspaces-test-fake-remote-handler
                           ,prefix-var ',allow))
                    file-name-handler-alist)))
         ,@body))))

(defun workspaces-test-fake-remote-operations ()
  "Return the distinct operations the fake remote handler was asked, first asked first."
  (delete-dups (reverse workspaces-test-fake-remote-asked)))

(provide 'workspaces-test-helpers)
;;; workspaces-test-helpers.el ends here
