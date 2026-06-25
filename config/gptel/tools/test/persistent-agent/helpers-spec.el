;;; helpers-spec.el --- Shared fixtures for persistent-agent tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared test infrastructure for the persistent-agent suite.
;; Provides:
;;   `jf/persistent-agent-test--with-mock-parent-session'  -- temp session-dir
;;                                                            with parent session
;;                                                            buffer-locals set
;;   `jf/persistent-agent-test--with-mock-preset'          -- register an
;;                                                            ephemeral preset
;;   `jf/persistent-agent-test--with-mock-gptel-request'   -- capture args + FSM
;;                                                            instead of network
;;
;; This file uses `provide' so sibling spec files may `require' it.

;;; Code:
(require 'cl-lib)
(require 'buttercup)
(require 'gptel)
(require 'gptel-session-filesystem)

(defmacro jf/persistent-agent-test--with-mock-parent-session (&rest body)
  "Run BODY inside a temp parent persistent session.
The temp dir matches the layout `<root>/<session-id>/branches/main/'
so that nested-agent paths under `agents/' resolve correctly when
content-addressed activation binds session identity.

Inside BODY, the following buffer-local-style bindings are visible
as `let'-scoped variables:
  `mock-session-dir' -- `<root>/<session-id>/'
  `mock-branch-dir'  -- `<root>/<session-id>/branches/main/'
  `mock-session-id'  -- the session-id string

On exit, the temp tree is removed."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "pa-test-" t))
          (mock-session-id (format "test-session-%d" (random 1000000)))
          (mock-session-dir (expand-file-name mock-session-id root))
          (mock-branch-dir (expand-file-name "branches/main" mock-session-dir)))
     (unwind-protect
         (progn
           (make-directory mock-branch-dir t)
           (with-temp-file (expand-file-name "session.org" mock-branch-dir)
             (insert ":PROPERTIES:\n:GPTEL_PRESET: dummy\n:END:\n#+begin_user\n#+end_user\n"))
           (let ((jf/gptel--session-id mock-session-id)
                 (jf/gptel--session-dir mock-session-dir)
                 (jf/gptel--branch-name "main")
                 (jf/gptel--branch-dir mock-branch-dir))
             ,@body))
       (delete-directory root t))))

(defmacro jf/persistent-agent-test--with-mock-preset (name &rest body)
  "Register a minimal gptel preset NAME (a symbol) for the duration of BODY."
  (declare (indent 1) (debug t))
  `(let ((preset-name ,name))
     (unwind-protect
         (progn
           (gptel-make-preset preset-name
             :description "test preset"
             :backend gptel-backend
             :model gptel-model)
           ,@body)
       (setq gptel--known-presets
             (assq-delete-all preset-name gptel--known-presets)))))

(defmacro jf/persistent-agent-test--with-mock-gptel-request (capture-var &rest body)
  "Stub `gptel-request' to push its args onto CAPTURE-VAR.
CAPTURE-VAR is a symbol naming a list variable in the surrounding
scope.  Each captured invocation is a cons whose car is `:prompt'
and whose cdr is `(PROMPT . PLIST-ARGS)' — i.e. the prompt
followed by the keyword arguments `gptel-request' was called with
(`:callback', `:context', `:fsm', etc.).  `gptel-request'
defaults `:buffer' to `current-buffer' at call time, so it only
appears in PLIST-ARGS when the caller passed it explicitly."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'gptel-request)
              (lambda (prompt &rest args)
                (push (cons :prompt (cons prompt args)) ,capture-var)
                nil)))
     ,@body))

;;; Smoke tests -------------------------------------------------------------

(describe "jf/persistent-agent-test--with-mock-parent-session"
  (it "binds session vars and creates the branch directory"
    (jf/persistent-agent-test--with-mock-parent-session
      (expect (file-directory-p mock-branch-dir) :to-be t)
      (expect (file-directory-p mock-session-dir) :to-be t)
      (expect (stringp mock-session-id) :to-be t)
      (expect jf/gptel--session-id :to-equal mock-session-id)
      (expect jf/gptel--session-dir :to-equal mock-session-dir)
      (expect jf/gptel--branch-name :to-equal "main")
      (expect jf/gptel--branch-dir :to-equal mock-branch-dir)
      (expect (file-exists-p (expand-file-name "session.org" mock-branch-dir))
              :to-be t)))

  (it "removes the temp tree on exit"
    (let (captured-root)
      (jf/persistent-agent-test--with-mock-parent-session
        (setq captured-root (file-name-directory
                             (directory-file-name mock-session-dir))))
      (expect (file-exists-p captured-root) :to-be nil))))

(describe "jf/persistent-agent-test--with-mock-preset"
  (it "registers the preset for BODY and removes it after"
    (let ((name (intern (format "pa-test-preset-%d" (random 1000000)))))
      (jf/persistent-agent-test--with-mock-preset name
        (expect (assq name gptel--known-presets) :not :to-be nil))
      (expect (assq name gptel--known-presets) :to-be nil))))

(describe "jf/persistent-agent-test--with-mock-gptel-request"
  (it "captures gptel-request invocations instead of dispatching them"
    (let ((captured nil))
      (jf/persistent-agent-test--with-mock-gptel-request captured
        (gptel-request "hello" :callback #'ignore))
      (expect (length captured) :to-equal 1)
      (let ((entry (car captured)))
        (expect (car entry) :to-equal :prompt)
        (expect (cadr entry) :to-equal "hello")
        (expect (plist-get (cddr entry) :callback) :to-equal #'ignore)))))

(provide 'jf-persistent-agent-test-helpers)
;;; helpers-spec.el ends here
