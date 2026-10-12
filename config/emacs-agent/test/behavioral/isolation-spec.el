;;; isolation-spec.el --- What isolation the operating system gives a target -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D2, D4 and
;; § Seams: seam/target-lifecycle.
;;
;; The properties the design takes from the operating system rather than
;; from its own code: a named server socket is the control requirement
;; (D2), each target is its own process (D4), and a target shares the host
;; filesystem.  They hold at HEAD, so these specs are plain and pass
;; today; they start Emacs servers directly (`emacs -Q --daemon=<name>',
;; or a batch Emacs calling `server-start') rather than through the
;; wrapper, whose own specs are in bare-target-spec.el.
;;
;; Every Emacs here lives in a private socket directory
;; (`emacs-agent-test-with-socket-dir').  Where a scenario speaks of the
;; user's live session, a daemon named `server' in that private directory
;; stands in for it: the real live session is never contacted.  Each spec
;; `assume's the Emacs and emacsclient binaries.
;;
;; One guard is the configuration's own: org-roam.el starts a server only
;; outside a daemon, so a daemon target never takes the default `server'
;; socket during its init.  Its spec evaluates that one form with
;; `daemonp' stubbed, starting nothing.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defvar native-comp-enable-subr-trampolines)

(defmacro isolation-spec--scope (&rest body)
  "Run BODY in a private socket directory, when the binaries exist."
  (declare (indent 0))
  `(progn
     (assume (emacs-agent-test-binaries-available-p)
             "no Emacs / emacsclient binary to start a server with")
     (emacs-agent-test-with-socket-dir ,@body)))

(defun isolation-spec--eval (name form)
  "Evaluate FORM in NAME and return the printed value; expect success."
  (let ((reply (emacs-agent-test-client name form)))
    (expect (cons form (car reply)) :to-equal (cons form 0))
    (cdr reply)))

(defconst isolation-spec--snapshot
  "(list (sort (mapcar #'buffer-name (buffer-list)) #'string<) (with-current-buffer \"*scratch*\" (buffer-string)) (boundp 'isolation-spec-probe))"
  "A form describing an Emacs's session state: buffers, *scratch*, a probe variable.")

(defconst isolation-spec--disturb
  "(progn (get-buffer-create \"isolation-probe\") (with-current-buffer \"*scratch*\" (insert \"changed\")) (defvar isolation-spec-probe t) t)"
  "A form that changes an Emacs's buffers and globals.")


(describe "Isolation from the operating system (seam/target-lifecycle)"

  (it "controls a non-daemon Emacs that called server-start under a unique name (Scenario: isolated-emacs-targets § Server, not daemon, is the control requirement)"
    (isolation-spec--scope
      (emacs-agent-test-start-batch-server "ea-batch")
      (expect (isolation-spec--eval "ea-batch" "(list (daemonp) server-name (+ 1 2))")
              :to-equal "(nil \"ea-batch\" 3)")))

  (it "leaves a stand-in live session unchanged when a target's buffers and globals change (Scenario: isolated-emacs-targets § Target activity does not touch the live session)"
    (isolation-spec--scope
      (emacs-agent-test-start-daemon "server")
      (emacs-agent-test-start-daemon "ea-target")
      (let ((before (isolation-spec--eval "server" isolation-spec--snapshot)))
        (isolation-spec--eval "ea-target" isolation-spec--disturb)
        (expect (isolation-spec--eval "ea-target" "(boundp 'isolation-spec-probe)")
                :to-equal "t")
        (expect (isolation-spec--eval "server" isolation-spec--snapshot)
                :to-equal before))))

  (it "leaves one target's session state unchanged when another is modified (Scenario: isolated-emacs-targets § Targets do not share session state)"
    (isolation-spec--scope
      (emacs-agent-test-start-daemon "ea-one")
      (emacs-agent-test-start-daemon "ea-two")
      (let ((before (isolation-spec--eval "ea-two" isolation-spec--snapshot)))
        (isolation-spec--eval "ea-one" isolation-spec--disturb)
        (expect (isolation-spec--eval "ea-two" isolation-spec--snapshot)
                :to-equal before))))

  (it "keeps other targets and the stand-in live session usable, and fails fast against a killed target's socket (Scenario: isolated-emacs-targets § A crashing target does not affect others)"
    (isolation-spec--scope
      (emacs-agent-test-start-daemon "server")
      (let ((doomed (emacs-agent-test-start-daemon "ea-doomed")))
        (emacs-agent-test-start-daemon "ea-survivor")
        (signal-process doomed 'kill)
        (expect (emacs-agent-test-wait-for (lambda () (null (process-attributes doomed))) 5)
                :to-be t)
        (let* ((start (float-time))
               (reply (emacs-agent-test-client "ea-doomed" "(+ 1 1)")))
          (expect (car reply) :not :to-equal 0)
          (expect (< (- (float-time) start) 5) :to-be t))
        (expect (isolation-spec--eval "ea-survivor" "(+ 1 2)") :to-equal "3")
        (expect (isolation-spec--eval "server" "(+ 2 2)") :to-equal "4"))))

  (it "lets the host see a file a target writes, since a target is no filesystem sandbox (Scenario: isolated-emacs-targets § Filesystem is shared, not sandboxed)"
    (isolation-spec--scope
      (emacs-agent-test-start-daemon "ea-writer")
      (let ((file (expand-file-name "written-by-target.txt" emacs-agent-test--tmpdir)))
        (isolation-spec--eval
         "ea-writer" (format "(progn (write-region \"from the target\" nil %S) t)" file))
        (expect (with-temp-buffer (insert-file-contents file) (buffer-string))
                :to-equal "from the target")))))

(defun isolation-spec--server-start-forms ()
  "The top-level forms of org-roam.el that call `server-start'."
  (let ((file (expand-file-name "config/major-modes/org-roam.el"
                                emacs-agent-test-root))
        forms)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)))
    (seq-filter (lambda (form) (memq 'server-start (flatten-tree form)))
                (nreverse forms))))


(describe "The org-roam server-start guard (seam/target-lifecycle)"

  (it "starts no server during a daemon's init or in batch, the default one only in the live session, and isolated-CHECKOUT from an emacs-isolated.sh launch (seam/target-lifecycle)"
    ;; Only the guarded form is evaluated, never the module: loading it
    ;; would start org-roam in the test process.  The form sets
    ;; `server-name', so it is bound here.
    (let ((forms (isolation-spec--server-start-forms))
          (daemon nil)
          (started nil))
      (expect (length forms) :to-equal 1)
      (spy-on 'server-start :and-call-fake (lambda (&rest _) (push server-name started)))
      (spy-on 'server-running-p :and-return-value nil)
      ;; `daemonp' is a primitive: redefining it must not compile a
      ;; native trampoline.
      (let ((native-comp-enable-subr-trampolines nil)
            (process-environment (copy-sequence process-environment))
            (server-name server-name)
            (jf/emacs-dir "/w/emacs/"))
        (cl-letf (((symbol-function 'daemonp) (lambda () daemon)))
          (setq daemon t)
          (let ((noninteractive nil)) (eval (car forms) t))
          (setq daemon nil)
          (eval (car forms) t)            ; batch, as the test runner is
          (expect 'server-start :not :to-have-been-called)
          (setenv "EMACS_USER_DIRECTORY" nil)
          (let ((noninteractive nil)) (eval (car forms) t))
          (setenv "EMACS_USER_DIRECTORY" "/w/emacs/runtime")
          (let ((noninteractive nil)) (eval (car forms) t))
          (expect (reverse started) :to-equal '("server" "isolated-emacs")))))))

;;; isolation-spec.el ends here
