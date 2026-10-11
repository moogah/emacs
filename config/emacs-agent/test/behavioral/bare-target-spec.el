;;; bare-target-spec.el --- Real bare (-Q) targets driven through bin/emacs-agent -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D3, D7 and
;; § Seams: seam/target-lifecycle, seam/wrapper-cli, seam/target-support.
;;
;; The guarded real-daemon smoke of D7 for bare targets.  Every target is
;; an `emacs -Q --daemon=<name>' the wrapper starts in a private socket
;; directory (`emacs-agent-test-with-socket-dir'), with the real Emacs and
;; emacsclient handed to it (`emacs-agent-test-with-real-binaries'); the
;; scope kills whatever is left in that directory on exit.  Nothing here
;; can reach the live socket.  Each spec `assume's the binaries, so a
;; machine without them skips it.  A -Q daemon starts in well under a
;; second.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defmacro bare-target-spec--scope (&rest body)
  "Run BODY in a private socket directory with the real binaries."
  (declare (indent 0))
  `(progn
     (assume (emacs-agent-test-binaries-available-p)
             "no Emacs / emacsclient binary to start a target with")
     (emacs-agent-test-with-socket-dir
       (emacs-agent-test-with-real-binaries
         ,@body))))

(defun bare-target-spec--run (&rest args)
  "Run the wrapper with ARGS; return its result plist."
  (apply #'emacs-agent-test-wrapper-run args))

(defun bare-target-spec--start (name)
  "Start the bare target NAME through the wrapper and expect success."
  (let ((result (bare-target-spec--run "target" "start" name "--bare")))
    (expect (cons name (plist-get result :exit)) :to-equal (cons name 0))
    result))

(defun bare-target-spec--eval (name form)
  "Evaluate FORM in target NAME through the wrapper; return stdout, trimmed."
  (let ((result (bare-target-spec--run "target" name "eval" form)))
    (expect (cons form (plist-get result :exit)) :to-equal (cons form 0))
    (string-trim (plist-get result :stdout))))


(describe "Bare targets through bin/emacs-agent (seam/target-lifecycle)"

  (it "starts a bare target reachable by name with no configuration loaded (Scenario: isolated-emacs-targets § Start a bare target)"
    (bare-target-spec--scope
      (bare-target-spec--start "smoke-bare")
      (expect (file-exists-p (emacs-agent-test-socket "smoke-bare")) :to-be t)
      (expect (car (emacs-agent-test-client "smoke-bare" "server-name"))
              :to-equal 0)
      (expect (bare-target-spec--eval
               "smoke-bare" "(list server-name (boundp 'jf/emacs-dir) (featurep 'gptel))")
              :to-equal "(\"smoke-bare\" nil nil)")))

  (it "lists running targets by name and never the server socket (Scenario: isolated-emacs-targets § Enumerate running targets)"
    (bare-target-spec--scope
      ;; A daemon named `server' in the private directory plays the live
      ;; session's socket.
      (emacs-agent-test-start-daemon "server")
      (bare-target-spec--start "smoke-a")
      (bare-target-spec--start "smoke-b")
      (let ((result (bare-target-spec--run "target" "list")))
        (expect (plist-get result :exit) :to-equal 0)
        (expect (sort (split-string (plist-get result :stdout) "[\n]+" t) #'string<)
                :to-equal '("smoke-a" "smoke-b")))))

  (it "stops a target: its process ends, its socket goes, and the next call fails (Scenario: isolated-emacs-targets § Disposed target no longer routes)"
    (bare-target-spec--scope
      (bare-target-spec--start "smoke-stop")
      (let ((pid (string-to-number (bare-target-spec--eval "smoke-stop" "(emacs-pid)"))))
        (expect (plist-get (bare-target-spec--run "target" "stop" "smoke-stop") :exit)
                :to-equal 0)
        (expect (emacs-agent-test-wait-for (lambda () (null (process-attributes pid))) 5)
                :to-be t)
        (expect (file-exists-p (emacs-agent-test-socket "smoke-stop")) :to-be nil)
        (expect (plist-get (bare-target-spec--run "target" "smoke-stop" "eval" "1") :exit)
                :to-equal 2))))

  (it "answers tools on a bare target with an empty list and still evaluates (Scenario: control-surface-discoverability § Bare target has no manifest)"
    (bare-target-spec--scope
      (bare-target-spec--start "smoke-tools")
      (let ((result (bare-target-spec--run "target" "smoke-tools" "tools")))
        (expect (plist-get result :exit) :to-equal 0)
        (expect (json-parse-string (string-trim (plist-get result :stdout)))
                :to-equal []))
      (expect (bare-target-spec--eval "smoke-tools" "(+ 1 2)") :to-equal "3")))

  (it "returns an eval error's message and keeps serving the target (Scenario: emacs-control-tools § Eval errors are reported to the client)"
    (bare-target-spec--scope
      (bare-target-spec--start "smoke-error")
      (let ((result (bare-target-spec--run "target" "smoke-error" "eval"
                                           "(error \"emacs-agent boom\")")))
        (expect (plist-get result :exit) :to-equal 1)
        (expect (concat (plist-get result :stdout) (plist-get result :stderr))
                :to-match "emacs-agent boom"))
      (expect (bare-target-spec--eval "smoke-error" "(+ 1 2)") :to-equal "3")))

  (it "evaluates and reports frame state with no frame attached (Scenario: emacs-control-tools § Headless target needs no frame for eval and observation)"
    (bare-target-spec--scope
      (bare-target-spec--start "smoke-headless")
      (expect (bare-target-spec--eval
               "smoke-headless"
               "(list (length (frame-list)) (seq-count (lambda (f) (frame-parameter f 'window-system)) (frame-list)))")
              :to-equal "(1 0)")
      (let ((result (bare-target-spec--run "target" "smoke-headless" "state")))
        (expect (plist-get result :exit) :to-equal 0)
        (expect (plist-member (json-parse-string (plist-get result :stdout)
                                                 :object-type 'plist)
                              :buffer)
                :not :to-be nil))))

  (it "returns frame state from a headless bare target through the wrapper (Scenario: frame-state-observation § Frame state returned for a headless target; frame-state-observation § Headless target relies on frame-state)"
    (bare-target-spec--scope
      (bare-target-spec--start "smoke-state")
      (bare-target-spec--eval
       "smoke-state"
       "(progn (with-current-buffer (window-buffer (selected-window)) (erase-buffer) (insert \"smoke text\")) (message \"emacs-agent smoke marker\") t)")
      (let* ((result (bare-target-spec--run "target" "smoke-state" "state"))
             (state (json-parse-string (plist-get result :stdout) :object-type 'plist
                                       :false-object nil :null-object nil)))
        (expect (plist-get result :exit) :to-equal 0)
        (expect (plist-get (plist-get state :buffer) :contents) :to-equal "smoke text")
        (expect (integerp (plist-get (plist-get state :buffer) :point)) :to-be t)
        (expect (length (plist-get state :windows)) :to-be-greater-than 0)
        (expect (plist-get (plist-get state :minibuffer) :active) :to-be nil)
        (expect (seq-some (lambda (line) (string-match-p "emacs-agent smoke marker" line))
                          (plist-get state :messages))
                :to-be-truthy)))))

(defun bare-target-spec--crash (name)
  "Kill the target NAME with SIGKILL, so its socket file stays behind.
Return the socket's path once the process is gone."
  (let ((pid (string-to-number (cdr (emacs-agent-test-client name "(emacs-pid)")))))
    (signal-process pid 'kill)
    (expect (emacs-agent-test-wait-for (lambda () (null (process-attributes pid))) 5)
            :to-be t)
    (let ((socket (emacs-agent-test-socket name)))
      (expect (file-exists-p socket) :to-be t)
      socket)))


(describe "Dead targets through bin/emacs-agent (seam/wrapper-cli, seam/target-lifecycle)"

  (it "reports a crashed target or live session as not running, exit 2, though its socket file stays (Scenario: isolated-emacs-targets § Detect a dead target; agent-target-policy § Live session not running)"
    (bare-target-spec--scope
      (emacs-agent-test-start-daemon "server")
      (bare-target-spec--start "smoke-crash")
      (bare-target-spec--crash "smoke-crash")
      (bare-target-spec--crash "server")
      (dolist (args '(("target" "smoke-crash" "eval" "1")
                      ("target" "smoke-crash" "tools")
                      ("target" "smoke-crash" "state")))
        (let ((result (apply #'bare-target-spec--run args)))
          (expect (cons args (plist-get result :exit)) :to-equal (cons args 2))
          (expect (plist-get result :stderr) :to-match "smoke-crash is not running")))
      (dolist (args '(("live" "tools") ("live" "eval" "1")))
        (let ((result (apply #'bare-target-spec--run args)))
          (expect (cons args (plist-get result :exit)) :to-equal (cons args 2))
          (expect (plist-get result :stderr) :to-match "no live session is running")))))

  (it "lists no target whose socket nothing listens on, and removes that socket (Scenario: isolated-emacs-targets § Enumerate running targets)"
    (bare-target-spec--scope
      (bare-target-spec--start "smoke-alive")
      (bare-target-spec--start "smoke-dead")
      (let ((socket (bare-target-spec--crash "smoke-dead"))
            (result (bare-target-spec--run "target" "list")))
        (expect (plist-get result :exit) :to-equal 0)
        (expect (split-string (plist-get result :stdout) "[\n]+" t)
                :to-equal '("smoke-alive"))
        (expect (file-exists-p socket) :to-be nil))))

  (it "stops a crashed target by removing its socket, reporting it not running (Scenario: isolated-emacs-targets § Disposed target no longer routes)"
    (bare-target-spec--scope
      (bare-target-spec--start "smoke-gone")
      (let* ((socket (bare-target-spec--crash "smoke-gone"))
             (result (bare-target-spec--run "target" "stop" "smoke-gone")))
        (expect (plist-get result :exit) :to-equal 2)
        (expect (plist-get result :stderr) :to-match "smoke-gone is not running")
        (expect (file-exists-p socket) :to-be nil)))))

;;; bare-target-spec.el ends here
