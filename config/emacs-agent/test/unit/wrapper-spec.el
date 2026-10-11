;;; wrapper-spec.el --- bin/emacs-agent: targets, tiers, sockets, refusals -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D13, D23 and
;; § Seams:
;;   seam/wrapper-cli        grammar, explicit sockets, quoting, exit status
;;   seam/target-lifecycle   what `target start' launches
;;   seam/data-profile       the wrapper's half: --data exported verbatim
;;
;; The wrapper is run as an agent runs it, as a command with arguments.
;; Behind it sit fake emacsclient and Emacs binaries
;; (`emacs-agent-test-with-fake-clients') that record what they are given
;; and start nothing, inside a private socket directory
;; (`emacs-agent-test-with-socket-dir'), so no call can reach the user's
;; live Emacs.  A listening socket with nothing behind it
;; (`emacs-agent-test-stand-in-socket') plays a running live session or
;; target where the wrapper checks for one.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defmacro wrapper-spec--scope (&rest body)
  "Run BODY in a private socket directory with fake binaries."
  (declare (indent 0))
  `(emacs-agent-test-with-socket-dir
     (emacs-agent-test-with-fake-clients
       ,@body)))

(defun wrapper-spec--with-reply (text thunk)
  "Call THUNK with the fake emacsclient answering the string TEXT.
The fake prints TEXT as emacsclient prints a string value: `prin1'ed."
  (let ((process-environment (cons (concat "FAKE_EMACSCLIENT_REPLY="
                                           (prin1-to-string text))
                                   process-environment)))
    (funcall thunk)))

(defun wrapper-spec--refused-p (result)
  "Non-nil when RESULT is a usage refusal: exit 2 with a message."
  (and (eq (plist-get result :exit) 2)
       (not (string-empty-p (string-trim (plist-get result :stderr))))))

(defun wrapper-spec--nothing-started-p ()
  "Non-nil when no fake binary was run."
  (and (null (emacs-agent-test-fake-calls "client"))
       (null (emacs-agent-test-fake-calls "emacs"))
       (null (emacs-agent-test-fake-calls "isolated"))))


(describe "bin/emacs-agent: naming the target (seam/wrapper-cli)"

  (it "delivers a live call to the default server socket by its full path (Scenario: agent-target-policy § Live target is named explicitly)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (let ((result (wrapper-spec--with-reply
                     "{\"ok\":true,\"result\":\"()\"}"
                     (lambda () (emacs-agent-test-wrapper-run
                                 "live" "call" "org_graph_query" "{}")))))
        (expect (plist-get result :exit) :to-equal 0)
        (let ((calls (emacs-agent-test-fake-calls "client")))
          (expect (length calls) :to-equal 1)
          (expect (emacs-agent-test-socket-arg (car calls))
                  :to-equal (emacs-agent-test-socket "server"))
          (expect (emacs-agent-test-sent-form (car calls))
                  :to-equal '(jf/agent-call "call" "org_graph_query" "{}"))))))

  (it "refuses a call that names no target, and runs no emacsclient (Scenario: agent-target-policy § Missing target is refused)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (dolist (args '(()
                      ("call" "org_graph_query" "{}")
                      ("call-write" "org_graph_write_node" "{}")
                      ("eval" "(+ 1 1)")
                      ("tools")
                      ("target")
                      ("target" "eval" "(+ 1 1)")
                      ("target" "call" "org_graph_query" "{}")))
        (let ((result (apply #'emacs-agent-test-wrapper-run args)))
          (expect (cons args (wrapper-spec--refused-p result))
                  :to-equal (cons args t))))
      (expect (wrapper-spec--nothing-started-p) :to-be t)))

  (it "fails with \"no live session is running\" when the default socket is absent, using no other socket (Scenario: agent-target-policy § Live session not running)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket "smoke-1")
      (dolist (args '(("live" "tools")
                      ("live" "call" "org_graph_query" "{}")
                      ("live" "eval" "(+ 1 1)")))
        (let ((result (apply #'emacs-agent-test-wrapper-run args)))
          (expect (plist-get result :exit) :to-equal 2)
          (expect (plist-get result :stderr) :to-match "no live session is running")))
      (expect (emacs-agent-test-fake-calls "client") :to-be nil)))

  (it "refuses to start or address a target named server, starting nothing (Scenario: isolated-emacs-targets § Live socket name is refused)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (dolist (args `(("target" "start" "server" "--bare")
                      ("target" "start" "server" "--worktree"
                       ,(emacs-agent-test-fake-worktree))
                      ("target" "server" "eval" "(+ 1 1)")
                      ("target" "server" "tools")
                      ("target" "stop" "server")))
        (let ((result (apply #'emacs-agent-test-wrapper-run args)))
          (expect (cons args (wrapper-spec--refused-p result))
                  :to-equal (cons args t))))
      (expect (wrapper-spec--nothing-started-p) :to-be t)))

  (it "refuses start, list and stop as target names (seam/wrapper-cli)"
    (wrapper-spec--scope
      (dolist (name '("start" "list" "stop"))
        (expect (wrapper-spec--refused-p
                 (emacs-agent-test-wrapper-run "target" "start" name "--bare"))
                :to-be t))
      (expect (wrapper-spec--nothing-started-p) :to-be t)))

  (it "fails a call to a target that is not running, at once and without emacsclient (Scenario: isolated-emacs-targets § Detect a dead target)"
    (wrapper-spec--scope
      (let* ((start (float-time))
             (result (emacs-agent-test-wrapper-run "target" "ghost" "eval" "(+ 1 1)")))
        (expect (plist-get result :exit) :to-equal 2)
        (expect (plist-get result :stderr) :to-match "ghost")
        (expect (plist-get result :stderr) :to-match "not running")
        (expect (< (- (float-time) start) 5) :to-be t))
      (expect (emacs-agent-test-fake-calls "client") :to-be nil))))


(describe "bin/emacs-agent: tiers and what reaches Emacs (seam/wrapper-cli)"

  (it "passes -s with a socket path on every emacsclient invocation (Scenario: agent-target-policy § Wrapper never omits the socket)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (emacs-agent-test-stand-in-socket "smoke-1")
      (wrapper-spec--with-reply
       "{\"ok\":true,\"result\":\"x\"}"
       (lambda ()
         (dolist (args '(("live" "tools")
                         ("live" "call" "org_graph_query" "{}")
                         ("live" "call-write" "org_graph_write_node" "{\"title\":\"t\"}")
                         ("live" "eval" "(+ 1 1)")
                         ("target" "smoke-1" "tools")
                         ("target" "smoke-1" "call" "org_graph_query" "{}")
                         ("target" "smoke-1" "call-write" "org_graph_write_node" "{}")
                         ("target" "smoke-1" "eval" "(+ 1 1)")
                         ("target" "smoke-1" "state")))
           (apply #'emacs-agent-test-wrapper-run args))))
      (let ((calls (emacs-agent-test-fake-calls "client")))
        (expect (length calls) :to-equal 9)
        (dolist (argv calls)
          (let ((socket (emacs-agent-test-socket-arg argv)))
            (expect (cons argv (and socket (file-name-absolute-p socket)
                                    (string-prefix-p emacs-agent-test-socket-dir socket)))
                    :to-equal (cons argv t)))))))

  (it "exposes call, call-write and eval as distinct leading words and never sends a raw form for a call (Scenario: agent-target-policy § Tiers are distinguishable by permission rules)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (wrapper-spec--with-reply
       "{\"ok\":true,\"result\":\"x\"}"
       (lambda ()
         (emacs-agent-test-wrapper-run "live" "call" "org_graph_query" "{}")
         (emacs-agent-test-wrapper-run "live" "call-write" "org_graph_write_node" "{}")
         (emacs-agent-test-wrapper-run "live" "eval" "(+ 1 2)")
         ;; A form in the tool position is a tool name, never evaluated.
         (emacs-agent-test-wrapper-run "live" "call" "(kill-emacs)" "{}")))
      (expect (mapcar #'emacs-agent-test-sent-form (emacs-agent-test-fake-calls "client"))
              :to-equal '((jf/agent-call "call" "org_graph_query" "{}")
                          (jf/agent-call "call-write" "org_graph_write_node" "{}")
                          (jf/agent-eval "(+ 1 2)")
                          (jf/agent-call "call" "(kill-emacs)" "{}")))
      ;; No other spelling reaches a tier.
      (dolist (args '(("live" "call-writer" "x" "{}")
                      ("live" "evaluate" "(+ 1 1)")
                      ("live" "--tier=eval" "(+ 1 1)")))
        (expect (wrapper-spec--refused-p (apply #'emacs-agent-test-wrapper-run args))
                :to-be t))
      (expect (length (emacs-agent-test-fake-calls "client")) :to-equal 4)))

  (it "prints the target's manifest as JSON for tools (Scenario: agent-target-policy § Listing tools through the wrapper)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (let ((result (wrapper-spec--with-reply
                     "[{\"name\":\"org_graph_query\",\"tier\":\"read\"}]"
                     (lambda () (emacs-agent-test-wrapper-run "live" "tools")))))
        (expect (plist-get result :exit) :to-equal 0)
        (let ((manifest (json-parse-string (plist-get result :stdout) :object-type 'plist)))
          (expect (plist-get (aref manifest 0) :name) :to-equal "org_graph_query"))
        (expect (memq 'jf/agent-api
                      (flatten-tree (emacs-agent-test-sent-form
                                     (car (emacs-agent-test-fake-calls "client")))))
                :not :to-be nil))))

  (it "carries JSON with quotes, backslashes, newlines and non-ASCII to Emacs unchanged (seam/wrapper-cli)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (let ((json "{\"body\":\"line 1\nline \\\"2\\\" \\\\ é\",\"tags\":[\"a b\"]}"))
        (wrapper-spec--with-reply
         "{\"ok\":true,\"result\":\"x\"}"
         (lambda () (emacs-agent-test-wrapper-run
                     "live" "call-write" "org_graph_write_node" json)))
        (expect (nth 3 (emacs-agent-test-sent-form
                        (car (emacs-agent-test-fake-calls "client"))))
                :to-equal json))))

  (it "exits 0 on a success envelope and 1 on a failure envelope, printing it decoded (seam/wrapper-cli)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (let ((ok (wrapper-spec--with-reply
                 "{\"ok\":true,\"result\":\"a \\\"b\\\"\"}"
                 (lambda () (emacs-agent-test-wrapper-run "live" "call" "t" "{}"))))
            (failed (wrapper-spec--with-reply
                     "{\"ok\":false,\"error\":\"user-error Nope\"}"
                     (lambda () (emacs-agent-test-wrapper-run "live" "call" "t" "{}")))))
        (expect (plist-get ok :exit) :to-equal 0)
        (expect (plist-get (json-parse-string (plist-get ok :stdout) :object-type 'plist)
                           :result)
                :to-equal "a \"b\"")
        (expect (plist-get failed :exit) :to-equal 1)
        (expect (plist-get (json-parse-string (plist-get failed :stdout) :object-type 'plist)
                           :error)
                :to-equal "user-error Nope")))))


(describe "bin/emacs-agent target start (seam/target-lifecycle, seam/data-profile)"

  (it "runs the worktree's emacs-isolated.sh --daemon=NAME with the profile exported verbatim (seam/target-lifecycle)"
    (wrapper-spec--scope
      (emacs-agent-test-wrapper-run "target" "start" "smoke-1"
                                    "--worktree" (emacs-agent-test-fake-worktree)
                                    "--data" "scratch:/tmp/emacs-agent-vault")
      (let ((calls (emacs-agent-test-fake-calls "isolated")))
        (expect (length calls) :to-equal 1)
        (expect (member "--daemon=smoke-1" (car calls)) :not :to-be nil))
      (expect (emacs-agent-test-fake-env "isolated")
              :to-equal '("scratch:/tmp/emacs-agent-vault"))))

  (it "exports none when --data is omitted (seam/data-profile)"
    (wrapper-spec--scope
      (let ((process-environment (cons "EMACS_AGENT_DATA=live" process-environment)))
        (emacs-agent-test-wrapper-run "target" "start" "smoke-1"
                                      "--worktree" (emacs-agent-test-fake-worktree)))
      (expect (emacs-agent-test-fake-env "isolated") :to-equal '("none"))))

  (it "runs EMACS -Q --daemon=NAME for a bare target (seam/target-lifecycle)"
    (wrapper-spec--scope
      (emacs-agent-test-wrapper-run "target" "start" "smoke-2" "--bare")
      (let ((argv (car (emacs-agent-test-fake-calls "emacs"))))
        (expect (member "-Q" argv) :not :to-be nil)
        (expect (member "--daemon=smoke-2" argv) :not :to-be nil))
      (expect (emacs-agent-test-fake-calls "isolated") :to-be nil)))

  (it "states the default data profile in its usage text (seam/data-profile)"
    (let ((help (emacs-agent-test-wrapper-run "--help")))
      (expect (plist-get help :exit) :to-equal 0)
      (expect (plist-get help :stdout) :to-match "--data")
      (expect (plist-get help :stdout) :to-match "scratch:")
      (expect (plist-get help :stdout) :to-match "default[^\n]*none\\|none[^\n]*default"))))

;;; wrapper-spec.el ends here
