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

(defun wrapper-spec--with-reply (text thunk &optional pad)
  "Call THUNK with the fake emacsclient answering the string TEXT.
The fake prints TEXT as emacsclient prints the reply of a form the
wrapper sends: the base64 of TEXT's UTF-8 as a printed string, ending
in the pad character with PAD (`emacs-agent-test-printed-reply')."
  (wrapper-spec--with-raw-reply (emacs-agent-test-printed-reply text pad) thunk))

(defun wrapper-spec--with-raw-reply (output thunk)
  "Call THUNK with the fake emacsclient printing OUTPUT as it is."
  (let ((process-environment (cons (concat "FAKE_EMACSCLIENT_REPLY=" output)
                                   process-environment)))
    (funcall thunk)))

(defun wrapper-spec--subforms (form)
  "FORM, when it is a list, and every list nested in it."
  (when (consp form)
    (cons form (mapcan #'wrapper-spec--subforms (seq-filter #'consp form)))))

(defun wrapper-spec--refused-p (result)
  "Non-nil when RESULT is a usage refusal: exit 2 with a message."
  (and (eq (plist-get result :exit) 2)
       (not (string-empty-p (string-trim (plist-get result :stderr))))))

(defun wrapper-spec--nothing-started-p ()
  "Non-nil when no fake binary was run."
  (and (null (emacs-agent-test-fake-calls "client"))
       (null (emacs-agent-test-fake-calls "emacs"))
       (null (emacs-agent-test-fake-calls "isolated"))))


(defun wrapper-spec--stand-in-at (name)
  "Stand a listening socket where the target name NAME would resolve.
NAME is taken relative to the socket directory, `..' and subdirectories
included, so a wrapper that let NAME through would find a socket there
and run emacsclient against it.  Return the socket's path."
  (let* ((path (expand-file-name name emacs-agent-test-socket-dir))
         (relative (file-relative-name path emacs-agent-test-socket-dir)))
    (make-directory (file-name-directory path) t)
    (emacs-agent-test-stand-in-socket relative)))

(defun wrapper-spec--with-real-emacsclient (thunk)
  "Call THUNK with the real emacsclient behind the wrapper, the fake Emacs kept.
For a socket that must really be connected to, while nothing the wrapper
launches can start an Emacs."
  (let ((process-environment (cons (concat "EMACS_AGENT_EMACSCLIENT="
                                           (emacs-agent-test-emacsclient))
                                   process-environment)))
    (funcall thunk)))


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
          (expect (emacs-agent-test-sent-payload (car calls))
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

  (it "refuses a target name that is server in another case or not a plain socket name, running nothing, though a socket waits where it resolves (Scenario: isolated-emacs-targets § Live socket name is refused)"
    (dolist (name (list "Server" "SERVER" "sErVeR"
                        (format "../emacs%d/x" (user-uid))
                        "x/y" ".hidden" "-x" "a b"))
      ;; One scope per name: on a case-insensitive file system the three
      ;; spellings of server are one socket.
      (wrapper-spec--scope
        (wrapper-spec--stand-in-at name)
        (dolist (args `(("target" ,name "tools")
                        ("target" ,name "eval" "(+ 1 1)")
                        ("target" "start" ,name "--bare")
                        ("target" "start" ,name "--worktree"
                         ,(emacs-agent-test-fake-worktree))
                        ("target" "stop" ,name)))
          (let ((result (apply #'emacs-agent-test-wrapper-run args)))
            (expect (cons args (wrapper-spec--refused-p result))
                    :to-equal (cons args t))))
        (expect (cons name (wrapper-spec--nothing-started-p))
                :to-equal (cons name t)))))

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

  (it "exposes call, call-write and eval as distinct leading words and never sends a raw form for a call, each sent form wrapped in the reply encoder (Scenario: agent-target-policy § Tiers are distinguishable by permission rules)"
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
      ;; The tier's form is what the encoder evaluates, never the bare
      ;; form: `emacs-agent-test-sent-payload' is nil for anything else.
      (expect (mapcar #'emacs-agent-test-sent-payload (emacs-agent-test-fake-calls "client"))
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

  (it "sends a target eval form that starts with - as a string literal inside the form after --eval, never as an emacsclient option (seam/wrapper-cli)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket "smoke-1")
      (let ((forms '("--socket-name=server" "--alternate-editor=emacs" "(+ 1 2)")))
        (dolist (form forms)
          (expect (plist-get (emacs-agent-test-wrapper-run "target" "smoke-1" "eval" form)
                             :exit)
                  :to-equal 0))
        (let ((calls (emacs-agent-test-fake-calls "client")))
          (expect (length calls) :to-equal (length forms))
          (cl-loop for argv in calls
                   for form in forms
                   do (expect (seq-filter (lambda (arg) (string-prefix-p "--" arg)) argv)
                              :to-equal '("--eval"))
                      (expect (emacs-agent-test-socket-arg argv)
                              :to-equal (emacs-agent-test-socket "smoke-1"))
                      ;; The argument after --eval is a form, the last one.
                      (expect (cadr (member "--eval" argv)) :to-be (car (last argv)))
                      (expect (substring (car (last argv)) 0 1) :to-equal "(")
                      ;; The caller's form arrives as a string literal, read
                      ;; and evaluated in Emacs, its value printed with pp.
                      (let ((payload (emacs-agent-test-sent-payload argv)))
                        (expect (member (list 'read-from-string form)
                                        (wrapper-spec--subforms payload))
                                :not :to-be nil)
                        (expect (memq 'pp (flatten-tree payload)) :not :to-be nil)))))))

  (it "keeps the caller's EMACS_SOCKET_NAME, ALTERNATE_EDITOR and EMACS_SERVER_FILE from emacsclient and Emacs (seam/wrapper-cli)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (emacs-agent-test-stand-in-socket "smoke-1")
      (let ((process-environment
             (append (list (concat "EMACS_SOCKET_NAME=" (emacs-agent-test-socket "server"))
                           "ALTERNATE_EDITOR=emacs"
                           (concat "EMACS_SERVER_FILE=" (emacs-agent-test-socket "server")))
                     process-environment)))
        (dolist (args `(("live" "tools")
                        ("live" "call" "org_graph_query" "{}")
                        ("target" "smoke-1" "tools")
                        ("target" "smoke-1" "eval" "(+ 1 1)")
                        ("target" "start" "smoke-2" "--bare")
                        ("target" "start" "smoke-3" "--worktree"
                         ,(emacs-agent-test-fake-worktree))))
          (apply #'emacs-agent-test-wrapper-run args)))
      (let ((unset (mapcar (lambda (var) (cons var "<unset>"))
                           emacs-agent-test-fallback-variables))
            (seen (append (emacs-agent-test-fake-vars "client")
                          (emacs-agent-test-fake-vars "emacs")
                          (emacs-agent-test-fake-vars "isolated"))))
        (expect (length (emacs-agent-test-fake-vars "client")) :to-equal 4)
        (expect (length seen) :to-equal 6)
        (dolist (vars seen)
          (expect vars :to-equal unset)))))

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
        (expect (nth 3 (emacs-agent-test-sent-payload
                        (car (emacs-agent-test-fake-calls "client"))))
                :to-equal json))))

  (it "exits 0 on a success envelope and 1 on a failure envelope, printing it decoded from base64 (seam/wrapper-cli)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      ;; `wrapper-spec--with-reply' answers the envelope as base64.
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
                :to-equal "user-error Nope"))))

  (it "drops the pad from a base64 reply and prints the bytes it carries, a target eval value byte for byte (seam/wrapper-cli)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (emacs-agent-test-stand-in-socket "smoke-1")
      (let ((coding-system-for-read 'utf-8)
            (envelope "{\"ok\":true,\"result\":\"caf\u00e9 \u2014 a-b & c\"}")
            ;; pp's text of a value: it ends in a newline, printed as it is.
            (value "(\"caf\u00e9\" \"a\nb\")\n"))
        (dolist (pad '(nil t))
          (let ((call (wrapper-spec--with-reply
                       envelope
                       (lambda () (emacs-agent-test-wrapper-run "live" "call" "t" "{}"))
                       pad))
                (eval (wrapper-spec--with-reply
                       value
                       (lambda () (emacs-agent-test-wrapper-run "target" "smoke-1" "eval" "x"))
                       pad)))
            (expect (list pad (plist-get call :exit) (plist-get call :stdout))
                    :to-equal (list pad 0 (concat envelope "\n")))
            (expect (list pad (plist-get eval :exit) (plist-get eval :stdout))
                    :to-equal (list pad 0 value)))))))

  (it "exits 1 on a reply that is not the base64 string it asked for, printing it undecoded (seam/wrapper-cli)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (emacs-agent-test-stand-in-socket "smoke-1")
      ;; The printed JSON an encoder-less form would answer, text that is
      ;; not base64, and base64 of a length no encoding gives.
      (dolist (output '("\"{\\\"ok\\\":true}\"" "\"not base64\"" "\"QUJD.\"x" "\"QUJ\""))
        (dolist (args '(("live" "call" "t" "{}")
                        ("target" "smoke-1" "eval" "x")))
          (let ((result (wrapper-spec--with-raw-reply
                         output (lambda () (apply #'emacs-agent-test-wrapper-run args)))))
            (expect (list output args (plist-get result :exit))
                    :to-equal (list output args 1))
            (expect (plist-get result :stdout) :to-equal (concat output "\n"))
            (expect (plist-get result :stderr) :to-match "not decoded"))))))

  (it "exits 1 on emacsclient output holding \"*ERROR*: Unknown message\" on every path, target eval included, printing it (seam/wrapper-cli)"
    (wrapper-spec--scope
      (emacs-agent-test-stand-in-socket)
      (emacs-agent-test-stand-in-socket "smoke-1")
      ;; How emacsclient shows a reply whose messages it misread.
      (let ((output (concat "\"" (make-string 1000 ?Q)
                            "\n*ERROR*: Unknown message: -"
                            "\n*ERROR*: Unknown message: print-nonl QQQQ\"")))
        (dolist (args '(("live" "call" "t" "{}")
                        ("live" "call-write" "t" "{}")
                        ("live" "eval" "(+ 1 1)")
                        ("live" "tools")
                        ("target" "smoke-1" "eval" "(+ 1 1)")
                        ("target" "smoke-1" "state")))
          (let ((result (wrapper-spec--with-raw-reply
                         output (lambda () (apply #'emacs-agent-test-wrapper-run args)))))
            (expect (cons args (plist-get result :exit)) :to-equal (cons args 1))
            (expect (plist-get result :stdout) :to-match "Unknown message: -")
            (expect (plist-get result :stderr) :to-match "garbled")))))))


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

  (it "refuses to start a target whose socket a process holds while refusing connections, launching nothing and keeping the socket (seam/target-lifecycle)"
    (assume (emacs-agent-test-binaries-available-p)
            "no emacsclient to connect to the refusing socket with")
    (assume (emacs-agent-test-refusing-socket-available-p)
            "no perl or lsof to hold a refusing socket")
    ;; The fake Emacs and launcher: a real one would take the socket over
    ;; and outlive the scope, whose teardown never contacts that socket.
    (wrapper-spec--scope
      (let ((socket (emacs-agent-test-refusing-socket "smoke-busy")))
        (wrapper-spec--with-real-emacsclient
         (lambda ()
           (dolist (args `(("target" "start" "smoke-busy" "--bare")
                           ("target" "start" "smoke-busy" "--worktree"
                            ,(emacs-agent-test-fake-worktree))))
             (let ((result (apply #'emacs-agent-test-wrapper-run args)))
               (expect (cons args (plist-get result :exit)) :to-equal (cons args 2))
               (expect (plist-get result :stderr)
                       :to-match (regexp-quote "target smoke-busy is already running (busy)"))))))
        (expect (emacs-agent-test-fake-calls "emacs") :to-be nil)
        (expect (emacs-agent-test-fake-calls "isolated") :to-be nil)
        (expect (file-exists-p socket) :to-be t))))

  (it "states the default data profile in its usage text (seam/data-profile)"
    (let ((help (emacs-agent-test-wrapper-run "--help")))
      (expect (plist-get help :exit) :to-equal 0)
      (expect (plist-get help :stdout) :to-match "--data")
      (expect (plist-get help :stdout) :to-match "scratch:")
      (expect (plist-get help :stdout) :to-match "default[^\n]*none\\|none[^\n]*default"))))

;;; wrapper-spec.el ends here
