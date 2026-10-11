;;; helpers-spec.el --- Shared fixtures for the emacs-agent specs -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md § Seams and
;; § Scenario coverage.
;;
;; Every fixture the emacs-agent specs share has its one home here; a spec
;; file does not define its own copy.  The specs under unit/ and behavioral/
;; load this file by path before their first `describe':
;;
;;   (require 'emacs-agent-test-helpers
;;            (expand-file-name "../helpers-spec.el"
;;                              (file-name-directory
;;                               (or load-file-name buffer-file-name))))
;;
;; The runner loads it once more as a spec of its own (its name ends in
;; -spec.el), after the subdirectories; that load redefines the same
;; functions and runs the self-tests at the bottom.
;;
;; Nothing here may reach the user's live Emacs.  Every subprocess a
;; fixture starts (Emacs, emacsclient, bin/emacs-agent) runs inside
;; `emacs-agent-test-with-socket-dir', which points TMPDIR at a fresh
;; private directory and removes XDG_RUNTIME_DIR, so each of them resolves
;; its socket directory inside it; the macro refuses to run its body when
;; that directory is the test process's own `server-socket-dir'.  The
;; private directory is short (under `temporary-file-directory') because a
;; server socket's path must fit the platform's limit: a long TMPDIR makes
;; `server-start' fail with "Service name too long".
;;
;; The module under test, config/emacs-agent/, does not exist at the design
;; round.  `emacs-agent-test-load-module' loads its loader when the file
;; exists and the feature is absent, and does nothing otherwise, so every
;; spec file loads cleanly before the module is written.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'server)
(require 'gptel nil t)

(defvar gptel--known-tools)
(defvar gptel--known-presets)


;;; Paths and binaries

(defconst emacs-agent-test-root
  (file-name-as-directory
   (or (locate-dominating-file
        (file-name-directory (or load-file-name buffer-file-name))
        "early-init.el")
       (and (boundp 'jf/emacs-dir) jf/emacs-dir)))
  "Root of the configuration checkout under test (the code root).")

(defconst emacs-agent-test-wrapper
  (expand-file-name "bin/emacs-agent" emacs-agent-test-root)
  "The wrapper under test, `bin/emacs-agent' of this checkout.")

(defun emacs-agent-test-emacs ()
  "The Emacs binary running the tests."
  (expand-file-name invocation-name invocation-directory))

(defun emacs-agent-test-emacsclient ()
  "The emacsclient that belongs to the running Emacs, or one on PATH."
  (seq-find #'file-executable-p
            (list (expand-file-name "bin/emacsclient" invocation-directory)
                  (expand-file-name "emacsclient" invocation-directory)
                  (or (executable-find "emacsclient") ""))))

(defun emacs-agent-test-binaries-available-p ()
  "Non-nil when an Emacs binary and its emacsclient are available.
Real-process specs `assume' this, so a machine without them skips them."
  (and (file-executable-p (emacs-agent-test-emacs))
       (emacs-agent-test-emacsclient)
       t))


;;; The module under test

(defun emacs-agent-test-load-module ()
  "Load config/emacs-agent/emacs-agent.el when it exists and is not loaded.
The test process has loaded init.el; once the module is in
`jf/enabled-modules' it is loaded already and this does nothing."
  (let ((loader (expand-file-name "config/emacs-agent/emacs-agent.el"
                                  emacs-agent-test-root)))
    (when (and (not (featurep 'emacs-agent)) (file-exists-p loader))
      (load loader nil t))))

(emacs-agent-test-load-module)

(defun emacs-agent-test-envelope (json)
  "Parse the dispatcher's JSON envelope JSON into a plist.
JSON true reads as t, false and null as nil."
  (json-parse-string json :object-type 'plist
                     :false-object nil :null-object nil))


;;; Stub gptel tools in the agent presets

(defun emacs-agent-test-arg (name &optional optional type)
  "A gptel tool argument spec named NAME, of TYPE (default string)."
  (append (list :name name :type (or type 'string)
                :description (format "Argument %s." name))
          (and optional (list :optional t))))

(defun emacs-agent-test--extend-preset (preset names)
  "Re-register PRESET with NAMES appended to its :tools."
  (let ((plist (copy-sequence (gptel-get-preset preset))))
    (apply #'gptel-make-preset preset
           (plist-put plist :tools
                      (append (plist-get plist :tools) names)))))

(defun emacs-agent-test--install-tools (specs)
  "Register the stub tools SPECS and add them to the agent presets.
Each spec is a plist: :name, :function, :args, :async, :description and
:preset (`read', `write' or nil for neither).  With :register nil the
name goes into the preset but no tool is registered."
  (let (read write)
    (dolist (spec specs)
      (let ((name (plist-get spec :name)))
        (unless (and (plist-member spec :register)
                     (not (plist-get spec :register)))
          (gptel-make-tool :name name
                           :function (or (plist-get spec :function) #'ignore)
                           :description (or (plist-get spec :description)
                                            "Stub tool of the emacs-agent specs.")
                           :args (plist-get spec :args)
                           :async (plist-get spec :async)
                           :category "emacs-agent-test"))
        (pcase (plist-get spec :preset)
          ('read (push name read))
          ('write (push name write)))))
    (when read
      (emacs-agent-test--extend-preset 'emacs-agent-read (nreverse read)))
    (when write
      (emacs-agent-test--extend-preset 'emacs-agent-write (nreverse write)))))

(defmacro emacs-agent-test-with-tools (specs &rest body)
  "Run BODY with the stub tools SPECS registered and placed in the presets.
gptel's registry and preset table are bound to copies for BODY, so a
stub tool or a preset change never outlives it.  SPECS is evaluated; see
`emacs-agent-test--install-tools' for its shape."
  (declare (indent 1))
  `(let ((gptel--known-tools (copy-tree gptel--known-tools))
         (gptel--known-presets (copy-tree gptel--known-presets)))
     (emacs-agent-test--install-tools ,specs)
     ,@body))


;;; A private socket directory

(defvar emacs-agent-test-socket-dir nil
  "Socket directory of the current `emacs-agent-test-with-socket-dir' scope.")

(defvar emacs-agent-test--tmpdir nil
  "The private TMPDIR of the current scope.")

(defvar emacs-agent-test--pids nil
  "Pids of the Emacs processes the current scope started.")

(defvar emacs-agent-test--procs nil
  "Processes of the current scope (batch servers, stand-in sockets).")

(defvar emacs-agent-test--fake-sockets nil
  "Socket files of the current scope that no Emacs serves.")

(defun emacs-agent-test--assert-private-socket-dir ()
  "Signal unless the scope's socket directory is private to the scope."
  (let ((mine (file-name-as-directory
               (expand-file-name emacs-agent-test-socket-dir)))
        (real (and (boundp 'server-socket-dir) server-socket-dir
                   (file-name-as-directory
                    (expand-file-name server-socket-dir)))))
    (when (or (equal mine real)
              (not (string-prefix-p (expand-file-name emacs-agent-test--tmpdir)
                                    mine)))
      (error "Refusing to run: socket directory %s is not private" mine))
    (unless (equal (getenv "TMPDIR") emacs-agent-test--tmpdir)
      (error "Refusing to run: TMPDIR is not the private directory"))
    (when (getenv "XDG_RUNTIME_DIR")
      (error "Refusing to run: XDG_RUNTIME_DIR is set"))))

(defun emacs-agent-test-socket (name)
  "Absolute path of the socket NAME in the scope's socket directory."
  (expand-file-name name emacs-agent-test-socket-dir))

(defun emacs-agent-test--teardown ()
  "Stop everything the current scope started; touch nothing else."
  (dolist (proc emacs-agent-test--procs)
    (ignore-errors (delete-process proc)))
  (dolist (pid emacs-agent-test--pids)
    (ignore-errors (signal-process pid 'kill)))
  ;; Targets started through the wrapper: ask each socket left in the
  ;; private directory to exit.  Only that directory is read.
  (when (and (file-directory-p emacs-agent-test-socket-dir)
             (emacs-agent-test-emacsclient))
    (dolist (sock (directory-files emacs-agent-test-socket-dir t "\\`[^.]"))
      (unless (member sock emacs-agent-test--fake-sockets)
        (ignore-errors
          (call-process (emacs-agent-test-emacsclient) nil nil nil
                        "-w" "3" "-s" sock "--eval" "(kill-emacs)")))))
  (ignore-errors (delete-directory emacs-agent-test--tmpdir t)))

(defmacro emacs-agent-test-with-socket-dir (&rest body)
  "Run BODY with every subprocess confined to a private socket directory.
Binds `emacs-agent-test-socket-dir' to TMPDIR/emacs<uid> of a fresh,
short temp directory, puts that TMPDIR in `process-environment' and
removes XDG_RUNTIME_DIR from it.  On exit kills every Emacs the scope
started and deletes the directory."
  (declare (indent 0))
  `(let* ((emacs-agent-test--tmpdir
           (file-name-as-directory (make-temp-file "ea" t)))
          (emacs-agent-test-socket-dir
           (expand-file-name (format "emacs%d" (user-uid))
                             emacs-agent-test--tmpdir))
          (emacs-agent-test--pids nil)
          (emacs-agent-test--procs nil)
          (emacs-agent-test--fake-sockets nil)
          (process-environment
           (cons (concat "TMPDIR=" emacs-agent-test--tmpdir)
                 (seq-remove (lambda (entry)
                               (or (string-prefix-p "TMPDIR=" entry)
                                   (string-prefix-p "XDG_RUNTIME_DIR=" entry)))
                             process-environment))))
     (unwind-protect
         (progn
           (emacs-agent-test--assert-private-socket-dir)
           ,@body)
       (emacs-agent-test--teardown))))


;;; Emacs processes in the private directory

(defun emacs-agent-test-client (name form)
  "Evaluate FORM (a string) in the Emacs serving socket NAME.
Uses the real emacsclient against the private directory.  Return
\(EXIT . OUTPUT), OUTPUT holding stdout and stderr, trimmed."
  (with-temp-buffer
    (let ((exit (call-process (emacs-agent-test-emacsclient) nil t nil
                              "-w" "10" "-s" (emacs-agent-test-socket name)
                              "--eval" form)))
      (cons exit (string-trim (buffer-string))))))

(defun emacs-agent-test-start-daemon (name)
  "Start `emacs -Q --daemon=NAME' in the private directory; return its pid.
Started directly, not through the wrapper, for the properties the design
takes from the operating system (D2, D4)."
  (let ((exit (call-process (emacs-agent-test-emacs) nil nil nil
                            "-Q" (concat "--daemon=" name))))
    (unless (eq exit 0)
      (error "emacs --daemon=%s exited %S" name exit))
    (let ((pid (string-to-number
                (cdr (emacs-agent-test-client name "(emacs-pid)")))))
      (push pid emacs-agent-test--pids)
      pid)))

(defun emacs-agent-test-wait-for (predicate &optional seconds)
  "Poll PREDICATE every 0.1s for up to SECONDS (default 10); return its value."
  (let ((deadline (+ (float-time) (or seconds 10)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    value))

(defun emacs-agent-test-start-batch-server (name)
  "Start a non-daemon `emacs -Q --batch' that runs `server-start' as NAME.
Return its process once the socket exists."
  (let ((proc (make-process
               :name (concat "emacs-agent-test-" name)
               :command (list (emacs-agent-test-emacs) "-Q" "--batch" "--eval"
                              (format "(progn (setq server-name %S) (server-start) (while t (sleep-for 0.2)))"
                                      name))
               :buffer nil :noquery t :connection-type 'pipe)))
    (push proc emacs-agent-test--procs)
    (unless (emacs-agent-test-wait-for
             (lambda () (file-exists-p (emacs-agent-test-socket name))))
      (error "Batch server %s did not create its socket" name))
    proc))

(defun emacs-agent-test-stand-in-socket (&optional name)
  "Listen on NAME (default `server') in the private directory.
A listening socket with no Emacs behind it: it stands in for a running
target (or, as `server', the live session) for the wrapper's socket
checks while the fake emacsclient, which never connects, takes the call.
Return the socket's path."
  (make-directory emacs-agent-test-socket-dir t)
  (set-file-modes emacs-agent-test-socket-dir #o700)
  (let ((path (emacs-agent-test-socket (or name "server"))))
    (push (make-network-process :name (concat "emacs-agent-test-stand-in-"
                                              (or name "server"))
                                :family 'local :service path
                                :server t :noquery t)
          emacs-agent-test--procs)
    (push path emacs-agent-test--fake-sockets)
    path))

(defmacro emacs-agent-test-with-real-binaries (&rest body)
  "Run BODY with the wrapper pointed at the real Emacs and emacsclient.
The `emacs' on PATH may not be the binary under test, so the wrapper is
given both explicitly.  Use inside `emacs-agent-test-with-socket-dir'."
  (declare (indent 0))
  `(let ((process-environment
          (append (list (concat "EMACS_AGENT_EMACSCLIENT="
                                (emacs-agent-test-emacsclient))
                        (concat "EMACS_AGENT_EMACS=" (emacs-agent-test-emacs)))
                  process-environment)))
     ,@body))


;;; Running the wrapper, with fake binaries behind it

(defvar emacs-agent-test-fake-dir nil
  "Directory of the fake binaries of the current fake-clients scope.")

(defun emacs-agent-test--write-fake (file kind)
  "Write the fake binary FILE (relative to the fake dir) recording as KIND.
It records its argv, NUL-separated, in calls/KIND-NNNN and the value of
EMACS_AGENT_DATA in calls/KIND-NNNN.env, and starts nothing.  The fake
emacsclient prints FAKE_EMACSCLIENT_REPLY (default the Lisp string
\"ok\") and exits FAKE_EMACSCLIENT_EXIT (default 0)."
  (let ((path (expand-file-name file emacs-agent-test-fake-dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert "#!/bin/sh\n"
              "d=\"" (expand-file-name "calls" emacs-agent-test-fake-dir) "\"\n"
              "mkdir -p \"$d\"\n"
              "n=$(ls \"$d\" | wc -l | tr -d ' ')\n"
              "f=\"$d/" kind "-$(printf %05d \"$n\")\"\n"
              "printf '%s\\0' \"$@\" > \"$f\"\n"
              "printf '%s' \"${EMACS_AGENT_DATA-<unset>}\" > \"$f.env\"\n"
              (if (equal kind "client")
                  (concat "printf '%s\\n' \"${FAKE_EMACSCLIENT_REPLY-\\\"ok\\\"}\"\n"
                          "exit \"${FAKE_EMACSCLIENT_EXIT:-0}\"\n")
                "exit 0\n")))
    (set-file-modes path #o755)
    path))

(defmacro emacs-agent-test-with-fake-clients (&rest body)
  "Run BODY with fake emacsclient and Emacs binaries behind the wrapper.
EMACS_AGENT_EMACSCLIENT and EMACS_AGENT_EMACS name recording scripts in
`emacs-agent-test-fake-dir' (see `emacs-agent-test--write-fake');
`emacs-agent-test-fake-worktree' is a checkout whose
bin/emacs-isolated.sh records the same way.  Use inside
`emacs-agent-test-with-socket-dir'."
  (declare (indent 0))
  `(let* ((emacs-agent-test-fake-dir
           (file-name-as-directory (make-temp-file "ea-fake" t)))
          (process-environment
           (append
            (list (concat "EMACS_AGENT_EMACSCLIENT="
                          (emacs-agent-test--write-fake "emacsclient" "client"))
                  (concat "EMACS_AGENT_EMACS="
                          (emacs-agent-test--write-fake "emacs" "emacs")))
            process-environment)))
     (emacs-agent-test--write-fake "worktree/bin/emacs-isolated.sh" "isolated")
     (unwind-protect (progn ,@body)
       (delete-directory emacs-agent-test-fake-dir t))))

(defun emacs-agent-test-fake-worktree ()
  "The fake checkout of the current fake-clients scope."
  (expand-file-name "worktree" emacs-agent-test-fake-dir))

(defun emacs-agent-test-fake-calls (kind)
  "Argv lists the fake KIND (\"client\", \"emacs\", \"isolated\") received, oldest first."
  (let ((dir (expand-file-name "calls" emacs-agent-test-fake-dir)))
    (when (file-directory-p dir)
      (mapcar (lambda (file)
                (split-string (with-temp-buffer
                                (insert-file-contents-literally file)
                                (decode-coding-string (buffer-string) 'utf-8))
                              "\0" t))
              (directory-files dir t (concat "\\`" kind "-[0-9]+\\'"))))))

(defun emacs-agent-test-fake-env (kind)
  "EMACS_AGENT_DATA as each call of the fake KIND saw it, oldest first."
  (let ((dir (expand-file-name "calls" emacs-agent-test-fake-dir)))
    (when (file-directory-p dir)
      (mapcar (lambda (file)
                (with-temp-buffer (insert-file-contents file) (buffer-string)))
              (directory-files dir t (concat "\\`" kind "-[0-9]+\\.env\\'"))))))

(defun emacs-agent-test-wrapper-run (&rest args)
  "Run bin/emacs-agent with ARGS; return (:exit N :stdout S :stderr S)."
  (let ((err (make-temp-file "ea-stderr")))
    (unwind-protect
        (with-temp-buffer
          (let ((exit (apply #'call-process emacs-agent-test-wrapper nil
                             (list t err) nil args)))
            (list :exit exit
                  :stdout (buffer-string)
                  :stderr (with-temp-buffer
                            (insert-file-contents err)
                            (buffer-string)))))
      (delete-file err))))

(defun emacs-agent-test-sent-form (argv)
  "The form an emacsclient ARGV carries after --eval, read, or nil."
  (let ((tail (member "--eval" argv)))
    (and tail (cadr tail) (car (read-from-string (cadr tail))))))

(defun emacs-agent-test-socket-arg (argv)
  "The value ARGV passes with -s (or --socket-name=), or nil."
  (or (cadr (member "-s" argv))
      (seq-some (lambda (arg)
                  (and (string-prefix-p "--socket-name=" arg)
                       (substring arg (length "--socket-name="))))
                argv)))


;;; A child Emacs that loads this checkout's whole configuration

(defun emacs-agent-test-home ()
  "The fake HOME of the current scope, created on first use.
A child under it expands `~/org' inside the scope, never to the user's
vault: a path that does not follow the data profile lands here, where a
spec can look for it."
  (let ((home (expand-file-name "home/" emacs-agent-test--tmpdir)))
    (make-directory home t)
    home))

(defun emacs-agent-test-child-init (profile form)
  "Load the full configuration in a child `emacs --batch', then print FORM.
PROFILE is the value of EMACS_AGENT_DATA for the child, or nil to leave
it unset.  FORM is a string, read and evaluated after init.el.  The
child runs with HOME at `emacs-agent-test-home' and the private TMPDIR.
Return (:exit N :value VALUE :output TEXT), VALUE read from what the
child printed after the marker line, or nil.
Paths are expanded before HOME is rebound: `expand-file-name' reads
HOME from `process-environment', so a `~/' path (an abbreviated
`default-directory' or checkout root) would otherwise resolve under
the fake HOME."
  (let* ((root (expand-file-name emacs-agent-test-root))
         (default-directory root)
         (process-environment
          (append
           (list (concat "HOME=" (directory-file-name (emacs-agent-test-home)))
                 (concat "EMACS_USER_DIRECTORY="
                         (expand-file-name "runtime" root)))
           (and profile (list (concat "EMACS_AGENT_DATA=" profile)))
           (seq-remove (lambda (entry)
                         (or (string-prefix-p "HOME=" entry)
                             (string-prefix-p "EMACS_AGENT_DATA=" entry)
                             (string-prefix-p "EMACS_USER_DIRECTORY=" entry)))
                       process-environment))))
    (with-temp-buffer
      (let* ((exit (call-process
                    (emacs-agent-test-emacs) nil t nil
                    "-q" "--load" (expand-file-name "early-init.el" root)
                    "--load" (expand-file-name "init.el" root)
                    "-batch" "--eval"
                    (format "(progn (setq kill-emacs-hook nil) (princ \"\\n@@emacs-agent-test@@\\n\") (prin1 (eval (car (read-from-string %S)) t)) (terpri))"
                            form)))
             (output (buffer-string))
             (marker (string-search "@@emacs-agent-test@@\n" output))
             (value (and marker
                         (ignore-errors
                           (car (read-from-string
                                 output (+ marker (length "@@emacs-agent-test@@\n"))))))))
        (list :exit exit :value value :output output)))))


;;; Self-tests: the fixtures the passing specs rely on
;;
;; Registered on the first load only: a spec's `require' loads this file
;; first and the runner loads it again as a spec, after the feature is
;; provided.

(unless (featurep 'emacs-agent-test-helpers)
(describe "emacs-agent test helpers"

  (it "confines a scope to a private socket directory and removes it afterwards"
    (let (dir)
      (emacs-agent-test-with-socket-dir
        (setq dir emacs-agent-test--tmpdir)
        (expect (getenv "TMPDIR") :to-equal emacs-agent-test--tmpdir)
        (expect (getenv "XDG_RUNTIME_DIR") :to-be nil)
        (expect (string-prefix-p emacs-agent-test--tmpdir emacs-agent-test-socket-dir)
                :to-be t)
        (expect (file-name-as-directory emacs-agent-test-socket-dir)
                :not :to-equal (file-name-as-directory server-socket-dir)))
      (expect (file-exists-p dir) :to-be nil)))

  (it "records what a fake emacsclient is given and prints its canned reply"
    (emacs-agent-test-with-socket-dir
      (emacs-agent-test-with-fake-clients
        (let ((client (getenv "EMACS_AGENT_EMACSCLIENT"))
              (process-environment (cons "FAKE_EMACSCLIENT_REPLY=\"[]\""
                                         process-environment)))
          (with-temp-buffer
            (expect (call-process client nil t nil "-s" "/x/server" "--eval"
                                  "(jf/agent-call \"call\" \"t\" \"{\\\"a\\\":\\\"b c\\\"}\")")
                    :to-equal 0)
            (expect (string-trim (buffer-string)) :to-equal "\"[]\""))
          (let ((argv (car (emacs-agent-test-fake-calls "client"))))
            (expect (emacs-agent-test-socket-arg argv) :to-equal "/x/server")
            (expect (emacs-agent-test-sent-form argv)
                    :to-equal '(jf/agent-call "call" "t" "{\"a\":\"b c\"}")))
          (expect (emacs-agent-test-fake-env "client") :to-equal '("<unset>"))
          (expect (emacs-agent-test-fake-calls "emacs") :to-be nil)))))

  (it "stands in a listening socket for the live session in the private directory"
    (emacs-agent-test-with-socket-dir
      (let ((path (emacs-agent-test-stand-in-socket)))
        (expect path :to-equal (emacs-agent-test-socket "server"))
        (expect (call-process "/bin/test" nil nil nil "-S" path) :to-equal 0))))

  (it "registers stub tools and preset entries only for the body"
    (assume (fboundp 'gptel-make-tool) "gptel is not loaded")
    (let ((before-tools gptel--known-tools)
          (before-presets gptel--known-presets))
      (emacs-agent-test-with-tools
          (list (list :name "emacs_agent_test_probe" :preset nil))
        (expect (gptel-tool-name (gptel-get-tool "emacs_agent_test_probe"))
                :to-equal "emacs_agent_test_probe"))
      (expect gptel--known-tools :to-be before-tools)
      (expect gptel--known-presets :to-be before-presets)
      (expect (assoc "emacs-agent-test" gptel--known-tools) :to-be nil)))))

(provide 'emacs-agent-test-helpers)
;;; helpers-spec.el ends here
