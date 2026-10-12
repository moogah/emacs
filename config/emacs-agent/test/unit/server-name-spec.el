;;; server-name-spec.el --- Only the live session serves the default socket -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md § Open Questions
;; ("A non-daemon instance can take the live socket", decided: only the live
;; session serves `server'; an isolated GUI instance serves its own name).
;;
;; `jf/server-name-for-instance' (config/major-modes/org-roam.org) is driven
;; with the launch facts stubbed: `daemonp', `noninteractive', the
;; EMACS_USER_DIRECTORY bin/emacs-isolated.sh exports, `jf/emacs-dir' and
;; `server-running-p'.  No server is started.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))
(require 'server)

(unless (fboundp 'jf/server-name-for-instance)
  (load (expand-file-name "config/major-modes/org-roam.el" jf/emacs-dir) nil t))

(defun server-name-spec--name (&rest facts)
  "`jf/server-name-for-instance' under FACTS, a plist.
:daemon is what `daemonp' answers, :batch `noninteractive', :isolated-dir
the EMACS_USER_DIRECTORY value (nil: unset), :checkout `jf/emacs-dir' and
:running the names `server-running-p' answers t for."
  (let ((process-environment (copy-sequence process-environment))
        (noninteractive (plist-get facts :batch))
        (jf/emacs-dir (or (plist-get facts :checkout) "/w/emacs/"))
        (running (plist-get facts :running)))
    (setenv "EMACS_USER_DIRECTORY" (plist-get facts :isolated-dir))
    ;; `daemonp' is a primitive: redefining it must not compile a native
    ;; trampoline.
    (let ((native-comp-enable-subr-trampolines nil))
      (cl-letf (((symbol-function 'daemonp)
                 (lambda () (plist-get facts :daemon)))
                ((symbol-function 'server-running-p)
                 (lambda (&optional name) (and (member name running) t))))
        (jf/server-name-for-instance)))))


(describe "jf/server-name-for-instance (seam/target-lifecycle)"

  (it "serves the default socket from the live session, an interactive Emacs not launched by emacs-isolated.sh"
    (expect (server-name-spec--name) :to-equal "server"))

  (it "serves nothing from a batch Emacs, even one launched with EMACS_USER_DIRECTORY as the test runner is"
    (expect (server-name-spec--name :batch t) :to-be nil)
    (expect (server-name-spec--name :batch t :isolated-dir "/w/emacs/runtime") :to-be nil))

  (it "serves nothing from init in a daemon, which serves its daemon name itself"
    (expect (server-name-spec--name :daemon "smoke-1") :to-be nil)
    (expect (server-name-spec--name :daemon t :isolated-dir "/w/emacs/runtime") :to-be nil))

  (it "serves isolated-CHECKOUT from an emacs-isolated.sh GUI launch, never the default socket"
    (expect (server-name-spec--name :isolated-dir "/w/emacs/runtime")
            :to-equal "isolated-emacs")
    (expect (server-name-spec--name :isolated-dir "/w/emacs/runtime" :running '("server"))
            :to-equal "isolated-emacs"))

  (it "makes the checkout's name a valid target name and adds -PID when the name is served"
    (expect (server-name-spec--name :isolated-dir "/x/runtime" :checkout "/x/my wt+1/")
            :to-equal "isolated-my-wt-1")
    (expect (server-name-spec--name :isolated-dir "/w/emacs/runtime"
                                    :running '("isolated-emacs"))
            :to-equal (format "isolated-emacs-%d" (emacs-pid))))

  (it "names an isolated instance the wrapper accepts as a target name"
    (emacs-agent-test-with-socket-dir
      (emacs-agent-test-with-fake-clients
        (let ((result (emacs-agent-test-wrapper-run
                       "target"
                       (server-name-spec--name :isolated-dir "/x/runtime"
                                               :checkout "/x/.my wt/")
                       "tools")))
          (expect (plist-get result :exit) :to-equal 2)
          (expect (plist-get result :stderr) :not :to-match "not a target name"))))))

;;; server-name-spec.el ends here
