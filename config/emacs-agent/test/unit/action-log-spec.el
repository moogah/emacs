;;; action-log-spec.el --- The live session's action log -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D12 and § Seams:
;; seam/action-log.
;;
;; The live session is never driven by a test.  The log keys on the
;; serving Emacs being the live session (`server-name' is "server"), so a
;; spec binds `server-name' and dispatches in the test process: that is
;; the contract standing in for agent-target-policy § Live call is logged.
;; Pending until config/emacs-agent/action-log.org exists.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defvar server-name)

(defun action-log-spec--text ()
  "The action log's text, or nil when there is no log buffer."
  (let ((buffer (get-buffer "*emacs-agent-log*")))
    (and buffer (with-current-buffer buffer (buffer-string)))))

(defun action-log-spec--fresh ()
  "Start each spec without a log buffer."
  (let ((buffer (get-buffer "*emacs-agent-log*")))
    (when buffer
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))


(describe "The action log (seam/action-log)"
  (before-each (action-log-spec--fresh))
  (after-each (action-log-spec--fresh))

  (it "logs a live call-write with its tier, tool and outcome (Scenario: agent-target-policy § Live call is logged)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_writer" :preset 'write
                    :args (list (emacs-agent-test-arg "title"))
                    :function (lambda (title) (format "wrote %s" title))))
      (let ((server-name "server"))
        (jf/agent-call "call-write" "emacs_agent_test_writer" "{\"title\":\"Note\"}")))
    (let ((log (action-log-spec--text)))
      (expect log :to-match "call-write")
      (expect log :to-match "emacs_agent_test_writer")
      (expect log :to-match "Note")
      (expect log :to-match "\\_<ok\\_>")))

  (it "logs a refused or failing live call with an error outcome (seam/action-log)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_writer" :preset 'write))
      (let ((server-name "server"))
        (jf/agent-call "call" "emacs_agent_test_writer" "{}")))
    (let ((log (action-log-spec--text)))
      (expect log :to-match "emacs_agent_test_writer")
      (expect log :to-match "\\_<error\\_>")))

  (it "logs a live eval with its form (seam/action-log)"
    (let ((server-name "server"))
      (jf/agent-eval "(+ 40 2)"))
    (let ((log (action-log-spec--text)))
      (expect log :to-match "eval")
      (expect log :to-match (regexp-quote "(+ 40 2)"))))

  (it "logs nothing in an Emacs that is not the live session (seam/action-log)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_reader" :preset 'read))
      (let ((server-name "smoke-target"))
        (jf/agent-call "call" "emacs_agent_test_reader" "{}")
        (jf/agent-eval "(+ 1 1)")))
    (let ((log (action-log-spec--text)))
      (expect (or (null log) (not (string-match-p "emacs_agent_test_reader" log)))
              :to-be t)))

  (it "keeps each entry's argument summary within jf/agent-log-args-max (seam/action-log)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_writer" :preset 'write
                    :args (list (emacs-agent-test-arg "body"))))
      (let ((server-name "server"))
        (jf/agent-call "call-write" "emacs_agent_test_writer"
                       (format "{\"body\":%S}" (make-string 5000 ?x)))))
    (let ((longest (apply #'max (mapcar #'length
                                        (split-string (action-log-spec--text) "\n")))))
      (expect (< longest (+ jf/agent-log-args-max 200)) :to-be t))))

;;; action-log-spec.el ends here
