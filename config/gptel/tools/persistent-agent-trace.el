;;; persistent-agent-trace.el --- GPTEL persistent-agent hang trace -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; Diagnostic advice for the persistent-agent FSM hang.  Captures
;; FSM state transitions, stream-callback dispatch, scope authorize,
;; expansion UI queue state, and action-handler firings to a single
;; log file so the chain from "API response landed" to "FSM stuck"
;; can be read end-to-end.

;;; Code:

(defvar jf/gptel-pa-trace-file
  (expand-file-name "~/.gptel-pa-trace.log")
  "Path to the persistent-agent hang trace log.")

(defun jf/gptel-pa-trace--append (text)
  "Append TEXT to the persistent-agent trace log."
  (let ((coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert text)
      (append-to-file (point-min) (point-max) jf/gptel-pa-trace-file))))

(defun jf/gptel-pa-trace (tag fmt &rest args)
  "Write a single trace entry tagged TAG, formatted via FMT and ARGS."
  (condition-case err
      (jf/gptel-pa-trace--append
       (format "[%s] %-22s buf=%s :: %s\n"
               (format-time-string "%H:%M:%S.%3N")
               tag
               (buffer-name)
               (apply #'format fmt args)))
    (error
     (ignore-errors
       (jf/gptel-pa-trace--append
        (format "-- pa-trace LOG ERROR tag=%s err=%S --\n" tag err))))))

(defun jf/gptel-pa-trace--shape (response)
  "Return a short string describing RESPONSE's shape."
  (cond
   ((stringp response) (format "string len=%d" (length response)))
   ((null response) "nil(error)")
   ((eq response t) "t")
   ((eq response 'abort) "abort")
   ((and (consp response) (eq (car response) 'reasoning)) "(reasoning . _)")
   ((and (consp response) (eq (car response) 'tool-call))
    (format "(tool-call . %d calls: %s)"
            (length (cdr response))
            (mapconcat
             (lambda (c)
               (let ((spec (car-safe c)))
                 (cond
                  ((and spec (fboundp 'gptel-tool-name)
                        (ignore-errors (gptel-tool-name spec)))
                   (format "%s(%s)"
                           (gptel-tool-name spec)
                           (mapconcat
                            (lambda (kv) (format "%s" kv))
                            (let ((args (cadr c)) (acc nil))
                              (while args
                                (push (car args) acc)
                                (setq args (cddr args)))
                              (nreverse acc))
                            ",")))
                  (t "?"))))
             (cdr response) ", ")))
   ((and (consp response) (eq (car response) 'tool-result))
    (format "(tool-result . %d results)" (length (cdr response))))
   (t (format "OTHER %S" response))))

(defun jf/gptel-pa-trace--flag-snapshot (&optional buf)
  "Return a string like \"active=t queued=2\" reading from BUF (or current)."
  (let ((target (or buf (current-buffer))))
    (if (buffer-live-p target)
        (with-current-buffer target
          (format "active=%s queued=%d"
                  (if (and (boundp 'jf/gptel-scope--expansion-active)
                           jf/gptel-scope--expansion-active)
                      "t" "nil")
                  (if (boundp 'jf/gptel-scope--expansion-queue)
                      (length jf/gptel-scope--expansion-queue)
                    -1)))
      "buf=DEAD")))

(defun jf/gptel-pa-trace--around-task
    (orig main-cb preset description prompt &optional allowed-paths)
  "Around-advice for `jf/gptel-persistent-agent--task'."
  (jf/gptel-pa-trace
   'PA-task-entry
   "preset=%s desc=%S prompt-len=%d allowed=%S parent-id=%s"
   preset description (length prompt) allowed-paths
   (and (boundp 'jf/gptel--session-id) jf/gptel--session-id))
  (let ((wrapped-cb
         (lambda (text)
           (jf/gptel-pa-trace
            'PA-task-cb
            "main-cb fired text-len=%s preview=%S"
            (if (stringp text) (length text) "non-string")
            (if (stringp text)
                (substring text 0 (min 80 (length text)))
              text))
           (funcall main-cb text))))
    (prog1 (funcall orig wrapped-cb preset description prompt allowed-paths)
      (jf/gptel-pa-trace 'PA-task-exit "(returned from --task)"))))

(defun jf/gptel-pa-trace--fsm-transition (state fsm)
  "Log STATE entry for FSM."
  (let* ((info (and (fboundp 'gptel-fsm-info) (gptel-fsm-info fsm)))
         (req-buf (and info (plist-get info :buffer)))
         (tool-use (and info (plist-get info :tool-use))))
    (jf/gptel-pa-trace
     (intern (format "FSM-%s" state))
     "req-buf=%s tool-use=%s"
     (if (buffer-live-p req-buf) (buffer-name req-buf) "DEAD/nil")
     (if tool-use
         (format "%d-call(s)" (length tool-use))
       "nil"))))

(defun jf/gptel-pa-trace--before-on-wait (fsm) (jf/gptel-pa-trace--fsm-transition 'WAIT fsm))
(defun jf/gptel-pa-trace--before-on-type (fsm) (jf/gptel-pa-trace--fsm-transition 'TYPE fsm))
(defun jf/gptel-pa-trace--before-on-tool (fsm) (jf/gptel-pa-trace--fsm-transition 'TOOL fsm))
(defun jf/gptel-pa-trace--before-on-done (fsm) (jf/gptel-pa-trace--fsm-transition 'DONE fsm))
(defun jf/gptel-pa-trace--before-on-errs (fsm) (jf/gptel-pa-trace--fsm-transition 'ERRS fsm))
(defun jf/gptel-pa-trace--before-on-abrt (fsm) (jf/gptel-pa-trace--fsm-transition 'ABRT fsm))

(defun jf/gptel-pa-trace--around-stream-callback (orig &rest args)
  "Wrap the closure returned by `gptel-chat-stream-callback'."
  (let ((cb (apply orig args)))
    (lambda (response info)
      (let ((req-buf (and info (plist-get info :buffer))))
        (jf/gptel-pa-trace
         'stream-cb
         "shape=%s req-buf=%s tool-use=%s"
         (jf/gptel-pa-trace--shape response)
         (cond ((null req-buf) "nil")
               ((buffer-live-p req-buf) (buffer-name req-buf))
               (t "DEAD"))
         (if (plist-get info :tool-use) "yes" "no")))
      (condition-case err
          (funcall cb response info)
        (error
         (jf/gptel-pa-trace
          'stream-cb-ERR
          "shape=%s err=%S" (jf/gptel-pa-trace--shape response) err)
         (signal (car err) (cdr err)))))))

(defun jf/gptel-pa-trace--around-authorize
    (orig tool-name operation args on-allow on-deny)
  "Around-advice for `jf/gptel-scope-authorize-tool-call'."
  (jf/gptel-pa-trace
   'auth-entry
   "tool=%s op=%s args=%S %s"
   tool-name operation args (jf/gptel-pa-trace--flag-snapshot))
  (let ((traced-allow
         (lambda ()
           (jf/gptel-pa-trace 'auth-allow "tool=%s %s"
                              tool-name (jf/gptel-pa-trace--flag-snapshot))
           (condition-case err (funcall on-allow)
             (error (jf/gptel-pa-trace 'auth-allow-ERR "tool=%s err=%S"
                                       tool-name err)
                    (signal (car err) (cdr err))))))
        (traced-deny
         (lambda (resp)
           (jf/gptel-pa-trace 'auth-deny "tool=%s resp=%S %s"
                              tool-name resp (jf/gptel-pa-trace--flag-snapshot))
           (condition-case err (funcall on-deny resp)
             (error (jf/gptel-pa-trace 'auth-deny-ERR "tool=%s err=%S"
                                       tool-name err)
                    (signal (car err) (cdr err)))))))
    (funcall orig tool-name operation args traced-allow traced-deny)))

(defun jf/gptel-pa-trace--around-trigger-expansion
    (orig validation-error tool-name wrapper-callback)
  "Around-advice for `jf/gptel-scope--trigger-inline-expansion'."
  (jf/gptel-pa-trace
   'auth-trigger-expansion
   "tool=%s err=%S resource=%S op=%s vt=%s"
   tool-name
   (plist-get validation-error :error)
   (or (plist-get validation-error :resource)
       (plist-get validation-error :path))
   (plist-get validation-error :operation)
   (plist-get validation-error :validation-type))
  (let ((traced-cb
         (lambda (wrapper-result)
           (jf/gptel-pa-trace
            'auth-expansion-resolved
            "tool=%s approved=%s allowed-once=%s"
            tool-name
            (plist-get wrapper-result :approved)
            (plist-get wrapper-result :allowed-once))
           (condition-case err (funcall wrapper-callback wrapper-result)
             (error
              (jf/gptel-pa-trace 'auth-expansion-resolved-ERR
                                 "tool=%s err=%S" tool-name err)
              (signal (car err) (cdr err)))))))
    (funcall orig validation-error tool-name traced-cb)))

(defun jf/gptel-pa-trace--around-prompt-expansion
    (orig violation-info callback patterns tool-name)
  "Around-advice for `jf/gptel-scope-prompt-expansion'."
  (let* ((before-snap (jf/gptel-pa-trace--flag-snapshot))
         (will-queue (and (boundp 'jf/gptel-scope--expansion-active)
                          jf/gptel-scope--expansion-active)))
    (jf/gptel-pa-trace
     'expansion-prompt
     "tool=%s decision=%s %s patterns=%S resource=%S"
     tool-name
     (if will-queue "QUEUE" "SHOW-NOW")
     before-snap patterns
     (plist-get violation-info :resource))
    (let ((traced-cb
           (lambda (json-result)
             (jf/gptel-pa-trace
              'expansion-cb
              "tool=%s json=%s" tool-name
              (if (stringp json-result)
                  (substring json-result 0 (min 200 (length json-result)))
                json-result))
             (condition-case err (funcall callback json-result)
               (error
                (jf/gptel-pa-trace 'expansion-cb-ERR
                                   "tool=%s err=%S" tool-name err)
                (signal (car err) (cdr err)))))))
      (funcall orig violation-info traced-cb patterns tool-name))))

(defun jf/gptel-pa-trace--around-process-queue (orig &rest args)
  "Around-advice for `jf/gptel-scope--process-expansion-queue'."
  (let ((before (jf/gptel-pa-trace--flag-snapshot)))
    (prog1 (apply orig args)
      (jf/gptel-pa-trace
       'queue-pump
       "before=(%s) after=(%s)"
       before (jf/gptel-pa-trace--flag-snapshot)))))

(defun jf/gptel-pa-trace--before-allow-once (&rest _)
  (jf/gptel-pa-trace
   'expansion-action
   "kind=allow-once %s" (jf/gptel-pa-trace--flag-snapshot)))

(defun jf/gptel-pa-trace--before-add-to-scope (&rest _)
  (jf/gptel-pa-trace
   'expansion-action
   "kind=add-to-scope %s" (jf/gptel-pa-trace--flag-snapshot)))

(defun jf/gptel-pa-trace--before-add-wildcard (&rest _)
  (jf/gptel-pa-trace
   'expansion-action
   "kind=add-wildcard %s" (jf/gptel-pa-trace--flag-snapshot)))

(defun jf/gptel-pa-trace--before-add-custom (&rest _)
  (jf/gptel-pa-trace
   'expansion-action
   "kind=add-custom %s" (jf/gptel-pa-trace--flag-snapshot)))

(defun jf/gptel-pa-trace--before-deny (&rest _)
  (jf/gptel-pa-trace
   'expansion-action
   "kind=deny %s" (jf/gptel-pa-trace--flag-snapshot)))

(defun jf/gptel-pa-trace-snapshot ()
  "Walk all live buffers, log the gptel session-state of each."
  (interactive)
  (jf/gptel-pa-trace
   'forensic-snapshot
   "=== begin (frame=%s) ===" (selected-frame))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (or (and (boundp 'jf/gptel--session-id) jf/gptel--session-id)
                (string-match-p "session\\.org" (or (buffer-name) "")))
        (jf/gptel-pa-trace
         'forensic-snapshot
         "buf=%s mode=%s session-id=%s active=%s queued=%d size=%d fsm-state=%s"
         (buffer-name) major-mode
         (and (boundp 'jf/gptel--session-id) jf/gptel--session-id)
         (if (and (boundp 'jf/gptel-scope--expansion-active)
                  jf/gptel-scope--expansion-active) "t" "nil")
         (if (boundp 'jf/gptel-scope--expansion-queue)
             (length jf/gptel-scope--expansion-queue) -1)
         (buffer-size)
         (and (boundp 'gptel--fsm-last)
              gptel--fsm-last
              (fboundp 'gptel-fsm-state)
              (gptel-fsm-state gptel--fsm-last))))))
  (jf/gptel-pa-trace 'forensic-snapshot "=== end ===")
  (message "PA trace snapshot written to %s" jf/gptel-pa-trace-file))

(defun jf/gptel-pa-trace-register ()
  "Install persistent-agent trace advice. Idempotent."
  (jf/gptel-pa-trace-unregister)

  (advice-add 'jf/gptel-persistent-agent--task :around
              #'jf/gptel-pa-trace--around-task
              '((name . jf-pa-trace-task)))

  (dolist (pair '((gptel-chat--on-wait . jf/gptel-pa-trace--before-on-wait)
                  (gptel-chat--on-type . jf/gptel-pa-trace--before-on-type)
                  (gptel-chat--on-tool . jf/gptel-pa-trace--before-on-tool)
                  (gptel-chat--on-done . jf/gptel-pa-trace--before-on-done)
                  (gptel-chat--on-errs . jf/gptel-pa-trace--before-on-errs)
                  (gptel-chat--on-abrt . jf/gptel-pa-trace--before-on-abrt)))
    (advice-add (car pair) :before (cdr pair)
                `((name . ,(intern (format "jf-pa-trace-%s" (car pair)))))))

  (advice-add 'gptel-chat-stream-callback :around
              #'jf/gptel-pa-trace--around-stream-callback
              '((name . jf-pa-trace-stream-callback)))

  (advice-add 'jf/gptel-scope-authorize-tool-call :around
              #'jf/gptel-pa-trace--around-authorize
              '((name . jf-pa-trace-authorize)))

  (advice-add 'jf/gptel-scope--trigger-inline-expansion :around
              #'jf/gptel-pa-trace--around-trigger-expansion
              '((name . jf-pa-trace-trigger-expansion)))

  (advice-add 'jf/gptel-scope-prompt-expansion :around
              #'jf/gptel-pa-trace--around-prompt-expansion
              '((name . jf-pa-trace-prompt-expansion)))

  (advice-add 'jf/gptel-scope--process-expansion-queue :around
              #'jf/gptel-pa-trace--around-process-queue
              '((name . jf-pa-trace-process-queue)))

  (dolist (pair '((jf/gptel-scope--allow-once-action     . jf/gptel-pa-trace--before-allow-once)
                  (jf/gptel-scope--add-to-scope          . jf/gptel-pa-trace--before-add-to-scope)
                  (jf/gptel-scope--add-wildcard-to-scope . jf/gptel-pa-trace--before-add-wildcard)
                  (jf/gptel-scope--add-custom-to-scope   . jf/gptel-pa-trace--before-add-custom)
                  (jf/gptel-scope--deny-expansion        . jf/gptel-pa-trace--before-deny)))
    (advice-add (car pair) :before (cdr pair)
                `((name . ,(intern (format "jf-pa-trace-action-%s" (car pair))))))))

(defun jf/gptel-pa-trace-unregister ()
  "Remove persistent-agent trace advice."
  (advice-remove 'jf/gptel-persistent-agent--task 'jf-pa-trace-task)

  (dolist (sym '(gptel-chat--on-wait gptel-chat--on-type gptel-chat--on-tool
                 gptel-chat--on-done gptel-chat--on-errs gptel-chat--on-abrt))
    (advice-remove sym (intern (format "jf-pa-trace-%s" sym))))

  (advice-remove 'gptel-chat-stream-callback 'jf-pa-trace-stream-callback)
  (advice-remove 'jf/gptel-scope-authorize-tool-call 'jf-pa-trace-authorize)
  (advice-remove 'jf/gptel-scope--trigger-inline-expansion 'jf-pa-trace-trigger-expansion)
  (advice-remove 'jf/gptel-scope-prompt-expansion 'jf-pa-trace-prompt-expansion)
  (advice-remove 'jf/gptel-scope--process-expansion-queue 'jf-pa-trace-process-queue)

  (dolist (sym '(jf/gptel-scope--allow-once-action
                 jf/gptel-scope--add-to-scope
                 jf/gptel-scope--add-wildcard-to-scope
                 jf/gptel-scope--add-custom-to-scope
                 jf/gptel-scope--deny-expansion))
    (advice-remove sym (intern (format "jf-pa-trace-action-%s" sym)))))

(defun jf/gptel-pa-trace-start ()
  "Reset the trace log and install advice. Run before reproducing the hang."
  (interactive)
  (when (file-exists-p jf/gptel-pa-trace-file)
    (delete-file jf/gptel-pa-trace-file))
  (jf/gptel-pa-trace--append
   (format "=== PA TRACE RESET %s (pid=%d) ===\n"
           (format-time-string "%Y-%m-%d %H:%M:%S")
           (emacs-pid)))
  (jf/gptel-pa-trace-register)
  (message "Persistent-agent trace active — log: %s"
           jf/gptel-pa-trace-file))

(defun jf/gptel-pa-trace-stop ()
  "Remove advice. Log file is preserved."
  (interactive)
  (jf/gptel-pa-trace-unregister)
  (message "Persistent-agent trace stopped — log preserved at %s"
           jf/gptel-pa-trace-file))

(defun jf/gptel-pa-trace-open-log ()
  "Open the trace log buffer at its tail."
  (interactive)
  (find-file jf/gptel-pa-trace-file)
  (goto-char (point-max)))

(provide 'gptel-persistent-agent-trace)
;;; persistent-agent-trace.el ends here
