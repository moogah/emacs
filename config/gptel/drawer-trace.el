;;; drawer-trace.el --- GPTEL session drawer trace -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; Captures buffer snapshots around the gptel session save lifecycle,
;; isolating the source of property-drawer corruption.

;;; Code:

(require 'org)

(defvar jf/gptel-drawer-trace-file
  (expand-file-name "~/.gptel-drawer-trace.log")
  "Path to the drawer corruption trace log.")

(defvar jf/gptel-drawer-trace-context 400
  "Number of chars captured from buffer head and tail per snapshot.")

(defvar jf/gptel-drawer-trace-top-window 10000000
  "Position threshold (chars from point-min). Only edits whose BEG lands
below this threshold are logged by the after-change hook.

Set to a very large value by default so that edits anywhere in the
buffer are captured. The original motivation for a narrow top window
was noise reduction; with the match-data-clobbering hypothesis we need
full coverage to see inserts that land at point-max.")

(defun jf/gptel-drawer--session-p ()
  "Non-nil when the current buffer is a gptel session .org file."
  (and buffer-file-name
       (string-match-p
        "/\\(?:branches\\|agents\\)/[^/]+/session\\.org\\'"
        buffer-file-name)))

(defun jf/gptel-drawer--append (text)
  "Append TEXT to the drawer trace log."
  (let ((coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert text)
      (append-to-file (point-min) (point-max) jf/gptel-drawer-trace-file))))

(defun jf/gptel-drawer--backtrace-string ()
  "Return the current backtrace as a string."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (backtrace))
    (buffer-string)))

(defun jf/gptel-drawer--snapshot (phase &optional extra)
  "Append a HEAD+TAIL snapshot of the current buffer for PHASE.
Only fires inside gptel session buffers. EXTRA is appended to the
header line if given."
  (when (jf/gptel-drawer--session-p)
    (condition-case err
        (let* ((ctx (max 50 jf/gptel-drawer-trace-context))
               (head (buffer-substring-no-properties
                      (point-min)
                      (min (point-max) (+ (point-min) ctx))))
               (tail (buffer-substring-no-properties
                      (max (point-min) (- (point-max) ctx))
                      (point-max)))
               (drawer-valid
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (if (looking-at-p org-property-drawer-re) "t" "NIL")))))
          (jf/gptel-drawer--append
           (format "\n=== %s | %s | buf=%s | size=%d | drawer-valid=%s%s ===\nHEAD:\n%s\nTAIL:\n%s\n"
                   (format-time-string "%H:%M:%S.%3N")
                   phase
                   (buffer-name)
                   (buffer-size)
                   drawer-valid
                   (if extra (format " | %s" extra) "")
                   head tail)))
      (error
       (ignore-errors
         (jf/gptel-drawer--append
          (format "\n-- snapshot LOG ERROR in phase %s: %S --\n" phase err)))))))

(defun jf/gptel-drawer--after-change (beg end len)
  "Log top-of-buffer edits in gptel session buffers with a backtrace.
BEG, END, LEN are the standard `after-change-functions' arguments.

Logs two categories:
1. Edits inside a session buffer within the top window — full backtrace.
2. Edits with inhibit-modification-hooks semantics or weird contexts —
   log minimal info to account for the \"missing events\" case where
   `buffer-file-name` is transiently nil."
  (when (< beg jf/gptel-drawer-trace-top-window)
    (condition-case err
        (let* ((is-session (jf/gptel-drawer--session-p))
               (bname (buffer-name))
               (bfile buffer-file-name)
               (is-candidate
                (or is-session
                    ;; Also log if buffer name looks gptel-related
                    (and bname (string-match-p "gptel" bname))))
               (inserted (if (> end beg)
                             (buffer-substring-no-properties
                              beg (min end (point-max)))
                           "")))
          (when is-candidate
            (let ((bt (jf/gptel-drawer--backtrace-string)))
              (jf/gptel-drawer--append
               (format "\n-- AFTER-CHANGE %s | buf=%s | bfile=%s | session-p=%s | beg=%d end=%d len-before=%d ins=%d | imh=%s --\ninserted: %S\nbacktrace:\n%s\n"
                       (format-time-string "%H:%M:%S.%3N")
                       bname
                       (if bfile (file-name-nondirectory bfile) "nil")
                       (if is-session "t" "NIL")
                       beg end len (- end beg)
                       (if inhibit-modification-hooks "t" "nil")
                       inserted
                       bt)))))
      (error
       (ignore-errors
         (jf/gptel-drawer--append
          (format "\n-- after-change LOG ERROR: %S --\n" err)))))))

(defun jf/gptel-drawer--before-save-advice (&rest _)
  "Snapshot drawer region before `gptel--save-state' runs."
  (jf/gptel-drawer--snapshot "BEFORE-save-state"))

(defun jf/gptel-drawer--after-save-advice (&rest _)
  "Snapshot drawer region after `gptel--save-state' runs."
  (jf/gptel-drawer--snapshot "AFTER-save-state"))

(defun jf/gptel-drawer--post-response-hook (beg end)
  "Post-response snapshot. BEG and END are response bounds."
  (jf/gptel-drawer--snapshot
   "POST-RESPONSE"
   (format "resp-beg=%s resp-end=%s" beg end)))

(defun jf/gptel-drawer--entry-put-trace (orig-fn epom property value)
  "Around-advice wrapper for `org-entry-put' capturing match-data/point.
Logs only for gptel session buffers to keep the log focused."
  (if (not (jf/gptel-drawer--session-p))
      (funcall orig-fn epom property value)
    (let* ((md-before (match-data))
           (pt-before (point))
           (size-before (buffer-size))
           (ts (format-time-string "%H:%M:%S.%3N"))
           (err-result nil))
      (condition-case err
          (prog1 (funcall orig-fn epom property value)
            (jf/gptel-drawer--append
             (format (concat "\n-- ENTRY-PUT %s | prop=%s value=%S | epom=%S"
                             " | pt-before=%d pt-after=%d"
                             " | size-before=%d size-after=%d"
                             " | md-before=%S"
                             " | md-after=%S --\n")
                     ts property
                     (if (stringp value)
                         (substring value 0 (min 60 (length value)))
                       value)
                     epom pt-before (point)
                     size-before (buffer-size)
                     md-before (match-data))))
        (error
         (setq err-result err)
         (jf/gptel-drawer--append
          (format (concat "\n-- ENTRY-PUT-ERROR %s | prop=%s | epom=%S"
                          " | pt-before=%d pt-after=%d"
                          " | md-before=%S | md-after=%S"
                          " | err=%S --\n")
                  ts property epom pt-before (point)
                  md-before (match-data) err))
         (signal (car err) (cdr err)))))))

(defun jf/gptel-drawer-trace-register ()
  "Install drawer trace advice and hooks. Idempotent."
  (jf/gptel-drawer-trace-unregister)
  (advice-add 'gptel--save-state :before
              #'jf/gptel-drawer--before-save-advice
              '((name . jf-drawer-trace-before-save)))
  (advice-add 'gptel--save-state :after
              #'jf/gptel-drawer--after-save-advice
              '((name . jf-drawer-trace-after-save)))
  (advice-add 'org-entry-put :around
              #'jf/gptel-drawer--entry-put-trace
              '((name . jf-drawer-trace-entry-put)))
  (add-hook 'gptel-post-response-functions
            #'jf/gptel-drawer--post-response-hook)
  (add-hook 'after-change-functions
            #'jf/gptel-drawer--after-change))

(defun jf/gptel-drawer-trace-unregister ()
  "Remove drawer trace advice and hooks."
  (advice-remove 'gptel--save-state 'jf-drawer-trace-before-save)
  (advice-remove 'gptel--save-state 'jf-drawer-trace-after-save)
  (advice-remove 'org-entry-put 'jf-drawer-trace-entry-put)
  (remove-hook 'gptel-post-response-functions
               #'jf/gptel-drawer--post-response-hook)
  (remove-hook 'after-change-functions
               #'jf/gptel-drawer--after-change))

(defun jf/gptel-drawer-trace-start ()
  "Reset the drawer trace log and activate all hooks.
Run before starting a session you want to diagnose."
  (interactive)
  (when (file-exists-p jf/gptel-drawer-trace-file)
    (delete-file jf/gptel-drawer-trace-file))
  (jf/gptel-drawer--append
   (format "=== DRAWER TRACE RESET %s (pid=%d) ===\n"
           (format-time-string "%Y-%m-%d %H:%M:%S")
           (emacs-pid)))
  (jf/gptel-drawer-trace-register)
  (message "Drawer trace active — log: %s" jf/gptel-drawer-trace-file))

(defun jf/gptel-drawer-trace-stop ()
  "Deactivate drawer trace hooks. Log file is preserved."
  (interactive)
  (jf/gptel-drawer-trace-unregister)
  (message "Drawer trace stopped — log preserved at %s"
           jf/gptel-drawer-trace-file))

(defun jf/gptel-drawer-trace-open-log ()
  "Open the drawer trace log in a buffer at its tail."
  (interactive)
  (find-file jf/gptel-drawer-trace-file)
  (goto-char (point-max)))

(jf/gptel-drawer-trace-register)
(jf/gptel-drawer--append
 (format "\n=== DRAWER TRACE HOOKS LOADED %s (pid=%d) ===\n"
         (format-time-string "%Y-%m-%d %H:%M:%S")
         (emacs-pid)))
(message "GPTEL drawer trace armed — log: %s" jf/gptel-drawer-trace-file)

(provide 'jf-gptel-drawer-trace)
;;; drawer-trace.el ends here
