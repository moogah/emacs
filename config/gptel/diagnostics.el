(defvar jf/gptel--bounds-history nil
  "History of gptel--bounds changes for debugging.")

(defvar jf/gptel--parse-trace nil
  "Trace of tool parsing attempts.")

(defvar jf/gptel--diagnostic-file nil
  "Current diagnostic log file path. Auto-set to session dir or fallback.")

(defvar jf/gptel--diagnostic-fallback-file
  (expand-file-name "~/.gptel-diagnostics.log")
  "Fallback diagnostic file when not in a session.")

(defun jf/gptel--get-diagnostic-file ()
  "Get the current diagnostic file path.
Uses session directory if in a gptel session, otherwise fallback."
  (or jf/gptel--diagnostic-file
      (setq jf/gptel--diagnostic-file
            (if (and (boundp 'jf/gptel--current-session-id)
                     jf/gptel--current-session-id)
                ;; We're in a session - write to session directory
                (expand-file-name "diagnostics.log"
                                 (expand-file-name
                                  jf/gptel--current-session-id
                                  jf/gptel-session-root-dir))
              ;; Not in a session - use fallback
              jf/gptel--diagnostic-fallback-file))))

(defun jf/gptel--write-diagnostic-entry (entry)
  "Write diagnostic ENTRY to the auto-log file immediately."
  (let ((log-file (jf/gptel--get-diagnostic-file))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "\n=== %s | %s ===\n"
                     timestamp
                     (plist-get entry :phase)))
      (insert (format "Buffer: %s\n" (plist-get entry :buffer)))
      (insert (format "Bounds: %S\n" (plist-get entry :bounds)))

      ;; If tool parses were captured, show them
      (when-let ((tool-parses (plist-get entry :tool-parses)))
        (insert "\nTool Parse Results:\n")
        (dolist (parse tool-parses)
          (if (plist-get parse :success)
              (insert (format "  ✓ Position %d: %s\n"
                             (plist-get parse :position)
                             (plist-get parse :name)))
            (insert (format "  ✗ Position %d: ERROR - %s\n"
                           (plist-get parse :position)
                           (plist-get parse :error))))))

      (insert "\n")

      ;; Append to file
      (append-to-file (point-min) (point-max) log-file))))

(defun jf/gptel--write-parse-entry (entry)
  "Write parse trace ENTRY to the auto-log file immediately."
  (let ((log-file (jf/gptel--get-diagnostic-file))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N")))
    (with-temp-buffer
      (insert (format "\n=== %s | PARSE-BUFFER ===\n" timestamp))
      (insert (format "Buffer: %s\n" (plist-get entry :buffer)))
      (insert (format "API results: %d\n" (plist-get entry :num-results)))

      ;; Show tool scans
      (when-let ((tool-scans (plist-get entry :tool-scans)))
        (insert (format "Tool positions found: %d\n\n" (length tool-scans)))
        (dolist (scan tool-scans)
          (if (plist-get scan :success)
              (insert (format "  ✓ Position %d (line %d): %s\n"
                             (plist-get scan :position)
                             (plist-get scan :line)
                             (plist-get scan :name)))
            (insert (format "  ✗ Position %d (line %d): ERROR - %s\n"
                           (plist-get scan :position)
                           (plist-get scan :line)
                           (plist-get scan :error))))))

      (insert "\n")

      ;; Append to file
      (append-to-file (point-min) (point-max) log-file))))

(defun jf/gptel--record-bounds (phase &optional extra-info)
  "Record current gptel--bounds value with PHASE label.
EXTRA-INFO can provide additional context about the recording.
Also captures what's actually at tool positions and what would be parsed."
  (when (boundp 'gptel--bounds)
    (let* ((timestamp (format-time-string "%H:%M:%S.%3N"))
           (bounds-value (if (local-variable-p 'gptel--bounds)
                            gptel--bounds
                          'not-buffer-local))
           (tool-parses nil)
           (entry (list :timestamp timestamp
                       :phase phase
                       :bounds bounds-value
                       :buffer (buffer-name)
                       :extra extra-info)))

      ;; For each tool bound, capture what would be parsed
      (when (and (listp bounds-value)
                 (not (eq bounds-value 'not-buffer-local)))
        (when-let ((tool-bounds (alist-get 'tool bounds-value)))
          (dolist (bound tool-bounds)
            (let* ((beg (nth 0 bound))
                   (end (nth 1 bound))
                   (id (nth 2 bound))
                   (parse-result nil))
              (save-excursion
                (goto-char beg)
                (setq parse-result
                      (condition-case err
                          (let* ((tool-call (read (current-buffer)))
                                 (name (plist-get tool-call :name))
                                 (args (plist-get tool-call :args)))
                            (list :success t
                                  :id id
                                  :position beg
                                  :name name
                                  :args args
                                  :full-sexp tool-call))
                        (error
                         (list :success nil
                               :id id
                               :position beg
                               :error (error-message-string err)
                               :buffer-content (buffer-substring-no-properties
                                               beg (min end (+ beg 100))))))))
              (push parse-result tool-parses)))))

      (when tool-parses
        (plist-put entry :tool-parses (nreverse tool-parses)))

      (push entry jf/gptel--bounds-history)

      ;; Auto-write to diagnostic file
      (jf/gptel--write-diagnostic-entry entry)

      (message "[BOUNDS] %s: %s (tools: %d)" phase bounds-value (length tool-parses)))))

(defun jf/gptel--inspect-bounds-positions ()
  "Show buffer content at positions specified in gptel--bounds."
  (interactive)
  (unless (and (boundp 'gptel--bounds) gptel--bounds)
    (user-error "No gptel--bounds in current buffer"))

  (with-output-to-temp-buffer "*GPTEL Bounds Inspection*"
    (princ (format "Buffer: %s\n" (buffer-name)))
    (princ (format "gptel--bounds: %S\n\n" gptel--bounds))

    (dolist (prop-entry gptel--bounds)
      (let ((prop-type (car prop-entry))
            (bounds-list (cdr prop-entry)))
        (princ (format "=== Property: %s ===\n" prop-type))

        (dolist (bound bounds-list)
          (let* ((beg (nth 0 bound))
                 (end (nth 1 bound))
                 (val (nth 2 bound))
                 (text-prop (get-text-property beg 'gptel)))
            (princ (format "\nBound: %S\n" bound))
            (princ (format "  Position %d-%d" beg end))
            (when val (princ (format " (value: %s)" val)))
            (princ "\n")
            (princ (format "  Text property at %d: %S\n" beg text-prop))
            (princ (format "  Line at position %d: %d\n"
                          beg (line-number-at-pos beg)))
            (princ (format "  Content preview (50 chars from %d):\n" beg))
            (princ (format "    %S\n"
                          (buffer-substring-no-properties
                           beg (min end (+ beg 50)))))

            ;; For tool bounds, show if we can read a valid sexp
            (when (eq prop-type 'tool)
              (princ "  Attempting to read sexp from position:\n")
              (save-excursion
                (goto-char beg)
                (condition-case err
                    (let ((sexp (read (current-buffer))))
                      (princ (format "    SUCCESS: %S\n" sexp)))
                  (error
                   (princ (format "    ERROR: %s\n" err))))))))
        (princ "\n")))))

(defun jf/gptel--scan-tool-positions ()
  "Scan buffer for tool positions and attempt to parse them.
Returns list of parse attempts with results."
  (let ((tool-parses nil)
        (pt (point-max)))
    ;; Scan backwards like gptel--parse-buffer does
    (save-excursion
      (while (> pt (point-min))
        (let ((prop-val (get-char-property pt 'gptel)))
          (when (and (consp prop-val)
                     (eq (car prop-val) 'tool))
            (let* ((tool-id (cdr prop-val))
                   (line (line-number-at-pos pt))
                   (trace-entry (list :type "tool-scan"
                                     :position pt
                                     :line line
                                     :property prop-val
                                     :id tool-id)))
              (goto-char pt)
              (condition-case err
                  (let* ((tool-call (read (current-buffer)))
                         (name (plist-get tool-call :name))
                         (args (plist-get tool-call :args)))
                    (plist-put trace-entry :success t)
                    (plist-put trace-entry :sexp tool-call)
                    (plist-put trace-entry :name name)
                    (plist-put trace-entry :args args))
                (error
                 (plist-put trace-entry :success nil)
                 (plist-put trace-entry :error (error-message-string err))
                 (plist-put trace-entry :buffer-content
                           (buffer-substring-no-properties
                            pt (min (point-max) (+ pt 100))))))
              (push trace-entry tool-parses)))
          (setq pt (previous-single-char-property-change pt 'gptel nil (point-min))))))
    (nreverse tool-parses)))

(defun jf/gptel--trace-parse-buffer-advice (orig-fn backend &optional max-entries)
  "Trace gptel--parse-buffer execution for tool call parsing.
ORIG-FN is the original function, BACKEND is the gptel backend,
MAX-ENTRIES limits the number of entries to parse."
  ;; Capture tool positions BEFORE parsing
  (let* ((pre-parse-scan (jf/gptel--scan-tool-positions))
         (result (funcall orig-fn backend max-entries))
         (parse-entry (list :timestamp (format-time-string "%H:%M:%S.%3N")
                           :phase "parse-buffer-call"
                           :buffer (buffer-name)
                           :num-results (length result)
                           :tool-scans pre-parse-scan)))
    (push parse-entry jf/gptel--parse-trace)

    ;; Auto-write to diagnostic file
    (jf/gptel--write-parse-entry parse-entry)

    ;; Display trace window
    (with-temp-buffer-window "*GPTEL Parse Trace*" nil nil
      (princ (format "=== Parse Buffer Trace ===\n"))
      (princ (format "Buffer: %s\n" (buffer-name)))
      (princ (format "API entries generated: %d\n" (length result)))
      (princ (format "Tool positions found: %d\n\n" (length pre-parse-scan)))

      (dolist (scan pre-parse-scan)
        (princ (format "--- Tool at position %d (line %d) ---\n"
                      (plist-get scan :position)
                      (plist-get scan :line)))
        (princ (format "  ID: %s\n" (plist-get scan :id)))
        (if (plist-get scan :success)
            (progn
              (princ (format "  Name: %s\n" (plist-get scan :name)))
              (princ (format "  Args: %S\n" (plist-get scan :args))))
          (progn
            (princ (format "  ERROR: %s\n" (plist-get scan :error)))
            (when-let ((content (plist-get scan :buffer-content)))
              (princ (format "  Buffer content: %S\n" content)))))
        (princ "\n")))
    result))

(defun jf/gptel--show-all-text-properties ()
  "Display all gptel text properties in current buffer."
  (interactive)
  (let ((pt (point-min)))
    (with-output-to-temp-buffer "*GPTEL Text Properties*"
      (princ (format "=== GPTEL Text Properties in %s ===\n\n" (buffer-name)))

      (while (< pt (point-max))
        (let ((next-change (next-single-property-change pt 'gptel nil (point-max)))
              (prop-value (get-text-property pt 'gptel)))
          (when prop-value
            (let* ((line-start (line-number-at-pos pt))
                   (line-end (line-number-at-pos (or next-change (point-max))))
                   (content (buffer-substring-no-properties
                            pt (min (or next-change (point-max))
                                   (+ pt 60)))))
              (princ (format "Position %d-%s (lines %d-%d)\n"
                            pt (or next-change "END")
                            line-start line-end))
              (princ (format "  Property: %S\n" prop-value))
              (princ (format "  Content: %S%s\n\n"
                            content
                            (if (> (- (or next-change (point-max)) pt) 60)
                                "..." "")))))
          (setq pt (or next-change (point-max))))))))

(defun jf/gptel--start-diagnostic-session ()
  "Enable all diagnostics for a test session."
  (interactive)
  (setq jf/gptel--bounds-history nil)
  (setq jf/gptel--parse-trace nil)

  ;; Install all advice
  (advice-add 'gptel--restore-props :before
              (lambda (&rest _)
                (jf/gptel--record-bounds "BEFORE-restore-props")))

  ;; Trace what add-text-properties is called with during restoration
  (advice-add 'add-text-properties :before
              (lambda (beg end props &optional _)
                (when (and (plist-get props 'gptel)
                           (string-match-p "session\\.md" (or (buffer-file-name) "")))
                  (let ((log-file (jf/gptel--get-diagnostic-file)))
                    (with-temp-buffer
                      (insert (format "add-text-properties called: %d-%d props=%S\n"
                                     beg end (plist-get props 'gptel)))
                      (append-to-file (point-min) (point-max) log-file)))))
              '((name . gptel-diagnostics)))
  (advice-add 'gptel--restore-props :after
              (lambda (&rest _)
                (jf/gptel--record-bounds "AFTER-restore-props")
                ;; Also scan actual buffer properties to verify restoration
                (let* ((actual-tools (jf/gptel--scan-tool-positions))
                       (log-file (jf/gptel--get-diagnostic-file)))
                  (when actual-tools
                    (message "[BOUNDS] ACTUAL buffer properties after restore: %d tools found"
                             (length actual-tools))
                    ;; Write to log file
                    (with-temp-buffer
                      (insert (format "\n=== ACTUAL BUFFER PROPERTIES (after restore) ===\n"))
                      (insert (format "Found %d tool properties in buffer:\n" (length actual-tools)))
                      (dolist (tool actual-tools)
                        (let* ((pos (plist-get tool :position))
                               (gptel-prop (save-excursion
                                            (with-current-buffer (get-buffer "session.md")
                                              (get-text-property pos 'gptel))))
                               (text-sample (save-excursion
                                             (with-current-buffer (get-buffer "session.md")
                                               (buffer-substring-no-properties
                                                pos (min (point-max) (+ pos 20)))))))
                          (insert (format "  Position %d (line %d): name=%s\n"
                                         pos
                                         (plist-get tool :line)
                                         (or (plist-get tool :name) "nil")))
                          (insert (format "    Property value: %S\n" gptel-prop))
                          (insert (format "    Text at position: %S\n" text-sample))))
                      (insert "\n")
                      (append-to-file (point-min) (point-max) log-file))
                    (dolist (tool actual-tools)
                      (message "[BOUNDS]   Position %d: %s"
                               (plist-get tool :position)
                               (plist-get tool :name)))))))
  (advice-add 'gptel--get-buffer-bounds :around
              (lambda (orig-fn &rest args)
                (jf/gptel--record-bounds "BEFORE-get-buffer-bounds")
                (let ((result (apply orig-fn args)))
                  (jf/gptel--record-bounds "AFTER-get-buffer-bounds"
                                          (list :result result))
                  result)))
  (advice-add 'gptel--save-state :before
              (lambda (&rest _)
                (jf/gptel--record-bounds "BEFORE-save-state")))
  (advice-add 'gptel--save-state :after
              (lambda (&rest _)
                (jf/gptel--record-bounds "AFTER-save-state")))
  (advice-add 'jf/gptel--auto-init-session-buffer :before
              (lambda (&rest _)
                (jf/gptel--record-bounds "BEFORE-auto-init")))
  (advice-add 'jf/gptel--auto-init-session-buffer :after
              (lambda (&rest _)
                (jf/gptel--record-bounds "AFTER-auto-init")))
  (advice-add 'gptel--parse-buffer :around
              #'jf/gptel--trace-parse-buffer-advice)

  (message "Diagnostic session started. Run test flow, then call jf/gptel--dump-diagnostic-report"))

(defun jf/gptel--dump-diagnostic-report ()
  "Dump complete diagnostic report to a file."
  (interactive)
  (let ((report-file (expand-file-name
                     (format "gptel-diagnostic-%s.org"
                            (format-time-string "%Y%m%d-%H%M%S"))
                     "~/")))
    (with-temp-file report-file
      (insert "#+TITLE: GPTEL Diagnostic Report\n")
      (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))

      ;; Summary section
      (insert "* Summary\n\n")
      (insert "** Tool Parse Results by Phase\n\n")
      (dolist (entry (reverse jf/gptel--bounds-history))
        (when-let ((tool-parses (plist-get entry :tool-parses)))
          (insert (format "*** %s (%s)\n"
                         (plist-get entry :phase)
                         (plist-get entry :timestamp)))
          (dolist (parse tool-parses)
            (if (plist-get parse :success)
                (insert (format "- ✓ Position %d: ~%s~ with args\n"
                               (plist-get parse :position)
                               (plist-get parse :name)))
              (insert (format "- ✗ Position %d: ERROR - %s\n"
                             (plist-get parse :position)
                             (plist-get parse :error)))))
          (insert "\n")))

      (insert "** Parse Buffer Calls\n\n")
      (dolist (trace (reverse jf/gptel--parse-trace))
        (when (equal (plist-get trace :phase) "parse-buffer-call")
          (insert (format "*** %s - %d tools found\n"
                         (plist-get trace :timestamp)
                         (length (plist-get trace :tool-scans))))
          (dolist (scan (plist-get trace :tool-scans))
            (if (plist-get scan :success)
                (insert (format "- ✓ Position %d: ~%s~\n"
                               (plist-get scan :position)
                               (plist-get scan :name)))
              (insert (format "- ✗ Position %d: %s\n"
                             (plist-get scan :position)
                             (plist-get scan :error)))))
          (insert "\n")))

      ;; Raw data sections
      (insert "* Bounds History (Full Data)\n\n")
      (insert "#+BEGIN_SRC elisp\n")
      (pp jf/gptel--bounds-history (current-buffer))
      (insert "\n#+END_SRC\n\n")

      (insert "* Parse Trace (Full Data)\n\n")
      (insert "#+BEGIN_SRC elisp\n")
      (pp jf/gptel--parse-trace (current-buffer))
      (insert "\n#+END_SRC\n\n")

      (insert "* Current Buffer State\n\n")
      (when (buffer-live-p (get-buffer "session.md"))
        (with-current-buffer "session.md"
          (insert (format "** gptel--bounds\n\n"))
          (insert "#+BEGIN_SRC elisp\n")
          (when (boundp 'gptel--bounds)
            (pp gptel--bounds (current-buffer)))
          (insert "\n#+END_SRC\n\n"))))

    (message "Diagnostic report written to %s" report-file)
    (find-file report-file)))

(defun jf/gptel--stop-diagnostic-session ()
  "Disable all diagnostics and clean up."
  (interactive)
  (advice-remove 'gptel--restore-props
                 (lambda (&rest _)
                   (jf/gptel--record-bounds "BEFORE-restore-props")))
  (advice-remove 'gptel--restore-props
                 (lambda (&rest _)
                   (jf/gptel--record-bounds "AFTER-restore-props")))
  (advice-remove 'gptel--get-buffer-bounds
                 (lambda (orig-fn &rest args)
                   (jf/gptel--record-bounds "BEFORE-get-buffer-bounds")
                   (let ((result (apply orig-fn args)))
                     (jf/gptel--record-bounds "AFTER-get-buffer-bounds"
                                             (list :result result))
                     result)))
  (advice-remove 'gptel--save-state
                 (lambda (&rest _)
                   (jf/gptel--record-bounds "BEFORE-save-state")))
  (advice-remove 'gptel--save-state
                 (lambda (&rest _)
                   (jf/gptel--record-bounds "AFTER-save-state")))
  (advice-remove 'jf/gptel--auto-init-session-buffer
                 (lambda (&rest _)
                   (jf/gptel--record-bounds "BEFORE-auto-init")))
  (advice-remove 'jf/gptel--auto-init-session-buffer
                 (lambda (&rest _)
                   (jf/gptel--record-bounds "AFTER-auto-init")))
  (advice-remove 'gptel--parse-buffer #'jf/gptel--trace-parse-buffer-advice)
  (advice-remove 'add-text-properties 'gptel-diagnostics)

  (message "Diagnostic session stopped"))

;; Auto-start diagnostics when this file is loaded
(jf/gptel--start-diagnostic-session)
(message "GPTEL diagnostics auto-enabled - logging to %s"
         jf/gptel--diagnostic-fallback-file)
(message "Session diagnostics will write to: <session-dir>/diagnostics.log")

(provide 'jf-gptel-diagnostics)
