;;; branching.el --- GPTEL Session Branching -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Session branching support for gptel persistent sessions.
;; Enables creating divergent conversation paths from any user prompt.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-metadata)

(defun jf/gptel--session-file-format (buffer-or-file)
  "Detect the session file format from BUFFER-OR-FILE.
BUFFER-OR-FILE may be a buffer object or a file path string.

Returns a keyword indicating the format:
  :org  - org-mode format (file extension \"org\")
  :md   - markdown format (file extension \"md\")
  nil   - unknown or unrecognized extension

Use with pcase for idiomatic dispatch:
  (pcase (jf/gptel--session-file-format (current-buffer))
    (:org (message \"Org-mode session\"))
    (:md  (message \"Markdown session\")))"
  (let* ((file-path (cond
                     ((bufferp buffer-or-file)
                      (buffer-file-name buffer-or-file))
                     ((stringp buffer-or-file)
                      buffer-or-file)
                     (t nil)))
         (ext (when file-path
                (file-name-extension file-path))))
    (cond
     ((equal ext "org") :org)
     ((equal ext "md")  :md)
     (t nil))))

(defun jf/gptel--get-session-file-extension (buffer)
  "Get the file extension string for BUFFER's session format.
Returns \"org\" or \"md\" corresponding to the detected format.

Intended for use in file path construction when creating branch
sessions that inherit their parent's format:
  (let* ((ext (jf/gptel--get-session-file-extension (current-buffer)))
         (filename (concat \"session.\" ext)))
    ...)

Returns nil if the format cannot be determined."
  (pcase (jf/gptel--session-file-format buffer)
    (:org "org")
    (:md  "md")
    (_    nil)))

(defun jf/gptel--find-user-prompts ()
  "Find all user prompt positions in current buffer.
Returns list of (POSITION . PROMPT-TEXT) where POSITION is start of prompt.
User prompts are text regions without a gptel text property."
  (save-excursion
    (save-restriction
      (widen)
      (let ((prompts nil)
            (pos (point-min)))
        ;; Scan for regions without gptel property
        (while (< pos (point-max))
          (let* ((next-change (or (next-single-property-change pos 'gptel)
                                  (point-max)))
                 (has-property (get-text-property pos 'gptel)))
            (unless has-property
              ;; This region has no gptel property - might be a user prompt
              (let* ((region-text (buffer-substring-no-properties pos next-change))
                     (trimmed (string-trim region-text)))
                ;; Only include non-empty, non-whitespace regions
                (when (and (not (string-empty-p trimmed))
                          ;; Skip if it's just newlines or whitespace
                          (not (string-match-p "^[\s\n]*$" trimmed)))
                  ;; Get preview text (first line, up to 60 chars)
                  (let* ((first-line (car (split-string trimmed "\n")))
                         (preview (if (> (length first-line) 60)
                                     (concat (substring first-line 0 57) "...")
                                   first-line)))
                    (push (cons pos preview) prompts)))))
            (setq pos next-change)))
        (nreverse prompts)))))

(defun jf/gptel--select-branch-point ()
  "Interactively select a branch point from user prompts.
Returns (POSITION . INCLUDE-P) where:
  POSITION - buffer position of selected prompt
  INCLUDE-P - t to include prompt in branch, nil to exclude"
  (let ((prompts (jf/gptel--find-user-prompts)))
    (if (null prompts)
        (error "No user prompts found in current session")
      ;; Build numbered list for completing-read
      (let* ((choices (cl-loop for (pos . text) in prompts
                              for i from 1
                              collect (cons (format "%d. %s" i text) pos)))
             (selected (completing-read "Select branch point: "
                                       (mapcar #'car choices)
                                       nil t))
             (position (cdr (assoc selected choices)))
             (include-p (y-or-n-p "Include this prompt in the branch? ")))
        (cons position include-p)))))

(defun jf/gptel--filter-bounds-before-position (bounds-alist position)
  "Filter BOUNDS-ALIST to only include complete regions before POSITION.
Returns new bounds alist with only regions that START before POSITION.
Regions that start before but end after POSITION are EXCLUDED (partial overlap)."
  (let ((filtered nil))
    (dolist (bounds-entry bounds-alist)
      (let* ((type (car bounds-entry))
             (regions (cdr bounds-entry))
             (filtered-regions
              (seq-filter
               (lambda (region)
                 ;; region is (BEG END) or (BEG END ID)
                 ;; Only include if the region starts before the branch point
                 (< (car region) position))
               regions)))
        ;; Only include this type if it has regions after filtering
        (when filtered-regions
          (push (cons type filtered-regions) filtered))))
    (nreverse filtered)))

(defun jf/gptel--validate-bounds (bounds-alist)
  "Validate that BOUNDS-ALIST is well-formed.
Returns t if valid, signals error with helpful message if not."
  (dolist (bounds-entry bounds-alist)
    (let ((type (car bounds-entry))
          (regions (cdr bounds-entry)))
      (unless (symbolp type)
        (error "Invalid bounds type (expected symbol): %S" type))
      (dolist (region regions)
        (unless (and (listp region)
                    (>= (length region) 2)
                    (<= (length region) 3))
          (error "Invalid region format (expected (BEG END) or (BEG END ID)): %S" region))
        (let ((beg (car region))
              (end (cadr region)))
          (unless (and (integerp beg) (integerp end))
            (error "Region positions must be integers: %S" region))
          (unless (< beg end)
            (error "Region start must be before end: %S" region))))))
  t)

(defun jf/gptel--write-local-variables (format vars)
  "Write Emacs Local Variables block in FORMAT-appropriate syntax.
FORMAT is :org or :md keyword.
VARS is an alist of (SYMBOL . VALUE) pairs.

For :org format, writes:
  # Local Variables:
  # <var>: <value>
  # End:

For :md format, writes:
  <!-- Local Variables: -->
  <!-- <var>: <value> -->
  <!-- End: -->

VALUES are serialized with prin1-to-string to handle complex data structures.
Inserts text at point in the current buffer."
  (pcase format
    (:org
     (insert "# Local Variables:\n")
     (dolist (pair vars)
       (insert (format "# %s: %s\n"
                       (car pair)
                       (prin1-to-string (cdr pair)))))
     (insert "# End:\n"))
    (:md
     (insert "<!-- Local Variables: -->\n")
     (dolist (pair vars)
       (insert (format "<!-- %s: %s -->\n"
                       (car pair)
                       (prin1-to-string (cdr pair)))))
     (insert "<!-- End: -->\n"))
    (_
     (error "Unknown format: %S (expected :org or :md)" format))))

(defun jf/gptel--copy-truncated-context (source-file dest-file branch-position)
  "Copy SOURCE-FILE to DEST-FILE, truncating at BRANCH-POSITION.
Filters gptel--bounds in Local Variables to only include regions before branch point.
Detects source format and writes Local Variables in matching format.
Returns t on success, signals error on failure."
  ;; Read Local Variables from source file by visiting it in a temp buffer
  ;; and letting Emacs parse them via hack-local-variables
  (let ((source-format (jf/gptel--session-file-format source-file))
        (bounds nil)
        (gptel-model-val nil)
        (gptel-backend-val nil)
        (gptel-tools-val nil))

    ;; Extract buffer-local variables by visiting the file with local vars enabled
    (with-temp-buffer
      (insert-file-contents source-file)
      (setq buffer-file-name source-file)
      ;; Enable hack-local-variables to parse the Local Variables block
      (let ((enable-local-variables t)
            (hack-local-variables-hook nil))
        (hack-local-variables))
      ;; Extract the buffer-local values after parsing
      (setq bounds (and (boundp 'gptel--bounds) (buffer-local-value 'gptel--bounds (current-buffer))))
      (setq gptel-model-val (and (boundp 'gptel-model) (buffer-local-value 'gptel-model (current-buffer))))
      (setq gptel-backend-val (and (boundp 'gptel--backend-name) (buffer-local-value 'gptel--backend-name (current-buffer))))
      (setq gptel-tools-val (and (boundp 'gptel--tool-names) (buffer-local-value 'gptel--tool-names (current-buffer)))))

    (unless bounds
      (error "No bounds found in source file: %s" source-file))

    ;; Validate original bounds
    (jf/gptel--validate-bounds bounds)

    ;; Filter bounds to only include regions before branch point
    (let ((filtered-bounds (jf/gptel--filter-bounds-before-position bounds branch-position)))

      ;; Validate filtered bounds
      (jf/gptel--validate-bounds filtered-bounds)

      ;; Now work in a temp buffer to build the truncated output
      (with-temp-buffer
        (insert-file-contents source-file)

        ;; Truncate text at branch position
        (delete-region (min branch-position (point-max)) (point-max))

        ;; Remove old Local Variables block (handle both org and md formats)
        (goto-char (point-min))
        (let ((lv-start-re (pcase source-format
                             (:org "^# Local Variables:$")
                             (:md "^<!-- Local Variables: -->$")
                             (_ "^\\(?:# Local Variables:\\|<!-- Local Variables: -->\\)$")))
              (lv-end-re (pcase source-format
                           (:org "^# End:$")
                           (:md "^<!-- End: -->$")
                           (_ "^\\(?:# End:\\|<!-- End: -->\\)$"))))
          (when (re-search-forward lv-start-re nil t)
            (let ((start (match-beginning 0)))
              (when (re-search-forward lv-end-re nil t)
                (delete-region start (match-end 0))
                ;; Clean up trailing newlines
                (when (looking-at "\n+")
                  (delete-region (match-beginning 0) (match-end 0)))))))

        ;; Append separator and new Local Variables block
        (goto-char (point-max))
        (unless (looking-back "\n\n" nil)
          (insert "\n"))
        (insert "\n")

        ;; Build vars alist for the writer
        (let ((vars nil))
          (when gptel-model-val
            (push (cons 'gptel-model gptel-model-val) vars))
          (when gptel-backend-val
            (push (cons 'gptel--backend-name gptel-backend-val) vars))
          (when gptel-tools-val
            (push (cons 'gptel--tool-names gptel-tools-val) vars))
          (push (cons 'gptel--bounds filtered-bounds) vars)
          ;; Write in reverse so order matches original (model, backend, tools, bounds)
          (jf/gptel--write-local-variables
           (or source-format :md)
           (nreverse vars)))

        ;; Write complete file (with Local Variables) in one operation
        (write-region (point-min) (point-max) dest-file nil 'silent))

      (jf/gptel--log 'info "Copied truncated context: %d -> %d chars, bounds: %d types, format: %s"
                    (with-temp-buffer
                      (insert-file-contents source-file)
                      (buffer-size))
                    branch-position
                    (length filtered-bounds)
                    source-format)
      t)))

(defun jf/gptel--copy-branch-agents (parent-branch-dir branch-dir)
  "Copy agent subdirectories from PARENT-BRANCH-DIR/agents to BRANCH-DIR/agents.
For MVP, copies ALL agent directories.
Future enhancement: Only copy agents invoked before branch point."
  (let ((parent-agents-dir (jf/gptel--agents-dir-path parent-branch-dir))
        (branch-agents-dir (jf/gptel--agents-dir-path branch-dir)))
    (when (file-directory-p parent-agents-dir)
      ;; Create agents directory in branch
      (make-directory branch-agents-dir t)

      ;; Copy each agent directory
      (dolist (agent-dir (directory-files parent-agents-dir t "^[^.]"))
        (when (file-directory-p agent-dir)
          (let* ((agent-name (file-name-nondirectory agent-dir))
                 (dest-dir (expand-file-name agent-name branch-agents-dir)))
            (copy-directory agent-dir dest-dir t t t)
            (jf/gptel--log 'debug "Copied agent directory: %s" agent-name))))

      (jf/gptel--log 'info "Copied %d agent directories to branch"
                    (length (directory-files parent-agents-dir nil "^[^.]"))))))

(defun jf/gptel--create-branch-session (session-dir parent-branch-name branch-name branch-position include-prompt-p)
  "Create new branch inside SESSION-DIR.
SESSION-DIR - session directory containing branches
PARENT-BRANCH-NAME - name of parent branch (e.g., \"main\")
BRANCH-NAME - user-provided name for branch
BRANCH-POSITION - buffer position of branch point
INCLUDE-PROMPT-P - whether to include the branch point prompt

Returns new branch directory path."
  (let* ((parent-branch-dir (jf/gptel--branch-dir-path session-dir parent-branch-name))
         ;; Create timestamped branch name
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (new-branch-name (format "%s-%s" timestamp branch-name))
         ;; Create new branch directory
         (branch-dir (jf/gptel--create-branch-directory session-dir new-branch-name))
         (parent-context (jf/gptel--context-file-path parent-branch-dir))
         (branch-context (jf/gptel--context-file-path branch-dir))
         ;; Adjust position based on include-prompt-p
         (truncate-position (if include-prompt-p
                               ;; Find the next property change after branch-position
                               ;; (this skips the user prompt to just after it)
                               (with-current-buffer (current-buffer)
                                 (or (next-single-property-change branch-position 'gptel)
                                     (point-max)))
                             ;; Exclude: truncate exactly at the prompt
                             branch-position)))

    (jf/gptel--log 'info "Creating branch: %s from parent: %s" new-branch-name parent-branch-name)

    ;; Copy scope.yml from parent branch (new format)
    (let ((parent-scope-yml (jf/gptel--scope-file-path parent-branch-dir))
          (branch-scope-yml (jf/gptel--scope-file-path branch-dir)))
      (when (file-exists-p parent-scope-yml)
        (copy-file parent-scope-yml branch-scope-yml t)
        (jf/gptel--log 'info "Copied scope.yml to branch")))

    ;; Copy metadata.yml from parent branch (new format)
    (let ((parent-metadata (jf/gptel--metadata-file-path parent-branch-dir))
          (branch-metadata (jf/gptel--metadata-file-path branch-dir)))
      (when (file-exists-p parent-metadata)
        (copy-file parent-metadata branch-metadata t)
        (jf/gptel--log 'info "Copied metadata.yml to branch")))

    ;; Create branch-metadata.yml
    (jf/gptel--write-branch-metadata branch-dir parent-branch-name branch-position)

    ;; Copy agent directories from parent branch
    (jf/gptel--copy-branch-agents parent-branch-dir branch-dir)

    ;; Copy and truncate context file
    (jf/gptel--copy-truncated-context parent-context branch-context truncate-position)

    ;; Update current symlink to point to new branch
    (jf/gptel--update-current-symlink session-dir new-branch-name)

    (jf/gptel--log 'info "Branch created successfully: %s" new-branch-name)
    branch-dir))

(defun jf/gptel-branch-session (&optional branch-name)
  "Create a branch from current branch at a selected user prompt.

Interactively:
1. Prompts user to select a branch point (numbered list of user prompts)
2. Asks whether to include or exclude the selected prompt
3. Prompts for branch name
4. Creates new branch directory with copied metadata and truncated context
5. Opens new branch session buffer

The new branch is created in the same session with:
- Timestamped branch name: <timestamp>-<user-name>
- Copied scope.yml and metadata.yml from parent
- branch-metadata.yml tracking parent branch name
- Copied agent subdirectories from parent branch
- session.md truncated at branch point with filtered bounds
- Current symlink updated to point to new branch

After creation, the branch can evolve independently from parent."
  (interactive)

  ;; Verify current buffer is a branch session
  (unless (and (boundp 'jf/gptel--session-dir) jf/gptel--session-dir
              (boundp 'jf/gptel--branch-name) jf/gptel--branch-name)
    (error "Current buffer is not a gptel branch session"))

  (unless gptel-mode
    (error "Current buffer is not in gptel-mode"))

  ;; Select branch point
  (let* ((branch-point-data (jf/gptel--select-branch-point))
         (branch-position (car branch-point-data))
         (include-prompt-p (cdr branch-point-data))
         ;; Save parent info before switching buffers
         (parent-branch-name jf/gptel--branch-name)
         (parent-session-id jf/gptel--session-id)
         ;; Prompt for branch name
         (user-branch-name (or branch-name
                              (read-string "Branch name: ")))
         ;; Create branch
         (new-branch-dir (jf/gptel--create-branch-session
                         jf/gptel--session-dir
                         parent-branch-name      ; parent branch name
                         user-branch-name        ; user-provided name
                         branch-position
                         include-prompt-p))
         (new-branch-file (jf/gptel--context-file-path new-branch-dir))
         (new-branch-name (file-name-nondirectory new-branch-dir)))

    ;; Open new branch session buffer (auto-initializes via find-file-hook)
    (find-file new-branch-file)

    (message "Created branch: %s\nFrom parent: %s\nSession: %s"
             new-branch-name
             parent-branch-name
             parent-session-id)))

(provide 'gptel-session-branching)
;;; branching.el ends here
