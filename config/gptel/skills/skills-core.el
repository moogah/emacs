;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'gptel nil t)

(defgroup gptel-skills nil
  "Skills system for gptel with @mention activation."
  :group 'gptel
  :prefix "jf/gptel-skills-")

(defcustom jf/gptel-skills-directory "~/.claude/skills"
  "Directory containing Claude Code skills.
Skills should be organized as SKILL-NAME/SKILL.md."
  :type 'directory
  :group 'gptel-skills)

(defcustom jf/gptel-skills-mention-prefix "@"
  "Prefix character for skill mentions in buffer."
  :type 'string
  :group 'gptel-skills)

(defcustom jf/gptel-skills-strip-mentions t
  "Whether to strip @mentions from prompt before sending.
If nil, mentions are kept in the prompt (visible to LLM)."
  :type 'boolean
  :group 'gptel-skills)

(defcustom jf/gptel-skills-auto-expand nil
  "Automatically load referenced resources without prompting."
  :type 'boolean
  :group 'gptel-skills)

(defcustom jf/gptel-skills-verbose nil
  "When non-nil, print verbose messages during skill operations."
  :type 'boolean
  :group 'gptel-skills)

(defface jf/gptel-skills-mention-face
  '((t :inherit font-lock-keyword-face :underline t))
  "Face for @skill mentions in gptel buffers.")

(defvar jf/gptel-skills--registry (make-hash-table :test 'equal)
  "Hash table mapping skill names to metadata plists.

This registry contains both markdown (.md) and org-roam skills.

Each entry is a plist with keys:
  :name          - Skill name (string)
  :description   - Description for completion/help (string)
  :path          - Full path to SKILL.md file (string) [markdown only]
  :dir           - Skill directory path (string) [markdown only]
  :file          - Full path to .org skill file (string) [org-roam only]
  :source        - Source type: 'markdown or 'org-roam (symbol)
  :loaded        - Whether content has been loaded (boolean)
  :content       - Cached skill content (string or nil)")

(defvar-local jf/gptel-skills--active nil
  "List of skill names active in current buffer via @mentions.")

(defvar-local jf/gptel-skills--overlays nil
  "List of skill mention overlays in current buffer.")

(defun jf/gptel-skills--discover ()
  "Scan skills directory and return list of SKILL.md file paths.
Returns list of absolute paths to SKILL.md files."
  (let ((skills-dir (expand-file-name jf/gptel-skills-directory)))
    (when (file-directory-p skills-dir)
      (let ((skill-files '()))
        (dolist (entry (directory-files skills-dir t "^[^.]" t))
          (when (file-directory-p entry)
            (let ((skill-file (expand-file-name "SKILL.md" entry)))
              (when (file-exists-p skill-file)
                (push skill-file skill-files)))))
        (nreverse skill-files)))))

(defun jf/gptel-skills--parse-metadata (skill-path)
  "Parse YAML frontmatter from SKILL.md at SKILL-PATH.
Returns plist with :name, :description, :path, :dir.
Returns nil if parsing fails."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents skill-path)
        (goto-char (point-min))

        ;; Check for YAML frontmatter delimiter
        (if (not (looking-at "^---[ \t]*$"))
            (progn
              (when jf/gptel-skills-verbose
                (message "Warning: No YAML frontmatter in %s" skill-path))
              nil)

          ;; Parse YAML frontmatter
          (forward-line 1)
          (let ((yaml-start (point))
                (yaml-end nil)
                (metadata '()))

            ;; Find end of frontmatter
            (when (re-search-forward "^---[ \t]*$" nil t)
              (setq yaml-end (match-beginning 0))

              ;; Parse name
              (goto-char yaml-start)
              (when (re-search-forward "^name:[ \t]+\\(.+\\)$" yaml-end t)
                (setq metadata (plist-put metadata :name (string-trim (match-string 1)))))

              ;; Parse description
              (goto-char yaml-start)
              (when (re-search-forward "^description:[ \t]+\\(.+\\)$" yaml-end t)
                (setq metadata (plist-put metadata :description (string-trim (match-string 1)))))

              ;; Add path, directory, and source
              (setq metadata (plist-put metadata :path skill-path))
              (setq metadata (plist-put metadata :dir (file-name-directory skill-path)))
              (setq metadata (plist-put metadata :source 'markdown))

              ;; Set defaults
              (unless (plist-get metadata :name)
                (setq metadata (plist-put metadata :name
                                          (file-name-base (directory-file-name
                                                          (file-name-directory skill-path))))))
              (unless (plist-get metadata :description)
                (setq metadata (plist-put metadata :description
                                          (plist-get metadata :name))))

              ;; Initialize loading state
              (setq metadata (plist-put metadata :loaded nil))
              (setq metadata (plist-put metadata :content nil))

              metadata))))
    (error
     (message "Error parsing skill metadata from %s: %s" skill-path (error-message-string err))
     nil)))

(defun jf/gptel-skills--parse-content (skill-path)
  "Read full SKILL.md content from SKILL-PATH, excluding YAML frontmatter.
Returns content as string, or nil on error."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents skill-path)
        (goto-char (point-min))

        ;; Skip YAML frontmatter if present
        (when (looking-at "^---[ \t]*$")
          (forward-line 1)
          (when (re-search-forward "^---[ \t]*$" nil t)
            (forward-line 1)))

        ;; Return rest of buffer
        (buffer-substring-no-properties (point) (point-max)))
    (error
     (message "Error reading skill content from %s: %s" skill-path (error-message-string err))
     nil)))

(defun jf/gptel-skills--load-resource (skill-dir resource-file)
  "Load additional resource file from SKILL-DIR.
RESOURCE-FILE is relative filename (e.g., 'REFERENCE.md').
Returns content as string, or nil if file doesn't exist."
  (let ((resource-path (expand-file-name resource-file skill-dir)))
    (when (file-exists-p resource-path)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents resource-path)
            (buffer-string))
        (error
         (message "Error loading resource %s: %s" resource-path (error-message-string err))
         nil)))))

(defun jf/gptel-skills--detect-mentions (&optional buffer)
  "Scan BUFFER for @skill-name patterns.
Returns list of (skill-name . position) tuples for valid skills.
If BUFFER is nil, uses current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((mentions '())
          (prefix (regexp-quote jf/gptel-skills-mention-prefix))
          (skill-names (hash-table-keys jf/gptel-skills--registry)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat prefix "\\([a-zA-Z0-9_-]+\\)") nil t)
          (let ((skill-name (match-string 1))
                (pos (match-beginning 0)))
            ;; Only include if skill exists in registry
            (when (member skill-name skill-names)
              (push (cons skill-name pos) mentions)))))
      (nreverse mentions))))

(defun jf/gptel-skills--add-overlay (beg end skill-name)
  "Create overlay marking @mention from BEG to END for SKILL-NAME."
  (let* ((metadata (gethash skill-name jf/gptel-skills--registry))
         (description (plist-get metadata :description))
         (ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'face 'jf/gptel-skills-mention-face)
    (overlay-put ov 'gptel-skill skill-name)
    (overlay-put ov 'help-echo description)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'priority 100)
    (push ov jf/gptel-skills--overlays)
    ov))

(defun jf/gptel-skills--update-overlays (&optional _beg _end _len)
  "Update overlays and active skills list based on current @mentions.
Designed to be called from after-change-functions."
  (when (and (bound-and-true-p gptel-mode)  ; gptel-mode is a minor mode
             (hash-table-p jf/gptel-skills--registry)
             (> (hash-table-count jf/gptel-skills--registry) 0))
    ;; Remove all existing overlays
    (mapc #'delete-overlay jf/gptel-skills--overlays)
    (setq jf/gptel-skills--overlays nil)

    ;; Detect current mentions
    (let* ((mentions (jf/gptel-skills--detect-mentions))
           (skill-names (mapcar #'car mentions)))

      ;; Create overlays for each mention
      (dolist (mention mentions)
        (let* ((skill-name (car mention))
               (pos (cdr mention))
               (end-pos (+ pos (length jf/gptel-skills-mention-prefix)
                          (length skill-name))))
          (jf/gptel-skills--add-overlay pos end-pos skill-name)))

      ;; Update active skills list
      (setq jf/gptel-skills--active (delete-dups skill-names)))))

(defun jf/gptel-skills--completion-at-point ()
  "Provide completion for @skill mentions.
Integrates with completion-at-point-functions."
  (when (and (bound-and-true-p gptel-mode)  ; gptel-mode is a minor mode
             (> (hash-table-count jf/gptel-skills--registry) 0))
    (let* ((prefix (regexp-quote jf/gptel-skills-mention-prefix))
           (bounds (bounds-of-thing-at-point 'symbol))
           (start (and bounds (car bounds)))
           (end (and bounds (cdr bounds))))
      ;; Check if we're after @ prefix
      (when (and start
                 (> start (point-min))
                 (string= (buffer-substring-no-properties (1- start) start)
                          jf/gptel-skills-mention-prefix))
        (list (1- start)  ; include the @
              end
              (mapcar (lambda (name)
                       (concat jf/gptel-skills-mention-prefix name))
                     (hash-table-keys jf/gptel-skills--registry))
              :exclusive 'no
              :annotation-function
              (lambda (candidate)
                (let* ((skill-name (substring candidate (length jf/gptel-skills-mention-prefix)))
                       (metadata (gethash skill-name jf/gptel-skills--registry))
                       (source (plist-get metadata :source)))
                  (cond
                   ((eq source 'org-roam) " (org-roam)")
                   ((eq source 'markdown) " (md)")
                   (t "")))))))))

(defun jf/gptel-skills--transform-inject (fsm)
  "Main prompt transform function for injecting skills.

Skills are injected as a multi-part system message, where each skill
is a separate element in the gptel--system-message list. This enables
independent caching per skill when using Anthropic's API with prompt caching.

SKILL ORDERING:
Skills are ordered as follows:
1. Skills from buffer-local `gptel-skills' variable (transient menu)
   appear first, in the order they appear in that list
2. Additional skills from @mentions appear after, sorted alphabetically

The order matters for caching: changing skill order invalidates the cache,
as Anthropic's prompt caching is position-sensitive.

DETECTION SOURCES:
Detects skills from two sources:
1. Buffer-local gptel-skills variable (set via transient menu)
2. @mentions in the prompt (backward compatibility)

Loads content and injects to system message. Deduplicates skills.
Added to gptel-prompt-transform-functions. FSM is the state machine."
  (when (> (hash-table-count jf/gptel-skills--registry) 0)
    ;; Collect skill names from both sources
    (let* ((mention-data (jf/gptel-skills--detect-mentions))
           ;; Extract skill names from mentions
           (mention-names (mapcar #'car mention-data))
           ;; Get skills from buffer-local variable in the ORIGINAL buffer
           ;; (transform runs in temp buffer, need to access original)
           (original-buffer (plist-get (gptel-fsm-info fsm) :buffer))
           (buffer-local-skills (when original-buffer
                                  (buffer-local-value 'gptel-skills original-buffer)))
           ;; Combine and deduplicate
           (all-skill-names (delete-dups (append buffer-local-skills mention-names))))

      (when all-skill-names
        ;; Sort skills: buffer-local gptel-skills order first, then alphabetically
        ;; Order matters for caching: changing skill order invalidates cache
        (let* ((ordered-skills
                (append
                 ;; First, skills from buffer-local variable in their specified order
                 (cl-remove-if-not (lambda (name) (member name all-skill-names))
                                   buffer-local-skills)
                 ;; Then, any remaining skills (from @mentions) alphabetically
                 (sort (cl-remove-if (lambda (name) (member name buffer-local-skills))
                                     all-skill-names)
                       #'string<))))

          (dolist (skill-name ordered-skills)
            (let ((metadata (gethash skill-name jf/gptel-skills--registry)))
              (when metadata
                ;; Load content if not already loaded
                (unless (plist-get metadata :loaded)
                  (let* ((source (plist-get metadata :source))
                         (content (cond
                                   ;; Markdown skills - use existing parser
                                   ((eq source 'markdown)
                                    (jf/gptel-skills--parse-content (plist-get metadata :path)))
                                   ;; Org-roam skills - read file directly
                                   ((eq source 'org-roam)
                                    (let ((file (plist-get metadata :file)))
                                      (when (and file (file-exists-p file))
                                        (with-temp-buffer
                                          (insert-file-contents file)
                                          (buffer-string)))))
                                   (t nil))))
                    (when content
                      (plist-put metadata :loaded t)
                      (plist-put metadata :content content)
                      (puthash skill-name metadata jf/gptel-skills--registry))))

                ;; Get content and inject to system message
                (let ((content (plist-get metadata :content)))
                  (when content
                    (when jf/gptel-skills-verbose
                      (message "Injecting skill: %s" skill-name))
                    ;; Inject to system message
                    (jf/gptel-skills--inject-content content skill-name))))))))

        ;; Strip @mentions if configured (only strip actual mentions, not buffer-local skills)
        (when (and jf/gptel-skills-strip-mentions mention-data)
          (jf/gptel-skills--strip-mentions)))))

(defun jf/gptel-skills--inject-content (content skill-name)
  "Inject CONTENT for SKILL-NAME into system message as a separate part.

Each skill becomes a distinct element in the gptel--system-message list,
enabling independent caching per skill when using Anthropic's API with
prompt caching (see `gptel-cache').

When gptel--system-message is a list, gptel-anthropic.el (lines 220-226)
automatically adds cache_control blocks to each element.

NOTE: For multi-part system messages with Anthropic, the format must be
double-nested: (( \"part1\" \"part2\" ...)) because gptel interprets a
single-level list as (system-message user1 llm1 ...) directive format."
  ;; Ensure gptel--system-message is in the correct format for building
  ;; We want to work with a single-level list and wrap it at the end
  (cond
   ;; If it's nil, initialize with empty string
   ((null gptel--system-message)
    (setq gptel--system-message (list "")))
   ;; If it's a string, convert to single-level list
   ((stringp gptel--system-message)
    (setq gptel--system-message
          (list (if (string-empty-p gptel--system-message)
                    ""
                  gptel--system-message))))
   ;; If it's already a double-nested list (("part1" ...)), extract inner list
   ((and (consp gptel--system-message)
         (consp (car gptel--system-message))
         (stringp (car (car gptel--system-message))))
    (setq gptel--system-message (car gptel--system-message)))
   ;; If it's a single-level list of strings, use as-is
   ((and (consp gptel--system-message)
         (stringp (car gptel--system-message)))
    t)
   ;; Fallback: create new list with empty base message
   (t (setq gptel--system-message (list ""))))

  ;; Append skill as new list element with header
  (setq gptel--system-message
        (append gptel--system-message
                (list (format "## Skill: %s\n\n%s" skill-name content))))

  ;; Wrap in outer list for directive format (("part1" "part2" ...))
  (setq gptel--system-message (list gptel--system-message)))

(defun jf/gptel-skills--strip-mentions ()
  "Remove or hide @mentions from prompt buffer.
Uses invisible text property to hide mentions."
  (save-excursion
    (goto-char (point-min))
    (let ((prefix (regexp-quote jf/gptel-skills-mention-prefix)))
      (while (re-search-forward (concat prefix "\\([a-zA-Z0-9_-]+\\)") nil t)
        (let ((skill-name (match-string 1)))
          ;; Only strip if skill is in registry
          (when (gethash skill-name jf/gptel-skills--registry)
            ;; Make invisible
            (put-text-property (match-beginning 0) (match-end 0)
                              'invisible 'gptel-skill)))))))

(defun jf/gptel-skills-insert-mention (skill-name)
  "Insert @mention for SKILL-NAME at point.
Prompts for skill using completing-read."
  (interactive
   (list (completing-read "Insert skill: "
                         (hash-table-keys jf/gptel-skills--registry)
                         nil t)))
  (insert jf/gptel-skills-mention-prefix skill-name)
  ;; Trigger overlay update
  (jf/gptel-skills--update-overlays))

(defun jf/gptel-skills-list-active ()
  "Display currently active skills in minibuffer or buffer."
  (interactive)
  (if (null jf/gptel-skills--active)
      (message "No active skills in current buffer")
    (message "Active skills: %s" (string-join jf/gptel-skills--active ", "))))

(defun jf/gptel-skills-clear-mentions ()
  "Remove all @mentions from buffer."
  (interactive)
  (when (yes-or-no-p "Remove all @skill mentions from buffer? ")
    (save-excursion
      (goto-char (point-min))
      (let ((prefix (regexp-quote jf/gptel-skills-mention-prefix)))
        (while (re-search-forward (concat prefix "\\([a-zA-Z0-9_-]+\\)") nil t)
          (let ((skill-name (match-string 1)))
            (when (gethash skill-name jf/gptel-skills--registry)
              (delete-region (match-beginning 0) (match-end 0))
              ;; Clean up extra spaces
              (when (looking-at " +")
                (delete-region (point) (match-end 0))))))))
    (jf/gptel-skills--update-overlays)
    (message "Cleared all skill mentions")))

(defun jf/gptel-skills-reload ()
  "Reload all skills from directory and org-roam.
Clears cache, re-scans directory, and updates registry with both
markdown and org-roam skills."
  (interactive)

  ;; Clear registry
  (clrhash jf/gptel-skills--registry)

  ;; Discover markdown skills
  (let ((skill-files (jf/gptel-skills--discover))
        (md-count 0))
    (if (null skill-files)
        (message "No markdown skills found in %s" jf/gptel-skills-directory)

      ;; Parse and register each markdown skill
      (dolist (skill-file skill-files)
        (let ((metadata (jf/gptel-skills--parse-metadata skill-file)))
          (when metadata
            (let ((name (plist-get metadata :name)))
              (puthash name metadata jf/gptel-skills--registry)
              (setq md-count (1+ md-count)))))))

    ;; Try to load org-roam skills module if not already loaded
    (unless (featurep 'gptel-skills-roam)
      (let ((roam-skills-file (expand-file-name "gptel-skills-roam.el"
                                                 (file-name-directory (or load-file-name
                                                                         buffer-file-name)))))
        (when (file-exists-p roam-skills-file)
          (message "Loading org-roam skills from: %s" roam-skills-file)
          (load roam-skills-file nil t))))

    ;; Discover and register org-roam skills
    (message "Checking org-roam skills: featurep=%s enabled=%s"
             (featurep 'gptel-skills-roam)
             (if (boundp 'jf/gptel-skills-roam-enabled)
                 jf/gptel-skills-roam-enabled
               "unbound"))
    (when (and (featurep 'gptel-skills-roam)
               (boundp 'jf/gptel-skills-roam-enabled)
               jf/gptel-skills-roam-enabled)
      (let ((skill-files (jf/gptel-skills-roam--discover-files))
            (roam-count 0))
        (message "Skill files discovered: %s" skill-files)
        (dolist (file skill-files)
          (let ((metadata (jf/gptel-skills-roam--parse-file-metadata file)))
            (when metadata
              (let ((skill-name (plist-get metadata :name)))
                (message "Loaded org-roam skill: %s from %s" skill-name file)
                ;; Use skill name as key in unified registry
                (puthash skill-name metadata jf/gptel-skills--registry)
                (setq roam-count (1+ roam-count))))))
        (message "Loaded %d org-roam skill(s)" roam-count)))

    (message "Loaded %d skill(s) total (%d markdown, %d org-roam)"
             (hash-table-count jf/gptel-skills--registry)
             md-count
             (if (and (featurep 'gptel-skills-roam) jf/gptel-skills-roam-enabled)
                 (- (hash-table-count jf/gptel-skills--registry) md-count)
               0)))

  ;; Update overlays in all gptel buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p gptel-mode)  ; gptel-mode is a minor mode
        (jf/gptel-skills--update-overlays)))))

(defun jf/gptel-skills-describe (skill-name)
  "Show description and metadata for SKILL-NAME."
  (interactive
   (list (completing-read "Describe skill: "
                         (hash-table-keys jf/gptel-skills--registry)
                         nil t)))
  (let ((metadata (gethash skill-name jf/gptel-skills--registry)))
    (if (not metadata)
        (message "Skill not found: %s" skill-name)
      (with-output-to-temp-buffer (format "*Skill: %s*" skill-name)
        (princ (format "Skill: %s\n\n" skill-name))
        (princ (format "Description: %s\n" (plist-get metadata :description)))
        (princ (format "Path: %s\n" (plist-get metadata :path)))
        (princ (format "Loaded: %s\n" (if (plist-get metadata :loaded) "yes" "no")))
        (when (plist-get metadata :loaded)
          (princ (format "\nContent length: %d characters\n"
                        (length (plist-get metadata :content)))))))))

(defun jf/gptel-skills-setup ()
  "Initialize the skills system.
Called automatically when this module is loaded."
  (when (file-directory-p (expand-file-name jf/gptel-skills-directory))
    (jf/gptel-skills-reload)

    ;; Setup hooks for existing gptel buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p gptel-mode)  ; gptel-mode is a minor mode
          ;; Install buffer-local hooks
          (add-hook 'after-change-functions
                    #'jf/gptel-skills--update-overlays nil t)
          (add-hook 'completion-at-point-functions
                    #'jf/gptel-skills--completion-at-point nil t)
          ;; Initial overlay update
          (jf/gptel-skills--update-overlays))))))

;; Initialize skills and setup hooks when gptel is loaded
(with-eval-after-load 'gptel
  ;; Load skills
  (jf/gptel-skills-setup)

  ;; Add transform function to gptel
  (add-hook 'gptel-prompt-transform-functions
            #'jf/gptel-skills--transform-inject)

  ;; Add buffer-local hooks for gptel buffers
  (add-hook 'gptel-mode-hook
            (lambda ()
              ;; Update overlays on buffer changes
              (add-hook 'after-change-functions
                        #'jf/gptel-skills--update-overlays nil t)
              ;; Enable completion
              (add-hook 'completion-at-point-functions
                        #'jf/gptel-skills--completion-at-point nil t))))

;; If gptel is already loaded, initialize now
(when (featurep 'gptel)
  (jf/gptel-skills-setup))

;; Define keybindings in gptel-mode
(with-eval-after-load 'gptel
  (when (boundp 'gptel-mode-map)
    (define-key gptel-mode-map (kbd "C-c @ i") 'jf/gptel-skills-insert-mention)
    (define-key gptel-mode-map (kbd "C-c @ l") 'jf/gptel-skills-list-active)
    (define-key gptel-mode-map (kbd "C-c @ c") 'jf/gptel-skills-clear-mentions)
    (define-key gptel-mode-map (kbd "C-c @ d") 'jf/gptel-skills-describe)
    (define-key gptel-mode-map (kbd "C-c @ r") 'jf/gptel-skills-reload)))

(provide 'gptel-skills)
;;; gptel-skills.el ends here
