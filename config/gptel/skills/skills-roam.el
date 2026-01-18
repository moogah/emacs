;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'org-roam)

(defgroup gptel-skills-roam nil
  "Org-roam based skills for gptel."
  :group 'gptel-skills
  :prefix "jf/gptel-skills-roam-")

(defcustom jf/gptel-skills-roam-enabled t
  "Enable org-roam skills discovery and loading.
When nil, only markdown skills are used."
  :type 'boolean
  :group 'gptel-skills-roam)

(defcustom jf/gptel-skills-roam-directory
  (when (boundp 'org-roam-directory)
    (expand-file-name "skills" org-roam-directory))
  "Directory containing org-roam skill files.
Defaults to 'skills' subdirectory within org-roam-directory."
  :type 'directory
  :group 'gptel-skills-roam)

(defcustom jf/gptel-skills-roam-context-limit 40000
  "Maximum characters for loaded skill content.
Prevents context overflow when loading linked resources."
  :type 'integer
  :group 'gptel-skills-roam)

(defcustom jf/gptel-skills-roam-verbose nil
  "When non-nil, print verbose messages during operations."
  :type 'boolean
  :group 'gptel-skills-roam)

(defvar jf/gptel-skills-roam--registry (make-hash-table :test 'equal)
  "Hash table mapping skill file basenames to metadata plists.

Each entry is a plist with keys:
  :name             - Skill name (file basename without .org)
  :title            - Node title from #+TITLE (string)
  :description      - SKILL_DESCRIPTION property (string)
  :category         - Optional CATEGORY property (string, may be nil)
  :file             - Full file path (string)
  :source           - Always 'org-roam (symbol)
  :loaded           - Whether content loaded (boolean)
  :content          - Cached content (string or nil)")

(defun jf/gptel-skills-roam--discover-files ()
  "Find all .org files in skills directory.
Returns list of full file paths."
  (when (and jf/gptel-skills-roam-enabled
             jf/gptel-skills-roam-directory
             (file-directory-p jf/gptel-skills-roam-directory))
    (let ((files (directory-files jf/gptel-skills-roam-directory
                                  t                ; Return full paths
                                  "\\.org$"        ; Only .org files
                                  t)))             ; Sort by name
      (when jf/gptel-skills-roam-verbose
        (message "Org-roam discovery: found %d skill files in %s"
                 (length files)
                 jf/gptel-skills-roam-directory))
      files)))

(defun jf/gptel-skills-roam--parse-file-metadata (file)
  "Parse skill metadata from FILE.
Returns plist with :title, :description, :file, :name, and optionally :category."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      ;; Get properties from file
      (let* ((title (org-roam-get-keyword "TITLE"))
             (description (org-entry-get (point-min) "SKILL_DESCRIPTION" t))
             (category (org-entry-get (point-min) "CATEGORY" t))
             (skill-name (file-name-base file))
             (metadata (list :name skill-name
                            :title (or title skill-name)
                            :description (or description "")
                            :file file
                            :source 'org-roam
                            :loaded nil
                            :content nil)))
        ;; Add category only if present and not the org-mode default placeholder
        (when (and category (not (equal category "???")))
          (plist-put metadata :category category))
        metadata))))

(defun jf/gptel-skills-roam--load-content (skill-name &optional _granularity)
  "Load content for skill with SKILL-NAME.
Uses cached content if available, otherwise reads file and caches.
GRANULARITY parameter ignored for now (always loads full file)."
  (let ((metadata (gethash skill-name jf/gptel-skills-roam--registry)))
    (when metadata
      (if (plist-get metadata :loaded)
          ;; Return cached content
          (plist-get metadata :content)
        ;; Load and cache content
        (let* ((file (plist-get metadata :file))
               (content (when (file-exists-p file)
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
          (when content
            ;; Update metadata with content
            (plist-put metadata :content content)
            (plist-put metadata :loaded t)
            (puthash skill-name metadata jf/gptel-skills-roam--registry)
            content))))))

(defun jf/gptel-skills-roam--build-registry ()
  "Build registry of all org-roam skill files.
Populates `jf/gptel-skills-roam--registry' with metadata."
  (clrhash jf/gptel-skills-roam--registry)
  (let ((skill-files (jf/gptel-skills-roam--discover-files))
        (count 0))
    (dolist (file skill-files)
      (let ((metadata (jf/gptel-skills-roam--parse-file-metadata file)))
        (when metadata
          (let ((skill-name (plist-get metadata :name)))
            (puthash skill-name metadata jf/gptel-skills-roam--registry)
            (setq count (1+ count))))))
    (when jf/gptel-skills-roam-verbose
      (message "Built registry with %d org-roam skills" count))
    count))

(defun jf/gptel-skills-roam-setup ()
  "Initialize org-roam skills system.
Discovers and registers all skill files."
  (interactive)
  (when jf/gptel-skills-roam-enabled
    (jf/gptel-skills-roam--build-registry)
    (when jf/gptel-skills-roam-verbose
      (message "Org-roam skills system initialized"))))

(defun jf/gptel-skills-roam-create-skill (title description)
  "Create a new org-roam skill node.

TITLE is the skill name.
DESCRIPTION is the skill description for discovery/matching."
  (interactive
   (list
    (read-string "Skill title: ")
    (read-string "Skill description (use when...): ")))
  ;; Build the header template with proper variable substitution
  (let* ((header (format ":PROPERTIES:
:ID:       %%(org-id-new)
:SKILL_DESCRIPTION: %s
:END:
#+title: ${title}
#+filetags: :skill:

" description))
         (org-roam-capture-templates
          `(("s" "Skill" plain
             "* Overview\n\n%?\n\n* When to Use\n\n* Quick Reference\n\n"
             :target (file+head "skills/${slug}.org" ,header)
             :unnarrowed t))))
    (org-roam-capture- :node (org-roam-node-create :title title)
                       :templates org-roam-capture-templates
                       :keys "s")))

(defun jf/gptel-skills-roam-create-reference (title)
  "Create a new reference node linked to the current skill.
TITLE is the reference node title."
  (interactive "sReference title: ")
  (let ((org-roam-capture-templates
         `(("r" "Reference" plain
            "%?"
            :target (file+head "${slug}.org"
                               ":PROPERTIES:
:ID:       %(org-id-new)
:END:
#+title: ${title}
#+filetags: :skill:reference:

")
            :unnarrowed t))))
    (org-roam-capture- :node (org-roam-node-create :title title)
                       :templates org-roam-capture-templates
                       :keys "r")))

(defun jf/gptel-skills-roam--invalidate-cache (skill-name)
  "Invalidate cached content for SKILL-NAME.
Forces reload on next access."
  (let ((metadata (gethash skill-name jf/gptel-skills-roam--registry)))
    (when (and metadata (plist-get metadata :loaded))
      (plist-put metadata :loaded nil)
      (plist-put metadata :content nil)
      (puthash skill-name metadata jf/gptel-skills-roam--registry)
      (when jf/gptel-skills-roam-verbose
        (message "Invalidated cache for skill: %s" skill-name)))))

(defun jf/gptel-skills-roam--invalidate-all-caches ()
  "Invalidate all cached skill content.
Useful after modifying multiple skill files."
  (interactive)
  (maphash (lambda (skill-name _metadata)
             (jf/gptel-skills-roam--invalidate-cache skill-name))
           jf/gptel-skills-roam--registry)
  (when jf/gptel-skills-roam-verbose
    (message "Invalidated all skill caches")))

(provide 'gptel-skills-roam)
