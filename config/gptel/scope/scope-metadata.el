;; Dependencies


;; [[file:scope-metadata.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
;; Dependencies:1 ends here

;; File Metadata Collection

;; Gather contextual metadata for a filepath before validation. Returns a flat plist with:
;; - :path - Absolute expanded path
;; - :real-path - Symlink-resolved path
;; - :exists - Whether file/directory exists
;; - :git-tracked - Whether file is tracked by git (nil if git unavailable)
;; - :git-repo - Root of git repository (nil if not in repo)
;; - :type - File type (file, directory, or other)


;; [[file:scope-metadata.org::*File Metadata Collection][File Metadata Collection:1]]
(defun jf/gptel-scope--gather-file-metadata (filepath)
  "Gather contextual metadata for FILEPATH before validation.

Returns flat plist with keys:
  :path         - Absolute expanded path
  :real-path    - Symlink-resolved path
  :exists       - Whether file/directory exists
  :git-tracked  - Whether tracked by git (nil if git unavailable)
  :git-repo     - Root of git repository (nil if not in repo)
  :type         - File type (file, directory, or other)

Gracefully handles missing git by returning nil for git-related fields."
  (let* ((full-path (expand-file-name filepath))
         (real-path (condition-case nil
                        (file-truename full-path)
                      (error full-path)))
         (exists (file-exists-p full-path))
         (git-repo (locate-dominating-file full-path ".git"))
         (git-tracked (when git-repo
                        (jf/gptel-scope--file-is-git-tracked-p full-path)))
         (file-type (cond ((file-directory-p full-path) 'directory)
                         ((file-regular-p full-path) 'file)
                         (t 'other))))
    (list :path full-path
          :real-path real-path
          :exists exists
          :git-tracked git-tracked
          :git-repo git-repo
          :type file-type)))
;; File Metadata Collection:1 ends here

;; Git-Tracked File Check

;; Check if a file is tracked by git. Reused from scope-filesystem-tools.el.
;; Returns nil gracefully if git is not available or file is not in a git repository.


;; [[file:scope-metadata.org::*Git-Tracked File Check][Git-Tracked File Check:1]]
(defun jf/gptel-scope--file-is-git-tracked-p (file)
  "Check if FILE is tracked by git.
Returns t if file is tracked, nil if untracked or ignored.
Returns nil if file is not in a git repository or git is not available."
  (when-let* ((git-dir (locate-dominating-file file ".git")))
    (let ((default-directory git-dir)
          (relative-file (file-relative-name file git-dir)))
      (condition-case nil
          (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" relative-file))
        (error nil)))))
;; Git-Tracked File Check:1 ends here

;; Provide Feature


;; [[file:scope-metadata.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-metadata)
;;; scope-metadata.el ends here
;; Provide Feature:1 ends here
