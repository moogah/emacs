;;; home-org-writer-lint-spec.el --- Cross-module writer-allow-list lint for home.org -*- lexical-binding: t; -*-

;; Structural enforcement of register/invariant/home-org-user-authored-
;; after-creation (load_bearing: true): the workspaces package writes
;; <HOME>/home.org exactly ONCE, in workspace--scaffold-write-home-org.
;; No other code path under config/workspaces/*.el may write to home.org.
;;
;; This lint walks every .el under config/workspaces/ (excluding test/),
;; scans for file-write primitives whose body within ~200 chars references
;; the literal "home.org", and compares hits against a fixed allow-list.
;; Any unexpected writer is a defect.
;;
;; Ported from openspec/changes/add-workspace-home-directory/scaffolding/
;; invariants/home-org-user-authored-after-creation.el (it #1). The
;; scaffold file's it #2 (reader-module-specific assertion) lives in
;; home-org-spec.el; it #3 (byte-for-byte snapshot) is forward-pinned.

(require 'buttercup)
(require 'cl-lib)

(defconst home-org-writer-lint--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(defconst home-org-writer-lint--source-dir
  (expand-file-name "../" home-org-writer-lint--this-dir)
  "Directory containing the workspaces module's tangled .el sources.")

(defconst home-org-writer-lint--permitted-writers
  '(("scaffold.el" . "workspace--scaffold-write-home-org"))
  "Permitted (basename . defun-name) pairs that may write home.org.
Anything else surfaced by the lint is a defect.")

(defconst home-org-writer-lint--write-fn-symbols
  '(write-region with-temp-file append-to-file
    delete-file rename-file copy-file
    write-file save-buffer)
  "Elisp primitives that mutate files.  The lint flags any of these whose
body within ~200 chars references the literal string \"home.org\".")

(defconst home-org-writer-lint--proximity-window 200
  "Characters of look-ahead after a write primitive in which a \"home.org\"
literal counts as belonging to that write call.")

(defun home-org-writer-lint--ws-elisp-files ()
  "Return all .el files under config/workspaces/ (excluding test/)."
  (when (file-directory-p home-org-writer-lint--source-dir)
    (cl-remove-if
     (lambda (path) (string-match-p "/test/" path))
     (directory-files-recursively
      home-org-writer-lint--source-dir "\\.el\\'"))))

(defun home-org-writer-lint--enclosing-defun-name ()
  "Return the name of the defun enclosing point, or nil.
Searches backwards for the nearest top-level `defun' / `cl-defun' /
`defmacro' / `cl-defmacro' form."
  (save-excursion
    (when (re-search-backward
           "^(\\(?:cl-\\)?\\(?:defun\\|defmacro\\)[[:space:]]+\\([^[:space:]()]+\\)"
           nil t)
      (match-string-no-properties 1))))

(defun home-org-writer-lint--scan-file (file)
  "Return a list of (FILE LINE FUNCTION WRITE-SYM) hits in FILE.
Each hit corresponds to a write primitive whose body within
`home-org-writer-lint--proximity-window' characters mentions
the literal string \"home.org\"."
  (let ((hits nil))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym home-org-writer-lint--write-fn-symbols)
        (goto-char (point-min))
        (let ((pattern (concat "(" (regexp-quote (symbol-name sym)) "\\_>")))
          (while (re-search-forward pattern nil t)
            (let* ((call-start (match-beginning 0))
                   (window-end (min (point-max)
                                    (+ call-start
                                       home-org-writer-lint--proximity-window)))
                   (window (buffer-substring-no-properties
                            call-start window-end)))
              (when (string-match-p "\"home\\.org\"" window)
                (let ((line (line-number-at-pos call-start))
                      (fname (home-org-writer-lint--enclosing-defun-name)))
                  (push (list file line fname sym) hits))))))))
    (nreverse hits)))

(defun home-org-writer-lint--permitted-p (hit)
  "Non-nil when HIT (FILE LINE FUNCTION WRITE-SYM) is on the allow-list."
  (let ((basename (file-name-nondirectory (nth 0 hit)))
        (fname (nth 2 hit)))
    (cl-some (lambda (pair)
               (and (equal (car pair) basename)
                    (equal (cdr pair) fname)))
             home-org-writer-lint--permitted-writers)))

(defun home-org-writer-lint--scan-for-unexpected-writers ()
  "Walk every .el under config/workspaces/ (excluding test/) and return
the list of hits NOT covered by `home-org-writer-lint--permitted-writers'.
Each element has shape (FILE LINE FUNCTION WRITE-SYM)."
  (let ((all-hits
         (cl-mapcan #'home-org-writer-lint--scan-file
                    (home-org-writer-lint--ws-elisp-files))))
    (cl-remove-if #'home-org-writer-lint--permitted-p all-hits)))

(defun home-org-writer-lint--describe-failure (unexpected)
  "Render UNEXPECTED hits as a single descriptive string, or \"\" if empty.
The non-empty form embeds the actionable hint verbatim so the buttercup
failure output (`Expected ... :to-equal \"\"') carries the remediation
guidance directly."
  (if (null unexpected)
      ""
    (format "Unexpected writers found: %S. Each must either be \
removed or added to home-org-writer-lint--permitted-writers (and the register \
entry register/invariant/home-org-user-authored-after-creation updated to \
reflect the new permitted writer)." unexpected)))

(describe "Invariant: home-org-user-authored-after-creation (cross-module lint)"

  (it "no module under config/workspaces/ writes to home.org except permitted scaffolder"
    (let* ((unexpected (home-org-writer-lint--scan-for-unexpected-writers))
           (report (home-org-writer-lint--describe-failure unexpected)))
      (expect report :to-equal ""))))

(provide 'home-org-writer-lint-spec)
;;; home-org-writer-lint-spec.el ends here
