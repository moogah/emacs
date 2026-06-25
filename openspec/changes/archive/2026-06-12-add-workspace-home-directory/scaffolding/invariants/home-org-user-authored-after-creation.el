;;; home-org-user-authored-after-creation.el --- invariant pin -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/home-org-user-authored-after-creation
;; generated-at: 2026-05-25T18:04:59Z
;; license: implementor-may-revise
;;
;; Invariant: the package writes <HOME>/home.org exactly ONCE per
;; workspace, during scaffolding. After that single write, no code
;; path in config/workspaces/*.el SHALL write to or delete home.org.
;; The package MAY read home.org at any time.
;;
;; Enforcement mechanism: lint (grep over tangled config/workspaces/
;; sources for write operations on a "home.org" path). The cycle-1
;; scaffold does NOT need the scaffold module to exist — the lint
;; runs against whatever .el files are present and asserts the
;; writer allow-list. The byte-for-byte test layer is added in
;; cycle 2+ once the scaffold and command modules are wired.

(require 'buttercup)
(require 'cl-lib)

(defconst home-org-user-authored--repo-root
  (let ((here (file-name-directory (or load-file-name buffer-file-name))))
    (expand-file-name "../../../../../" here))
  "Repo root resolved relative to this scaffold's location.")

(defconst home-org-user-authored--source-dir
  (expand-file-name "config/workspaces/" home-org-user-authored--repo-root)
  "Directory containing the workspaces module's tangled .el sources.")

(defconst home-org-user-authored--permitted-writers
  ;; Allow-list: file:function pairs that may legitimately write
  ;; home.org. Updated by integrate when a new permitted writer is
  ;; added (which should be rare — the scaffold's job is to make
  ;; new writers conspicuous).
  '(("scaffold.el" . "workspace--scaffold-write-home-org"))
  "Permitted writer call-sites of `home.org'. Anything else is a defect.")

(defconst home-org-user-authored--write-fn-symbols
  '(write-region with-temp-file append-to-file
    delete-file rename-file copy-file
    write-file save-buffer)
  "Elisp primitives that mutate files. The lint flags any of these \
when their target argument can resolve to a `home.org' path.")

(defun home-org-user-authored--ws-elisp-files ()
  "Return all .el files under config/workspaces/ (excluding test/)."
  (when (file-directory-p home-org-user-authored--source-dir)
    (cl-remove-if
     (lambda (path) (string-match-p "/test/" path))
     (directory-files-recursively
      home-org-user-authored--source-dir "\\.el\\'"))))

(describe "Invariant: home-org-user-authored-after-creation"

  (it "no module under config/workspaces/ has a hand-written reference to a home.org write call other than the permitted scaffolder"
    ;; Lint: grep for the literal "home.org" within a write-call form
    ;; in each .el under config/workspaces/. Report file:line of
    ;; any matches not in the permitted-writers allow-list.
    ;;
    ;; The cycle-1 Implementor of home-org-reader-module makes this
    ;; pass trivially (reader module has zero write calls). The
    ;; cycle-2 Implementor of scaffold-module makes it pass by
    ;; ensuring workspace--scaffold-write-home-org is the SINGLE
    ;; writer.
    (error "TODO: implement lint — \
walk (home-org-user-authored--ws-elisp-files), for each file load \
into a temp buffer, scan for any of \
(home-org-user-authored--write-fn-symbols) whose body within ~200 chars \
mentions \"home.org\". For each hit, check (basename . function) is in \
(home-org-user-authored--permitted-writers). Collect hits; expect the \
hit list (minus permitted) to be empty. Failure message must enumerate \
the unexpected writers with file:line and function name."))

  (it "home-org reader module (config/workspaces/home-org.el) has zero file-write calls"
    ;; Narrower, faster assertion targeted at cycle 1's home-org
    ;; reader module specifically. The reader module's job is reads;
    ;; any file-write primitive in its body is a defect regardless
    ;; of what file it targets.
    (error "TODO: implement assertion — \
(let ((file (expand-file-name \"home-org.el\" home-org-user-authored--source-dir))) \
  (when (file-readable-p file) \
    (with-temp-buffer \
      (insert-file-contents file) \
      (dolist (sym home-org-user-authored--write-fn-symbols) \
        (goto-char (point-min)) \
        (expect (re-search-forward (regexp-quote (symbol-name sym)) nil t) \
                :to-be nil))))). \
Note: this asserts AGAINST the reader module file existing. In cycle 1 \
the file will exist after the home-org-reader-module task lands; \
before that, the (file-readable-p) guard above passes trivially."))

  (it "byte-for-byte snapshot test scaffolding (forward-pinned to cycle 2+)"
    ;; Forward-pinned for cycle 2+ when scaffold-module lands. The
    ;; cycle-2 task wires the snapshot test: create a workspace,
    ;; capture the home.org sha256, run an operation (autosave,
    ;; switch, idle tick, kill-emacs simulation), recompute the
    ;; sha256, expect equality.
    (error "TODO: implement snapshot test in cycle 2+ — \
(let* ((home (make-temp-file \"ws-immut-\" t)) \
       (homeorg (expand-file-name \"home.org\" home))) \
  (workspace-scaffold home \"alpha\") \
  (let ((before (with-temp-buffer (insert-file-contents homeorg) \
                                   (secure-hash 'sha256 (buffer-string))))) \
    (workspace-switch \"alpha\") \
    (workspace-save) \
    (workspaces-mode--idle-tick) \
    (let ((after (with-temp-buffer (insert-file-contents homeorg) \
                                    (secure-hash 'sha256 (buffer-string))))) \
      (expect after :to-equal before)))). \
Marked NOT expected to pass in cycle 1; cycle-2 task scaffold-module \
makes it pass.")))

(provide 'home-org-user-authored-after-creation)
;;; home-org-user-authored-after-creation.el ends here
