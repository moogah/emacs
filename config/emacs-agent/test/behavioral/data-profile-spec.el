;;; data-profile-spec.el --- Data profiles applied before modules load -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D15, D21, D22
;; and § Seams: seam/data-profile, seam/note-roots.
;;
;; Each spec loads this checkout's whole configuration in a child
;; `emacs --batch' (early-init.el, then init.el, as bin/emacs-isolated.sh
;; and the test runner do) with EMACS_AGENT_DATA set, and reads values
;; back from it (`emacs-agent-test-child-init').  The child runs with HOME
;; at an empty directory of the scope, so `~/org' inside it is never the
;; user's vault: a path that fails to follow the profile lands under that
;; HOME, and the specs look there.  TMPDIR is the scope's private
;; directory (org-roam.org's `server-start' puts its socket there).
;;
;; A full batch init takes a few seconds.  Note: a child whose profile is
;; NOT applied keeps the worktree's runtime databases, and org-roam syncs
;; its database as it loads, against the empty HOME; while these specs
;; fail they can prune runtime/org-roam.db, which the next ordinary start
;; rebuilds (D22).  Pending until the profile is implemented.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defun data-profile-child-spec--vault ()
  "A scratch vault in the scope's private directory, created."
  (let ((vault (expand-file-name "vault/" emacs-agent-test--tmpdir)))
    (make-directory vault t)
    (file-name-as-directory (file-truename vault))))

(defun data-profile-child-spec--under-p (dir path)
  "Non-nil when PATH (a string) lies under DIR, comparing true names."
  (and (stringp path)
       (string-prefix-p (file-name-as-directory (file-truename dir))
                        (file-truename (expand-file-name path)))))

(defun data-profile-child-spec--child (profile form)
  "Run FORM in a full-configuration child under PROFILE; return its value.
Expect the child to exit 0."
  (let ((child (emacs-agent-test-child-init profile form)))
    (expect (cons (plist-get child :exit)
                  (and (not (eq (plist-get child :exit) 0))
                       (plist-get child :output)))
            :to-equal '(0))
    (plist-get child :value)))

(defun data-profile-child-spec--real-vault-untouched-p ()
  "Non-nil when nothing was created under the child's ~/org."
  (not (file-exists-p (expand-file-name "org" (emacs-agent-test-home)))))


(describe "Data profiles in a full-configuration Emacs (seam/data-profile, seam/note-roots)"

  (xit "points every module-load note path under the scratch vault and creates nothing under the real one (Scenario: isolated-emacs-targets § Module-load paths follow the profile)"
    (emacs-agent-test-with-socket-dir
      (let* ((vault (data-profile-child-spec--vault))
             (paths (data-profile-child-spec--child
                     (concat "scratch:" vault)
                     "(list jf/org-directory org-roam-directory org-graph-vault-root org-graph-roam-root vulpea-default-notes-directory gptel-org-utils-dailies-directory jf/capture-file)")))
        (expect (length paths) :to-equal 7)
        (dolist (path paths)
          (expect (cons path (data-profile-child-spec--under-p vault path))
                  :to-equal (cons path t)))
        (expect (data-profile-child-spec--real-vault-untouched-p) :to-be t))))

  (xit "keeps the note databases and workspace state out of the worktree's runtime under scratch (seam/data-profile)"
    (emacs-agent-test-with-socket-dir
      (let* ((vault (data-profile-child-spec--vault))
             (runtime (expand-file-name "runtime" emacs-agent-test-root))
             (values (data-profile-child-spec--child
                      (concat "scratch:" vault)
                      "(list jf/data-state-directory vulpea-db-location org-roam-db-location org-id-locations-file workspace-state-directory-override)"))
             (state (car values)))
        (expect (stringp state) :to-be t)
        (dolist (path (cdr values))
          (expect (cons path (and (data-profile-child-spec--under-p state path)
                                  (not (data-profile-child-spec--under-p runtime path))))
                  :to-equal (cons path t))))))

  (xit "writes an org_graph_write_node note under the scratch vault and nothing under the real one (Scenario: isolated-emacs-targets § Scratch profile confines note writes)"
    (emacs-agent-test-with-socket-dir
      (let* ((vault (data-profile-child-spec--vault))
             (result (data-profile-child-spec--child
                      (concat "scratch:" vault)
                      "(list (jf/agent-call \"call-write\" \"org_graph_write_node\" \"{\\\"title\\\":\\\"Scratch smoke\\\"}\") (directory-files-recursively org-graph-roam-root \"\\\\.org\\\\'\"))"))
             (reply (emacs-agent-test-envelope (car result)))
             (notes (cadr result)))
        (expect (plist-get reply :ok) :to-be t)
        (expect (length notes) :to-equal 1)
        (expect (data-profile-child-spec--under-p vault (car notes)) :to-be t)
        (expect (with-temp-buffer (insert-file-contents (car notes)) (buffer-string))
                :to-match "Scratch smoke")
        (expect (data-profile-child-spec--real-vault-untouched-p) :to-be t))))

  (xit "syncs only the scratch vault, without workspace homes (Scenario: isolated-emacs-targets § Scratch profile indexes only the scratch vault)"
    (emacs-agent-test-with-socket-dir
      (let* ((vault (data-profile-child-spec--vault))
             (ws-home (expand-file-name "ws-home/" emacs-agent-test--tmpdir))
             (values (progn
                       (make-directory ws-home t)
                       (data-profile-child-spec--child
                        (concat "scratch:" vault)
                        ;; A registered workspace whose home the roots must not include.
                        (format "(progn (puthash \"ws-probe\" (workspace--make \"ws-probe\" %S) workspace--registry) (list (org-graph/index-roots) org-graph-watch-workspace-homes))"
                                ws-home)))))
        (expect (mapcar (lambda (dir) (file-name-as-directory (file-truename dir)))
                        (car values))
                :to-equal (list vault))
        (expect (cadr values) :to-be nil))))

  (xit "starts no org-graph sync and watches no note directory under none (Scenario: isolated-emacs-targets § None profile disables graph sync)"
    (emacs-agent-test-with-socket-dir
      (let ((values (data-profile-child-spec--child
                     "none"
                     "(progn (org-graph--configure-sync-deferred) (list (bound-and-true-p vulpea-db-autosync-mode) (bound-and-true-p vulpea-db-sync-directories) jf/org-directory (directory-files-recursively jf/org-directory \"\\\\.org\\\\'\")))")))
        (expect (nth 0 values) :to-be nil)
        (expect (nth 1 values) :to-be nil)
        (expect (data-profile-child-spec--under-p (emacs-agent-test-home) (nth 2 values))
                :to-be nil)
        (expect (nth 3 values) :to-be nil)
        (expect (data-profile-child-spec--real-vault-untouched-p) :to-be t)))))

;;; data-profile-spec.el ends here
