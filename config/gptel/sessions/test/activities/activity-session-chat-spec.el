;;; activity-session-chat-spec.el --- Activity-backed sessions emit pre-populated session.org -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for `jf/gptel-session-create-persistent' — the
;; activity-backed session creation helper.  Per design Decision 4 /
;; Decision 6, activity-backed and standalone session creation paths
;; both emit a `session.org' pre-populated with a `:PROPERTIES:' drawer
;; (GPTEL_PRESET) followed by the chat-mode empty-user-block template,
;; and neither writes a `metadata.yml' sidecar.  There is no
;; mode-selection parameter: `gptel-chat-mode' is the single session
;; mode (Decision 16).
;;
;; Invariants verified:
;;
;; 1. Activity-backed session creation writes `session.org' with the
;;    pre-populated drawer + empty user block.  The `initial-content'
;;    parameter is no longer overridden by the activities helper (was
;;    `"###\\n"' previously).
;;
;; 2. No `metadata.yml' sidecar is written during activity-backed
;;    session creation.  The drawer is the authoritative session-level
;;    configuration source.
;;
;; 3. The session file path is `.../branches/main/session.org' — the
;;    activity helper creates on the `main' branch only; the directory
;;    layout (`branches/<branch>/', `current' symlink) is unchanged.
;;
;; 4. The resume path for activity-backed sessions goes through the same
;;    `jf/gptel--auto-init-session-buffer' hook as standalone sessions:
;;    opening `.../branches/main/session.org' activates `gptel-chat-mode'
;;    (never `gptel-mode') and sets the five buffer-local session
;;    variables.  See `auto-init-chat-mode-spec.el' for coverage of that
;;    hook across all session-file layouts; this spec asserts the
;;    contract for the specific directory layout that activity creation
;;    produces.
;;
;; 5. Worktree-tracking (`gptel-activity-worktrees' file-comment
;;    annotation in `session.org') is emitted only when the activity
;;    declares `PROJECT_WORKTREE' properties — that wiring is unchanged
;;    by this rework.
;;
;; Tests use `with-captured-io' from `persistence-test-helpers' so all
;; production persistence code runs for real against captured I/O;
;; upstream gptel boundaries are mocked at the `cl-letf' seam.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Locate shared test helpers living with the gptel behavioral tests.
(add-to-list 'load-path
             (expand-file-name
              "../../../test"
              (file-name-directory (or load-file-name buffer-file-name))))
(require 'persistence-test-helpers)

;; Load production modules.
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)

;; activities-integration.el `require's `gptel' at top-level with
;; optional semantics; pre-declare symbols it consults so loading the
;; module and its helper is robust under the test runner.
(defvar gptel-backend nil)
(defvar gptel-model nil)

;; Load the activities-integration module under test.  The file lives
;; alongside the other session modules.
(let ((session-dir (expand-file-name
                    "../.."
                    (file-name-directory (or load-file-name buffer-file-name)))))
  (load (expand-file-name "activities-integration.el" session-dir) nil t))

(defvar jf-gptel-activities-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-activities-test--register-cleanup (session-id branch-name)
  "Remember (SESSION-ID, BRANCH-NAME) for `after-each' registry cleanup."
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-activities-test--registry-keys))

(describe "jf/gptel-session-create-persistent emits pre-populated session.org"

  (after-each
    (dolist (key jf-gptel-activities-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-activities-test--registry-keys nil))

  (describe "session.org initial content (Decision 4)"

    (it "contains the PROPERTIES drawer with GPTEL_PRESET and the empty user block"
      ;; Run the full helper with write-side I/O captured.  The helper
      ;; goes through `jf/gptel--create-session-core' which owns the
      ;; initial-content default; the activities path no longer
      ;; overrides it.
      (cl-letf (((symbol-function 'gptel-backend-name)
                 (lambda (_backend) "test-backend"))
                ((symbol-function 'jf/gptel-activities--parse-worktree-paths)
                 (lambda (&rest _) nil))
                ((symbol-function 'activities-ext--slugify)
                 (lambda (name)
                   (replace-regexp-in-string
                    "[^a-z0-9-]" "-" (downcase name)))))
        (with-captured-io
          (let* ((jf/gptel-sessions-directory "/sessions/")
                 (info (jf/gptel-session-create-persistent
                        "implement-auth" nil nil 'executor nil))
                 (session-file (plist-get info :session-file))
                 (content (captured-file-content
                           captured-files session-file)))
            (jf-gptel-activities-test--register-cleanup
             (plist-get info :session-id) "main")
            (expect content :to-be-truthy)
            ;; First write contains the pre-populated drawer followed
            ;; by the chat-mode empty-user-block template (no worktree
            ;; paths in this scenario, so no follow-up
            ;; `<!-- gptel-activity-worktrees: ... -->' line is
            ;; appended).
            (expect content :to-equal
                    (concat ":PROPERTIES:\n"
                            ":GPTEL_PRESET: executor\n"
                            ":END:\n"
                            "#+begin_user\n"
                            "\n"
                            "#+end_user\n"))
            (expect content :not :to-match "^###")
            (expect content :not :to-match "^# "))))))

  (describe "no Local Variables block in session.org"

    (it "does not write a Local Variables block during creation"
      (cl-letf (((symbol-function 'gptel-backend-name)
                 (lambda (_backend) "test-backend"))
                ((symbol-function 'jf/gptel-activities--parse-worktree-paths)
                 (lambda (&rest _) nil))
                ((symbol-function 'activities-ext--slugify)
                 (lambda (name)
                   (replace-regexp-in-string
                    "[^a-z0-9-]" "-" (downcase name)))))
        (with-captured-io
          (let* ((jf/gptel-sessions-directory "/sessions/")
                 (info (jf/gptel-session-create-persistent
                        "no-locals" nil nil 'executor nil))
                 (session-file (plist-get info :session-file))
                 (content (captured-file-content
                           captured-files session-file)))
            (jf-gptel-activities-test--register-cleanup
             (plist-get info :session-id) "main")
            (expect content :to-be-truthy)
            (expect content :not :to-match "Local Variables:")
            (expect content :not :to-match "^# Local Variables:"))))))

  (describe "no metadata.yml sidecar (Decision 6)"

    (it "does not write a metadata.yml file next to session.org"
      ;; Post-`session-creation-drawer-prepopulate': the drawer
      ;; embedded in `session.org' is authoritative.  Activity-backed
      ;; creation must not emit the legacy `metadata.yml' sidecar.
      (cl-letf (((symbol-function 'gptel-backend-name)
                 (lambda (_backend) "test-backend"))
                ((symbol-function 'jf/gptel-activities--parse-worktree-paths)
                 (lambda (&rest _) nil))
                ((symbol-function 'activities-ext--slugify)
                 (lambda (name)
                   (replace-regexp-in-string
                    "[^a-z0-9-]" "-" (downcase name)))))
        (with-captured-io
          (let* ((jf/gptel-sessions-directory "/sessions/")
                 (info (jf/gptel-session-create-persistent
                        "no-metadata" nil nil 'researcher nil))
                 (paths (hash-table-keys captured-files)))
            (jf-gptel-activities-test--register-cleanup
             (plist-get info :session-id) "main")
            (expect (cl-some (lambda (p) (string-suffix-p "/metadata.yml" p))
                             paths)
                    :to-be nil))))))

  (describe "session.org drawer encodes the preset name"

    (it "writes the preset name verbatim as GPTEL_PRESET"
      (cl-letf (((symbol-function 'gptel-backend-name)
                 (lambda (_backend) "test-backend"))
                ((symbol-function 'jf/gptel-activities--parse-worktree-paths)
                 (lambda (&rest _) nil))
                ((symbol-function 'activities-ext--slugify)
                 (lambda (name)
                   (replace-regexp-in-string
                    "[^a-z0-9-]" "-" (downcase name)))))
        (with-captured-io
          (let* ((jf/gptel-sessions-directory "/sessions/")
                 (info (jf/gptel-session-create-persistent
                        "preset-coverage" nil nil 'researcher nil))
                 (session-file (plist-get info :session-file))
                 (content (captured-file-content
                           captured-files session-file)))
            (jf-gptel-activities-test--register-cleanup
             (plist-get info :session-id) "main")
            (expect content :to-be-truthy)
            (expect content :to-match "^:GPTEL_PRESET: researcher$"))))))

  (describe "directory layout is unchanged"

    (it "creates session.org on the main branch (branches/main/session.org)"
      ;; The activity helper uses a fixed `main' branch; the session
      ;; file path and returned plist advertise this unchanged layout.
      (cl-letf (((symbol-function 'gptel-backend-name)
                 (lambda (_backend) "test-backend"))
                ((symbol-function 'jf/gptel-activities--parse-worktree-paths)
                 (lambda (&rest _) nil))
                ((symbol-function 'activities-ext--slugify)
                 (lambda (name)
                   (replace-regexp-in-string
                    "[^a-z0-9-]" "-" (downcase name)))))
        (with-captured-io
          (let* ((jf/gptel-sessions-directory "/sessions/")
                 (info (jf/gptel-session-create-persistent
                        "layout-check" nil nil 'executor nil))
                 (session-file (plist-get info :session-file)))
            (jf-gptel-activities-test--register-cleanup
             (plist-get info :session-id) "main")
            (expect (plist-get info :branch-name) :to-equal "main")
            (expect session-file :to-match "/branches/main/session\\.org\\'")
            (expect (captured-file-content captured-files session-file)
                    :to-be-truthy))))))

  (describe "worktree tracking logic is unchanged"

    (it "does not append worktree metadata when no PROJECT_WORKTREE paths"
      ;; When the activity has no PROJECT_WORKTREE properties, the helper
      ;; must NOT append the `<!-- gptel-activity-worktrees: ... -->'
      ;; annotation — just the drawer + chat-mode template on its own.
      (cl-letf (((symbol-function 'gptel-backend-name)
                 (lambda (_backend) "test-backend"))
                ((symbol-function 'jf/gptel-activities--parse-worktree-paths)
                 (lambda (&rest _) nil))
                ((symbol-function 'activities-ext--slugify)
                 (lambda (name)
                   (replace-regexp-in-string
                    "[^a-z0-9-]" "-" (downcase name)))))
        (with-captured-io
          (let* ((jf/gptel-sessions-directory "/sessions/")
                 (info (jf/gptel-session-create-persistent
                        "no-worktrees" nil nil 'executor nil))
                 (session-file (plist-get info :session-file))
                 (content (captured-file-content
                           captured-files session-file)))
            (jf-gptel-activities-test--register-cleanup
             (plist-get info :session-id) "main")
            (expect content :not :to-match "gptel-activity-worktrees")))))

    (it "appends the worktree comment when PROJECT_WORKTREE paths are declared"
      ;; The worktree-tracking side-effect is orthogonal to the drawer
      ;; pre-population and MUST remain intact.  Seed the parser to
      ;; report one worktree path and assert the annotation lands in
      ;; the final session.org content.  The `when raw-paths' branch
      ;; in the activities helper reads+writes the session file via
      ;; `insert-file-contents' + `write-region'; `with-captured-io'
      ;; seeds the initial write into the hash, and we re-stub
      ;; `insert-file-contents' here so the follow-up read sees the
      ;; captured template.
      (let ((raw-paths '("/work/repo/**")))
        (cl-letf (((symbol-function 'gptel-backend-name)
                   (lambda (_backend) "test-backend"))
                  ((symbol-function 'jf/gptel-activities--parse-worktree-paths)
                   (lambda (&rest _) raw-paths))
                  ((symbol-function 'activities-ext--slugify)
                   (lambda (name)
                     (replace-regexp-in-string
                      "[^a-z0-9-]" "-" (downcase name)))))
          (with-captured-io
            ;; After `jf/gptel--create-session-core' writes the
            ;; pre-populated template, the activities helper re-reads
            ;; `session.org' to append the worktree annotation.  Serve
            ;; the captured content back when the helper re-opens the
            ;; file.
            (cl-letf (((symbol-function 'insert-file-contents)
                       (lambda (path &rest _)
                         (let ((seeded (captured-file-content
                                        captured-files path)))
                           (when seeded (insert seeded))
                           (list path (if seeded (length seeded) 0))))))
              (let* ((jf/gptel-sessions-directory "/sessions/")
                     (info (jf/gptel-session-create-persistent
                            "worktree-check" nil nil 'executor nil))
                     (session-file (plist-get info :session-file))
                     (content (captured-file-content
                               captured-files session-file)))
                (jf-gptel-activities-test--register-cleanup
                 (plist-get info :session-id) "main")
                (expect content :to-be-truthy)
                ;; The pre-populated drawer + chat-mode template is
                ;; still the leading content.
                (expect content :to-match
                        (concat "\\`:PROPERTIES:\n"
                                ":GPTEL_PRESET: executor\n"
                                ":END:\n"
                                "#\\+begin_user\n\n#\\+end_user\n"))
                ;; And the worktree annotation is appended — untouched
                ;; by this task's scope.
                (expect content :to-match
                        "<!-- gptel-activity-worktrees: .*repo.*-->")))))))))

(describe "activity-backed session resume activates chat-mode"

  ;; The "resume" path for an activity-backed session is the same
  ;; `find-file-hook' that fires on any `.../branches/main/session.org'
  ;; file: `jf/gptel--auto-init-session-buffer'.  Here we assert the
  ;; end-to-end contract for the specific directory layout that
  ;; activities produce — branch session on `main', session-id derived
  ;; from the parent directory — activates `gptel-chat-mode' and
  ;; populates the buffer-local session variables, without calling
  ;; `gptel-mode'.

  (after-each
    (dolist (key jf-gptel-activities-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-activities-test--registry-keys nil))

  (it "opens .../implement-auth-<ts>/branches/main/session.org in chat-mode"
    (let ((buf (generate-new-buffer "session.org"))
          (chat-mode-called nil)
          (gptel-mode-called nil))
      (unwind-protect
          (with-current-buffer buf
            (setq buffer-file-name
                  (expand-file-name
                   "~/.gptel/sessions/implement-auth-20260421120000/branches/main/session.org"))
            (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                      ((symbol-function 'file-exists-p) (lambda (_) t))
                      ((symbol-function 'insert-file-contents)
                       (lambda (f &rest _)
                         (insert "session_id: \"implement-auth-20260421120000\"\npreset: \"executor\"\n")
                         (list f 0)))
                      ((symbol-function 'gptel-get-preset)
                       (lambda (_) '((gptel-model . "test"))))
                      ((symbol-function 'gptel--apply-preset)
                       (lambda (_name _setter) nil))
                      ((symbol-function 'gptel-chat-mode)
                       (lambda (&optional _) (setq chat-mode-called t)))
                      ((symbol-function 'gptel-mode)
                       (lambda (&optional _) (setq gptel-mode-called t)))
                      ((symbol-function 'make-symbolic-link)
                       (lambda (_t _l &optional _ok) nil))
                      ((symbol-function 'delete-file)
                       (lambda (_f &optional _trash) nil)))
              (jf/gptel--auto-init-session-buffer)
              (jf-gptel-activities-test--register-cleanup
               "implement-auth-20260421120000" "main")

              ;; chat-mode activated; gptel-mode never called.
              (expect chat-mode-called :to-be t)
              (expect gptel-mode-called :to-be nil)

              ;; Five buffer-local session vars set identically to the
              ;; standalone flow.
              (expect jf/gptel--session-id
                      :to-equal "implement-auth-20260421120000")
              (expect jf/gptel--session-dir :to-be-truthy)
              (expect jf/gptel--branch-name :to-equal "main")
              (expect jf/gptel--branch-dir :to-be-truthy)
              (expect (bound-and-true-p jf/gptel--parent-session-id)
                      :to-be nil)

              ;; Registry entry created for the activity-backed branch.
              (let ((key (jf/gptel--registry-key
                          "implement-auth-20260421120000" "main")))
                (expect (gethash key jf/gptel--session-registry)
                        :to-be-truthy))))
        (kill-buffer buf)))))

(provide 'activity-session-chat-spec)
;;; activity-session-chat-spec.el ends here
