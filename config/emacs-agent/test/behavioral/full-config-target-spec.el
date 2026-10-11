;;; full-config-target-spec.el --- A worktree target with a scratch vault, end to end -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D3, D7, D15 and
;; § Seams: seam/target-lifecycle, seam/data-profile, seam/wrapper-cli.
;;
;; The guarded full-configuration smoke of D7: `bin/emacs-agent target
;; start --worktree <this checkout> --data scratch:<vault>' starts a daemon
;; that loads the whole configuration, and the specs drive it through the
;; wrapper.  It is opt-in: it runs only when EMACS_AGENT_FULL_SMOKE is set
;; (and the binaries exist), because each spec starts a full-configuration
;; daemon against this checkout's shared runtime/ and waits for its startup
;; sync, which takes tens of seconds.  The batch-child specs of
;; data-profile-spec.el cover the same paths in every run.
;;
;; The daemon runs with HOME at an empty directory of the scope and TMPDIR
;; at the scope's private socket directory, so it can reach neither the
;; user's vault nor the live socket; the scope stops it on exit.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defmacro full-config-spec--with-target (name vault &rest body)
  "Start worktree target NAME on a scratch VAULT, run BODY, stop it.
VAULT is bound to the vault's true name inside BODY."
  (declare (indent 2))
  `(progn
     (assume (getenv "EMACS_AGENT_FULL_SMOKE")
             "EMACS_AGENT_FULL_SMOKE is not set (opt-in full-configuration smoke)")
     (assume (emacs-agent-test-binaries-available-p) "no Emacs / emacsclient binary")
     (emacs-agent-test-with-socket-dir
       (emacs-agent-test-with-real-binaries
         (let* ((,vault (let ((dir (expand-file-name "vault/" emacs-agent-test--tmpdir)))
                          (make-directory dir t)
                          (file-name-as-directory (file-truename dir))))
                ;; Expanded before HOME is rebound: an abbreviated "~/"
                ;; directory would resolve under the fake HOME, where
                ;; `call-process' cannot enter it.
                (default-directory (expand-file-name default-directory))
                (process-environment
                 (cons (concat "HOME=" (directory-file-name (emacs-agent-test-home)))
                       (seq-remove (lambda (entry) (string-prefix-p "HOME=" entry))
                                   process-environment)))
                (started (emacs-agent-test-wrapper-run
                          "target" "start" ,name
                          "--worktree" (directory-file-name emacs-agent-test-root)
                          "--data" (concat "scratch:" ,vault))))
           (expect (cons (plist-get started :exit) (plist-get started :stderr))
                   :to-equal (cons 0 ""))
           (unwind-protect (progn ,@body)
             (emacs-agent-test-wrapper-run "target" "stop" ,name)))))))

(defun full-config-spec--eval (name form)
  "Evaluate FORM in target NAME through the wrapper; return the value read."
  (let ((result (emacs-agent-test-wrapper-run "target" name "eval" form)))
    (expect (cons form (plist-get result :exit)) :to-equal (cons form 0))
    (car (read-from-string (plist-get result :stdout)))))


(describe "A worktree target on a scratch vault (seam/target-lifecycle, seam/data-profile)"

  (it "starts a worktree target that loads the configuration and answers eval (Scenario: isolated-emacs-targets § Start a worktree-configured target)"
    (full-config-spec--with-target "smoke-full" vault
      (expect (full-config-spec--eval "smoke-full" "(list server-name (featurep 'org-graph) (featurep 'emacs-agent))")
              :to-equal '("smoke-full" t t))
      (expect (file-exists-p (emacs-agent-test-socket "smoke-full")) :to-be t)))

  (it "evaluates configuration forms in the worktree's loaded configuration (Scenario: emacs-control-tools § Eval runs in the loaded configuration)"
    (full-config-spec--with-target "smoke-eval" vault
      (expect (file-name-as-directory (full-config-spec--eval "smoke-eval" "jf/emacs-dir"))
              :to-equal emacs-agent-test-root)
      (expect (> (full-config-spec--eval "smoke-eval" "(length jf/enabled-modules)") 0)
              :to-be t)))

  (it "writes through call-write and finds the note through call, all in the scratch vault (seam/data-profile)"
    (full-config-spec--with-target "smoke-write" vault
      (let ((write (emacs-agent-test-wrapper-run
                    "target" "smoke-write" "call-write" "org_graph_write_node"
                    "{\"title\":\"Full smoke\"}")))
        (expect (plist-get write :exit) :to-equal 0))
      (let ((notes (directory-files-recursively vault "\\.org\\'")))
        (expect (length notes) :to-equal 1)
        (expect (with-temp-buffer (insert-file-contents (car notes)) (buffer-string))
                :to-match "Full smoke"))
      (expect (file-exists-p (expand-file-name "org" (emacs-agent-test-home))) :to-be nil)
      (expect (emacs-agent-test-wait-for
               (lambda ()
                 (let ((query (emacs-agent-test-wrapper-run
                               "target" "smoke-write" "call" "org_graph_query"
                               "{\"title_match\":\"Full smoke\"}")))
                   (string-match-p "Full smoke" (plist-get query :stdout))))
               30)
              :to-be-truthy)))

  (it "indexes only notes under the scratch vault once startup sync has run (Scenario: isolated-emacs-targets § Scratch profile indexes only the scratch vault)"
    (full-config-spec--with-target "smoke-index" vault
      (with-temp-file (expand-file-name "roam/seed.org" vault)
        (insert ":PROPERTIES:\n:ID: emacs-agent-smoke-seed\n:END:\n#+title: Seed\n"))
      (expect (emacs-agent-test-wait-for
               (lambda ()
                 (equal (full-config-spec--eval
                         "smoke-index"
                         "(mapcar #'vulpea-note-id (vulpea-db-query))")
                        '("emacs-agent-smoke-seed")))
               60)
              :to-be t)
      (expect (mapcar #'file-truename
                      (full-config-spec--eval "smoke-index" "vulpea-db-sync-directories"))
              :to-equal (list vault)))))

;;; full-config-target-spec.el ends here
