;;; menu-buffer-local-spec.el --- Buttercup specs for chat-menu scope default -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the chat-menu scope default delivered by task
;; `default-chat-menu-scope-to-buffer-local' and corrected by task
;; `fix-chat-menu-scope-restore-on-real-exit' (see openspec/changes/
;; gptel-drawer-as-source-of-truth/tasks/, design.md §Decision 5,
;; register/boundary/chat-menu-scope-default).
;;
;; Coverage strategy: end-to-end against the real transient call path.
;; Each scenario for the *exit* contract drives `gptel-chat-menu's
;; prefix body (which sets `gptel--set-buffer-locally', registers the
;; restorer on `transient-post-exit-hook', and hands off to
;; `transient-setup') and then invokes `transient--post-exit' with a
;; sentinel command, mirroring what upstream does at the end of every
;; commit-style suffix.  This catches the production-bug shape that
;; the previous direct-call tests missed: by the time
;; `transient-exit-hook' fires on a real exit, `transient--export'
;; has already set `transient-current-prefix', so a guard of
;; `(unless transient-current-prefix ...)' would silently no-op.
;;
;; Coverage:
;;   - The prefix body sets `gptel--set-buffer-locally' to t before
;;     handing off to `transient-setup'.
;;   - The body registers `gptel-chat--restore-scope-on-exit' on
;;     `transient-post-exit-hook' (NOT `transient-exit-hook').
;;   - End-to-end: a full transient post-exit sequence restores the
;;     prior value of `gptel--set-buffer-locally' and self-removes
;;     the hook.
;;   - Restoration covers a non-nil prior (e.g. 'oneshot).
;;   - Upstream `gptel-menu' invoked directly is unaffected.
;;   - `gptel--set-with-scope' with the variable bound to t produces
;;     a buffer-local mutation in a chat-mode buffer (smoke test for
;;     the upstream contract this design relies on).

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'transient)

(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel)
(require 'gptel-transient)
(require 'gptel-chat-mode)
(require 'gptel-chat-menu)

(defun gptel-chat--spec-drive-post-exit ()
  "Drive the real `transient--post-exit' path with a sentinel command.

Mirrors what upstream does at the end of every commit-style
transient suffix: stage `transient--exitp' to a non-replace value,
clear the resume stack, and invoke `transient--post-exit' with a
non-nil command argument so the prefix is cleared and
`transient-post-exit-hook' is run.  Used by the end-to-end
restoration specs below."
  (let ((transient--exitp 'exit)
        (transient--stack nil))
    (transient--post-exit 'gptel-chat--spec-sentinel-suffix)))

(describe "gptel-chat-menu scope default (Decision 5)"

  (describe "prefix body — sets gptel--set-buffer-locally on entry"
    (it "sets gptel--set-buffer-locally to t before handing off to transient-setup"
      (let ((captured 'unset)
            (gptel--set-buffer-locally nil)
            (gptel-chat--scope-prior nil)
            (transient-post-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (&rest _)
                     (setq captured gptel--set-buffer-locally))))
          (call-interactively #'gptel-chat-menu))
        (expect captured :to-equal t)))

    (it "saves the prior value into gptel-chat--scope-prior"
      (let ((gptel--set-buffer-locally 'oneshot)
            (gptel-chat--scope-prior nil)
            (captured-prior 'unset)
            (transient-post-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (&rest _)
                     (setq captured-prior gptel-chat--scope-prior))))
          (call-interactively #'gptel-chat-menu))
        (expect captured-prior :to-equal 'oneshot)))

    (it "registers gptel-chat--restore-scope-on-exit on transient-post-exit-hook"
      (let ((gptel--set-buffer-locally nil)
            (gptel-chat--scope-prior nil)
            (transient-post-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (call-interactively #'gptel-chat-menu))
        (expect (memq #'gptel-chat--restore-scope-on-exit
                      transient-post-exit-hook)
                :to-be-truthy)))

    (it "does NOT register the restorer on the legacy transient-exit-hook"
      (let ((gptel--set-buffer-locally nil)
            (gptel-chat--scope-prior nil)
            (transient-exit-hook nil)
            (transient-post-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (call-interactively #'gptel-chat-menu))
        (expect (memq #'gptel-chat--restore-scope-on-exit
                      transient-exit-hook)
                :to-be nil))))

  (describe "end-to-end restoration via the real transient post-exit path"
    (it "restores gptel--set-buffer-locally and clears scope-prior on commit-style exit"
      (let ((gptel--set-buffer-locally nil)
            (gptel-chat--scope-prior nil)
            (transient-post-exit-hook nil)
            (transient-current-prefix nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (call-interactively #'gptel-chat-menu))
        ;; Sanity: the prefix body should have flipped the variable
        ;; and registered the hook.
        (expect gptel--set-buffer-locally :to-equal t)
        (expect (memq #'gptel-chat--restore-scope-on-exit
                      transient-post-exit-hook)
                :to-be-truthy)
        ;; Simulate the real call path: by the time
        ;; `transient--post-exit' starts running its hooks,
        ;; `transient--export' has already set
        ;; `transient-current-prefix' to a prefix object.  The fix
        ;; under test is that the restorer no longer guards on
        ;; `transient-current-prefix' (the previous implementation
        ;; would silently no-op here).
        (let ((transient-current-prefix 'gptel-chat-menu))
          (gptel-chat--spec-drive-post-exit))
        (expect gptel--set-buffer-locally :to-equal nil)
        (expect gptel-chat--scope-prior :to-equal nil)
        (expect (memq #'gptel-chat--restore-scope-on-exit
                      transient-post-exit-hook)
                :to-be nil)))

    (it "restores a non-nil prior (e.g. 'oneshot) through the real exit path"
      (let ((gptel--set-buffer-locally 'oneshot)
            (gptel-chat--scope-prior nil)
            (transient-post-exit-hook nil)
            (transient-current-prefix nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (call-interactively #'gptel-chat-menu))
        (expect gptel--set-buffer-locally :to-equal t)
        (expect gptel-chat--scope-prior :to-equal 'oneshot)
        (let ((transient-current-prefix 'gptel-chat-menu))
          (gptel-chat--spec-drive-post-exit))
        (expect gptel--set-buffer-locally :to-equal 'oneshot)
        (expect gptel-chat--scope-prior :to-equal nil)))

    (it "is one-shot — a second post-exit pass does not double-restore"
      (let ((gptel--set-buffer-locally nil)
            (gptel-chat--scope-prior nil)
            (transient-post-exit-hook nil)
            (transient-current-prefix nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (call-interactively #'gptel-chat-menu))
        (let ((transient-current-prefix 'gptel-chat-menu))
          (gptel-chat--spec-drive-post-exit))
        ;; After the first post-exit, the hook is gone; a second
        ;; post-exit must not re-run it (or perturb already-restored
        ;; state).  Mutate the variable to a sentinel and confirm
        ;; nothing changes.
        (setq gptel--set-buffer-locally 'sentinel)
        (gptel-chat--spec-drive-post-exit)
        (expect gptel--set-buffer-locally :to-equal 'sentinel))))

  (describe "upstream gptel-menu (M-x gptel-menu) is unaffected"
    (it "does not set gptel--set-buffer-locally to t"
      (let ((captured 'unset)
            (gptel--set-buffer-locally nil)
            (transient-post-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (&rest _)
                     (setq captured gptel--set-buffer-locally))))
          (call-interactively #'gptel-menu))
        (expect captured :to-equal nil)))

    (it "does not register the chat restorer on transient-post-exit-hook"
      (let ((gptel--set-buffer-locally nil)
            (transient-post-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (call-interactively #'gptel-menu))
        (expect (memq #'gptel-chat--restore-scope-on-exit
                      transient-post-exit-hook)
                :to-be nil))))

  (describe "buffer-local mutation contract (smoke test for the upstream call path)"
    (it "with gptel--set-buffer-locally = t, gptel--set-with-scope mutates buffer-locally only"
      (with-temp-buffer
        ;; Mimic chat-mode without the full mode setup: the contract we
        ;; rely on is purely the upstream helper's branching on the
        ;; scope argument.
        (let ((default-value-before (default-value 'gptel-tools))
              (test-list '(:tool-A :tool-B)))
          ;; gptel--set-with-scope dispatches on its third argument:
          ;; t (buffer-local), nil (global), or 'oneshot.
          (gptel--set-with-scope 'gptel-tools test-list t)
          (expect (local-variable-p 'gptel-tools) :to-be t)
          (expect (buffer-local-value 'gptel-tools (current-buffer))
                  :to-equal test-list)
          (expect (default-value 'gptel-tools)
                  :to-equal default-value-before))))))

(provide 'menu-buffer-local-spec)
;;; menu-buffer-local-spec.el ends here
