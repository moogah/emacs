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
                  :to-equal default-value-before)))))

  ;; -----------------------------------------------------------------------
  ;; Diff-on-exit: chat-menu marks the buffer modified when a tracked
  ;; buffer-local config var changed across the menu interaction.
  ;;
  ;; Restores the missing integration test specified in the closed task
  ;; brief `default-chat-menu-scope-to-buffer-local' (Implementation
  ;; steps §6, scenario 4 — "buffer-local tool change persists to drawer
  ;; on save").  Without this wiring, setq-local of gptel-tools etc.
  ;; from a menu suffix does not mark the buffer modified, so the
  ;; subsequent `save-buffer' is a no-op and the drawer never picks up
  ;; the change.
  ;;
  ;; The unit specs drive the same real transient post-exit path the
  ;; scope-restore specs above use; the integration spec wires
  ;; find-file-noselect → gptel-chat-mode → real save-buffer → re-read
  ;; the file from disk (pattern adapted from
  ;; config/gptel/chat/test/menu/system-prompt-file-spec.el:232-276).
  (describe "diff-on-exit marks buffer modified when tracked config changed"

    (it "does not mark the buffer modified when no tracked var changed"
      (with-temp-buffer
        (let ((gptel--set-buffer-locally nil)
              (gptel-chat--scope-prior nil)
              (gptel-chat--config-snapshot-prior nil)
              (transient-post-exit-hook nil)
              (transient-current-prefix nil)
              (gptel-context nil))
          (cl-letf (((symbol-function 'transient-setup) #'ignore))
            (call-interactively #'gptel-chat-menu))
          ;; Sanity: the snapshot was captured for this buffer.
          (expect (plist-get gptel-chat--config-snapshot-prior :buffer)
                  :to-equal (current-buffer))
          (set-buffer-modified-p nil)
          ;; No tracked var mutated between entry and exit.
          (let ((transient-current-prefix 'gptel-chat-menu))
            (gptel-chat--spec-drive-post-exit))
          (expect (buffer-modified-p) :to-be nil)
          ;; Hook is self-removed and snapshot slot is reusable.
          (expect (memq #'gptel-chat--maybe-mark-modified-on-exit
                        transient-post-exit-hook)
                  :to-be nil)
          (expect gptel-chat--config-snapshot-prior :to-be nil))))

    (it "marks the buffer modified when a tracked buffer-local changed"
      (with-temp-buffer
        (let ((gptel--set-buffer-locally nil)
              (gptel-chat--scope-prior nil)
              (gptel-chat--config-snapshot-prior nil)
              (transient-post-exit-hook nil)
              (transient-current-prefix nil)
              (gptel-context nil))
          ;; Establish a baseline buffer-local value so the snapshot
          ;; captures something concrete to diff against.  Bare
          ;; symbols match the tool-name convention used by
          ;; `gptel-tools' (keywords are interpreted as modify-list
          ;; specs by `--resolve-tool-names').
          (setq-local gptel-tools '(baseline-tool))
          (cl-letf (((symbol-function 'transient-setup) #'ignore))
            (call-interactively #'gptel-chat-menu))
          (set-buffer-modified-p nil)
          ;; Simulate the user toggling a tool inside the menu.
          (setq-local gptel-tools '(new-tool))
          (let ((transient-current-prefix 'gptel-chat-menu))
            (gptel-chat--spec-drive-post-exit))
          (expect (buffer-modified-p) :to-be t)
          (expect gptel-chat--config-snapshot-prior :to-be nil))))

    (it "targets the entry-time buffer even if current-buffer changes mid-menu"
      ;; Captures the design intent: the snapshot is keyed on the
      ;; chat-mode buffer where chat-menu was invoked, not whatever
      ;; buffer happens to be current when the post-exit hook fires.
      (let ((chat-buf (generate-new-buffer "chat-menu-spec-chat"))
            (other-buf (generate-new-buffer "chat-menu-spec-other")))
        (unwind-protect
            (let ((gptel--set-buffer-locally nil)
                  (gptel-chat--scope-prior nil)
                  (gptel-chat--config-snapshot-prior nil)
                  (transient-post-exit-hook nil)
                  (transient-current-prefix nil)
                  (gptel-context nil))
              (with-current-buffer chat-buf
                (setq-local gptel-tools '(baseline-tool))
                (cl-letf (((symbol-function 'transient-setup) #'ignore))
                  (call-interactively #'gptel-chat-menu))
                (set-buffer-modified-p nil)
                (setq-local gptel-tools '(new-tool)))
              ;; Switch buffers before the post-exit hook fires.  The
              ;; mark must still land on chat-buf, not other-buf.
              (with-current-buffer other-buf
                (set-buffer-modified-p nil)
                (let ((transient-current-prefix 'gptel-chat-menu))
                  (gptel-chat--spec-drive-post-exit)))
              (expect (buffer-modified-p chat-buf) :to-be t)
              (expect (buffer-modified-p other-buf) :to-be nil))
          (kill-buffer chat-buf)
          (kill-buffer other-buf)))))

  ;; -----------------------------------------------------------------------
  ;; Live drawer write on tracked-config commit: advice on the central
  ;; setter `gptel--set-with-scope' rewrites the drawer the moment a
  ;; menu suffix commits a buffer-local change to a tracked var.  For
  ;; the tool-selection sub-transient this fires once per confirmed
  ;; selection (the RET Confirm path is the only call into the setter;
  ;; per-tool letter-key toggles update transient-scope only and don't
  ;; hit the setter at all).  For one-shot infixes (model, temperature,
  ;; …) the setter is called directly on commit.
  (describe "live drawer write on tracked-config commit (advice on gptel--set-with-scope)"

    (it "rewrites the drawer immediately when gptel-tools is set buffer-locally in a chat-mode buffer"
      (with-temp-buffer
        (insert ":PROPERTIES:\n:END:\n#+begin_user\n\n#+end_user\n")
        (goto-char (point-min))
        (gptel-chat-mode)
        ;; Pre-condition: drawer carries no :GPTEL_TOOLS: line.
        (expect (org-entry-get (point-min) "GPTEL_TOOLS") :to-be nil)
        ;; Mimic the tool-selector RET Confirm path: upstream calls
        ;; `gptel--set-with-scope' once with the final tool list and
        ;; scope=t (buffer-local).  Bare-symbol tool names match the
        ;; project convention; `jf/gptel-scope-profile--resolve-tool-names'
        ;; calls `symbol-name' on each.
        (gptel--set-with-scope 'gptel-tools '(live-write-tool) t)
        ;; Buffer-local was set (upstream's contract) AND the drawer
        ;; text immediately reflects the new value (our advice).
        (expect (local-variable-p 'gptel-tools) :to-be t)
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_TOOLS")
                :to-equal '("live-write-tool"))
        ;; The drawer-text edit marks the buffer modified — no separate
        ;; set-buffer-modified-p call required.
        (expect (buffer-modified-p) :to-be t)))

    (it "is a no-op in buffers that are not in gptel-chat-mode"
      (with-temp-buffer
        (insert ":PROPERTIES:\n:END:\n")
        (goto-char (point-min))
        (spy-on 'gptel-chat--write-config-drawer :and-call-through)
        (gptel--set-with-scope 'gptel-tools '(some-tool) t)
        (expect 'gptel-chat--write-config-drawer :not :to-have-been-called)
        ;; The drawer is untouched.
        (expect (org-entry-get (point-min) "GPTEL_TOOLS") :to-be nil)))

    (it "is a no-op when the scope argument is nil (global write)"
      (with-temp-buffer
        (insert ":PROPERTIES:\n:END:\n#+begin_user\n\n#+end_user\n")
        (goto-char (point-min))
        (gptel-chat-mode)
        (spy-on 'gptel-chat--write-config-drawer :and-call-through)
        ;; Snapshot the pre-existing default so this test can restore
        ;; it — upstream's setter for scope=nil sets the global value.
        (let ((default-before (default-value 'gptel-tools)))
          (unwind-protect
              (progn
                (gptel--set-with-scope 'gptel-tools '(unwanted-tool) nil)
                (expect 'gptel-chat--write-config-drawer
                        :not :to-have-been-called)
                (expect (org-entry-get (point-min) "GPTEL_TOOLS")
                        :to-be nil))
            (setq-default gptel-tools default-before)))))

    (it "is a no-op when the scope argument is the oneshot sentinel"
      (with-temp-buffer
        (insert ":PROPERTIES:\n:END:\n#+begin_user\n\n#+end_user\n")
        (goto-char (point-min))
        (gptel-chat-mode)
        (spy-on 'gptel-chat--write-config-drawer :and-call-through)
        ;; Upstream's oneshot scope = the literal integer 1 — see
        ;; `gptel--set-with-scope's docstring.
        (let ((default-before (default-value 'gptel-tools)))
          (unwind-protect
              (progn
                (gptel--set-with-scope 'gptel-tools '(oneshot-tool) 1)
                (expect 'gptel-chat--write-config-drawer
                        :not :to-have-been-called)
                (expect (org-entry-get (point-min) "GPTEL_TOOLS")
                        :to-be nil))
            ;; gptel-post-request-hook is the oneshot restore path; clear
            ;; it so this test's restore lambda doesn't leak.
            (setq gptel-post-request-hook nil)
            (setq-default gptel-tools default-before)))))

    (it "is a no-op when the variable being set is not in gptel-chat--tracked-config-vars"
      (with-temp-buffer
        (insert ":PROPERTIES:\n:END:\n#+begin_user\n\n#+end_user\n")
        (goto-char (point-min))
        (gptel-chat-mode)
        (spy-on 'gptel-chat--write-config-drawer :and-call-through)
        ;; gptel-confirm-tool-calls is a real gptel variable, but it's
        ;; not a drawer-emitted var, so the advice should not fire.
        (gptel--set-with-scope 'gptel-confirm-tool-calls t t)
        (expect 'gptel-chat--write-config-drawer :not :to-have-been-called))))

  (describe "integration: chat-menu toggle then save-buffer writes drawer"

    :var (session-dir session-file)

    (before-each
      (setq session-dir (make-temp-file "chat-menu-buffer-modified-" t)
            session-file (expand-file-name "session.org" session-dir))
      (with-temp-file session-file
        (insert ":PROPERTIES:\n:END:\n"
                "#+begin_user\n\n#+end_user\n")))

    (after-each
      (when (and session-dir (file-directory-p session-dir))
        (delete-directory session-dir t)))

    (it "save-buffer writes :GPTEL_TOOLS: to disk after a menu-driven toggle"
      ;; Restores the integration scenario from the closed task brief:
      ;; "set up a chat-mode buffer with a registered preset, simulate a
      ;; tool toggle that goes buffer-local, run `save-buffer', assert
      ;; the saved drawer text contains a `:GPTEL_TOOLS:' line listing
      ;; the new tool set."  Uses the real save-buffer entry rather than
      ;; calling `gptel-chat--save-state' directly so the modified-flag
      ;; gate (the actual bug) is exercised.
      (spy-on 'save-buffer :and-call-through)
      (let ((buf (find-file-noselect session-file)))
        (unwind-protect
            (with-current-buffer buf
              (gptel-chat-mode)
              ;; Bare symbols match the tool-name convention used
              ;; throughout the snapshot writer's tests.
              (setq-local gptel-tools '(baseline-tool))
              (set-buffer-modified-p nil)
              (let ((gptel--set-buffer-locally nil)
                    (gptel-chat--scope-prior nil)
                    (gptel-chat--config-snapshot-prior nil)
                    (transient-post-exit-hook nil)
                    (transient-current-prefix nil)
                    (gptel-context nil))
                (cl-letf (((symbol-function 'transient-setup) #'ignore))
                  (call-interactively #'gptel-chat-menu))
                ;; Simulate the user toggling a tool from inside the
                ;; menu.  Real menu suffixes route through
                ;; `gptel--set-with-scope', but the contract we are
                ;; verifying is "any buffer-local mutation of a tracked
                ;; var across the menu lifetime marks the buffer
                ;; modified"; a direct setq-local is the minimal
                ;; trigger for that contract.
                (setq-local gptel-tools '(new-tool))
                (let ((transient-current-prefix 'gptel-chat-menu))
                  (gptel-chat--spec-drive-post-exit)))
              ;; The diff-on-exit hook marked us modified, so the real
              ;; save-buffer now fires `before-save-hook' (and thus
              ;; `gptel-chat--save-state', which writes the drawer).
              (expect (buffer-modified-p) :to-be t)
              (save-buffer)
              (expect 'save-buffer :to-have-been-called)
              ;; Re-read the drawer from disk to prove the property
              ;; landed in the file, not just the in-memory buffer.
              (let ((on-disk
                     (with-temp-buffer
                       (insert-file-contents session-file)
                       (when (re-search-forward
                              "^:GPTEL_TOOLS: \\(.*\\)$" nil t)
                         (match-string 1)))))
                (expect on-disk :to-equal "new-tool")))
          (kill-buffer buf))))))

(provide 'menu-buffer-local-spec)
;;; menu-buffer-local-spec.el ends here
