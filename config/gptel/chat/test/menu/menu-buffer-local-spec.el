;;; menu-buffer-local-spec.el --- Buttercup specs for chat-menu scope default -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the chat-menu scope default delivered by task
;; `default-chat-menu-scope-to-buffer-local' (see openspec/changes/
;; gptel-drawer-as-source-of-truth/tasks/open/default-chat-menu-scope-
;; to-buffer-local.md, design.md §Decision 5, register/boundary/chat-
;; menu-scope-default).
;;
;; Coverage:
;;   - The prefix body sets `gptel--set-buffer-locally' to t before
;;     handing off to `transient-setup' — proven by spying on the
;;     handoff and recording the variable's value at call time.
;;   - The body registers `gptel-chat--restore-scope-on-exit' on
;;     `transient-exit-hook' so the variable is restored on exit.
;;   - The restorer only restores when `transient-current-prefix' is
;;     nil (outermost-exit), to coexist with sub-transients fired
;;     from inside the chat menu.
;;   - The restorer is a one-shot (self-removes after restoring).
;;   - Upstream `gptel-menu' invoked directly is unaffected — its
;;     prefix body does not set the variable.
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

(describe "gptel-chat-menu scope default (Decision 5)"

  (describe "prefix body — sets gptel--set-buffer-locally on entry"
    (it "sets gptel--set-buffer-locally to t before handing off to transient-setup"
      (let ((captured 'unset)
            (gptel--set-buffer-locally nil)
            (gptel-chat--scope-prior nil)
            (transient-exit-hook nil)
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
            (transient-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (&rest _)
                     (setq captured-prior gptel-chat--scope-prior))))
          (call-interactively #'gptel-chat-menu))
        (expect captured-prior :to-equal 'oneshot)))

    (it "registers gptel-chat--restore-scope-on-exit on transient-exit-hook"
      (let ((gptel--set-buffer-locally nil)
            (gptel-chat--scope-prior nil)
            (transient-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (call-interactively #'gptel-chat-menu))
        (expect (memq #'gptel-chat--restore-scope-on-exit transient-exit-hook)
                :to-be-truthy))))

  (describe "gptel-chat--restore-scope-on-exit"
    (it "restores gptel--set-buffer-locally and clears scope-prior when no outer prefix"
      (let ((gptel--set-buffer-locally t)
            (gptel-chat--scope-prior nil)
            (transient-exit-hook (list #'gptel-chat--restore-scope-on-exit))
            (transient-current-prefix nil))
        (gptel-chat--restore-scope-on-exit)
        (expect gptel--set-buffer-locally :to-equal nil)
        (expect gptel-chat--scope-prior :to-equal nil)
        (expect (memq #'gptel-chat--restore-scope-on-exit transient-exit-hook)
                :to-be nil)))

    (it "does NOT restore when transient-current-prefix is non-nil (sub-transient exit)"
      (let ((gptel--set-buffer-locally t)
            (gptel-chat--scope-prior nil)
            (transient-exit-hook (list #'gptel-chat--restore-scope-on-exit))
            (transient-current-prefix 'gptel-chat-menu))
        (gptel-chat--restore-scope-on-exit)
        (expect gptel--set-buffer-locally :to-equal t)
        (expect (memq #'gptel-chat--restore-scope-on-exit transient-exit-hook)
                :to-be-truthy)))

    (it "restores a non-nil prior (e.g. oneshot)"
      (let ((gptel--set-buffer-locally t)
            (gptel-chat--scope-prior 'oneshot)
            (transient-exit-hook (list #'gptel-chat--restore-scope-on-exit))
            (transient-current-prefix nil))
        (gptel-chat--restore-scope-on-exit)
        (expect gptel--set-buffer-locally :to-equal 'oneshot)
        (expect gptel-chat--scope-prior :to-equal nil))))

  (describe "upstream gptel-menu (M-x gptel-menu) is unaffected"
    (it "does not set gptel--set-buffer-locally to t"
      (let ((captured 'unset)
            (gptel--set-buffer-locally nil)
            (transient-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (&rest _)
                     (setq captured gptel--set-buffer-locally))))
          (call-interactively #'gptel-menu))
        ;; Upstream's body does not touch the variable, so it stays at
        ;; whatever the caller's binding was — nil here.
        (expect captured :to-equal nil)))

    (it "does not register the chat restorer on transient-exit-hook"
      (let ((gptel--set-buffer-locally nil)
            (transient-exit-hook nil)
            (gptel-context nil))
        (cl-letf (((symbol-function 'transient-setup) #'ignore))
          (call-interactively #'gptel-menu))
        (expect (memq #'gptel-chat--restore-scope-on-exit transient-exit-hook)
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
