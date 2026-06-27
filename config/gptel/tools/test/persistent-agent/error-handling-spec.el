;;; error-handling-spec.el --- Persistent-agent error / abort tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Buttercup specs for the persistent-agent terminal-state FSM handlers
;; (`--make-on-done', `--make-on-errs', `--make-on-abrt'). The contract
;; under test: every terminal state must delete the parent overlay
;; before invoking `main-cb', and `main-cb' must be invoked exactly
;; once. See openspec/changes/persistent-agent-rebuild/specs/
;; persistent-agent/spec.md § "Error handling".

;;; Code:

(require 'cl-lib)
(require 'buttercup)

;; The helper file uses `provide' but is named `helpers-spec.el', so
;; `require' cannot autoload it from `load-path'. Load it by absolute
;; path on first use (e.g., when this spec runs before
;; `helpers-spec.el' in alphabetical order); subsequent requires no-op
;; via `featurep'.
(unless (featurep 'jf-persistent-agent-test-helpers)
  (load (expand-file-name
         "helpers-spec.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil 'nomessage))
(require 'jf-persistent-agent-test-helpers)
(require 'gptel-persistent-agent)

(describe "persistent-agent terminal-state handlers"

  ;; Scenario: specs/persistent-agent/spec.md § "Error handling" →
  ;; "Network failure cleanup"
  (it "ERRS deletes the overlay and returns an error string"
    (let* ((calls nil)
           (main-cb (lambda (text) (push text calls)))
           (host-buf (generate-new-buffer "*test-overlay-host*"))
           (overlay (with-current-buffer host-buf (make-overlay 1 1)))
           (handler (jf/gptel-persistent-agent--make-on-errs main-cb))
           (fsm (gptel-make-fsm
                 :info (list :context overlay
                             :error '(some-error "boom")))))
      (unwind-protect
          (progn
            (funcall handler fsm)
            (expect (length calls) :to-equal 1)
            (expect (car calls) :to-match "^Error: agent request failed")
            (expect (car calls) :to-match "boom")
            (expect (overlay-buffer overlay) :to-be nil))
        (kill-buffer host-buf))))

  ;; Scenario: specs/persistent-agent/spec.md § "Error handling" →
  ;; "User abort cleanup"
  (it "ABRT deletes the overlay and returns an abort string"
    (let* ((calls nil)
           (main-cb (lambda (text) (push text calls)))
           (host-buf (generate-new-buffer "*test-overlay-host*"))
           (overlay (with-current-buffer host-buf (make-overlay 1 1)))
           (handler (jf/gptel-persistent-agent--make-on-abrt main-cb))
           (fsm (gptel-make-fsm :info (list :context overlay))))
      (unwind-protect
          (progn
            (funcall handler fsm)
            (expect (length calls) :to-equal 1)
            (expect (car calls) :to-match "abort")
            (expect (overlay-buffer overlay) :to-be nil))
        (kill-buffer host-buf))))

  ;; Scenario: specs/persistent-agent/spec.md § "Error handling" →
  ;; "Overlay never leaks" (DONE branch).
  ;; Even on the happy path the overlay must be cleaned up before
  ;; `main-cb' is invoked.
  (it "DONE handler also deletes the overlay"
    (let* ((calls nil)
           (main-cb (lambda (text) (push text calls)))
           (host-buf (generate-new-buffer "*test-overlay-host*"))
           (overlay (with-current-buffer host-buf (make-overlay 1 1)))
           (agent-buf (generate-new-buffer "*test-agent-buffer*"))
           (handler (jf/gptel-persistent-agent--make-on-done
                     main-cb agent-buf))
           (fsm (gptel-make-fsm :info (list :context overlay))))
      (unwind-protect
          (progn
            (funcall handler fsm)
            (expect (length calls) :to-equal 1)
            ;; Empty agent buffer ⇒ no assistant turn ⇒ empty string
            ;; (Decision 2 in design.md). The exact text isn't the
            ;; assertion target here; overlay cleanup is.
            (expect (stringp (car calls)) :to-be t)
            (expect (overlay-buffer overlay) :to-be nil))
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
        (kill-buffer host-buf))))

  ;; Scenario: specs/persistent-agent/spec.md § "Error handling" →
  ;; "Overlay never leaks" (loop variant). The spec says "any
  ;; terminal FSM state", so iterate over all three constructors.
  (it "the parent overlay is gone after every terminal state"
    (let* ((agent-buf (generate-new-buffer "*test-agent-buffer*"))
           (constructors
            (list
             (lambda (cb)
               (jf/gptel-persistent-agent--make-on-done cb agent-buf))
             #'jf/gptel-persistent-agent--make-on-errs
             #'jf/gptel-persistent-agent--make-on-abrt)))
      (unwind-protect
          (dolist (ctor constructors)
            (let* ((calls nil)
                   (main-cb (lambda (text) (push text calls)))
                   (host-buf (generate-new-buffer "*test-overlay-host*"))
                   (overlay (with-current-buffer host-buf
                              (make-overlay 1 1)))
                   (handler (funcall ctor main-cb))
                   (fsm (gptel-make-fsm
                         :info (list :context overlay
                                     :error '(some-error "boom")))))
              (unwind-protect
                  (progn
                    (funcall handler fsm)
                    (expect (overlay-buffer overlay) :to-be nil)
                    (expect (length calls) :to-equal 1))
                (kill-buffer host-buf))))
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf)))))

  ;; Defensive: handler must not blow up if context isn't an overlay.
  ;; Not from spec, but reflects the `(when (overlayp overlay) ...)'
  ;; guard in the implementation.
  (it "ERRS handler is robust to nil overlay"
    (let* ((calls nil)
           (main-cb (lambda (text) (push text calls)))
           (handler (jf/gptel-persistent-agent--make-on-errs main-cb))
           (fsm (gptel-make-fsm
                 :info (list :context nil
                             :error '(some-error "boom")))))
      (funcall handler fsm)
      (expect (length calls) :to-equal 1)
      (expect (car calls) :to-match "^Error: agent request failed")))

  ;; Combined assertion: each terminal state invokes `main-cb' exactly
  ;; once. Already covered by the per-state tests, but a single
  ;; combined check guards against double-firing if a future
  ;; maintainer adds a redundant call.
  (it "main-cb is invoked exactly once per terminal state"
    (let* ((agent-buf (generate-new-buffer "*test-agent-buffer*"))
           (constructors
            (list
             (lambda (cb)
               (jf/gptel-persistent-agent--make-on-done cb agent-buf))
             #'jf/gptel-persistent-agent--make-on-errs
             #'jf/gptel-persistent-agent--make-on-abrt)))
      (unwind-protect
          (dolist (ctor constructors)
            (let* ((calls nil)
                   (main-cb (lambda (text) (push text calls)))
                   (host-buf (generate-new-buffer "*test-overlay-host*"))
                   (overlay (with-current-buffer host-buf
                              (make-overlay 1 1)))
                   (handler (funcall ctor main-cb))
                   (fsm (gptel-make-fsm
                         :info (list :context overlay
                                     :error '(some-error "boom")))))
              (unwind-protect
                  (progn
                    (funcall handler fsm)
                    (expect (length calls) :to-equal 1))
                (kill-buffer host-buf))))
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf)))))

  ;; Defensive: the inline `buffer-live-p' guard in
  ;; `--extract-final-text' must short-circuit to "" if the agent
  ;; buffer was killed before DONE fires. Not a formal spec scenario.
  (it "DONE handler returns empty string when agent buffer is killed before terminal state"
    (let* ((calls nil)
           (main-cb (lambda (text) (push text calls)))
           (host-buf (generate-new-buffer "*test-overlay-host*"))
           (overlay (with-current-buffer host-buf (make-overlay 1 1)))
           (agent-buf (generate-new-buffer "*test-agent-buffer*"))
           (handler (jf/gptel-persistent-agent--make-on-done
                     main-cb agent-buf))
           (fsm (gptel-make-fsm :info (list :context overlay))))
      (unwind-protect
          (progn
            (kill-buffer agent-buf)
            (funcall handler fsm)
            (expect (length calls) :to-equal 1)
            (expect (car calls) :to-equal "")
            (expect (overlay-buffer overlay) :to-be nil))
        (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
        (kill-buffer host-buf)))))

;;; error-handling-spec.el ends here
