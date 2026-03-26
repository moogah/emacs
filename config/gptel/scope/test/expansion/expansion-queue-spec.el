;;; expansion-queue-spec.el --- RED: expansion queue for serializing parallel tool denials -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RED PHASE UNIT TESTS for the expansion queue mechanism.
;;
;; When gptel fires multiple async tools via mapc, more than one may trigger
;; the expansion UI. Since transient is single-instance, the second call to
;; transient-setup clobbers the first's callback. The queue serializes prompts:
;;
;;   prompt-expansion(tool-1) → no active expansion → show transient
;;   prompt-expansion(tool-2) → active expansion → push to queue
;;   user responds to tool-1 → invoke callback → pop queue → show tool-2
;;   user responds to tool-2 → invoke callback → queue empty → done
;;
;; These tests exercise the queue API directly:
;; - jf/gptel-scope-prompt-expansion (entry point, queue-aware)
;; - jf/gptel-scope--process-expansion-queue (called after each action)
;; - jf/gptel-scope--expansion-queue (buffer-local queue variable)
;; - jf/gptel-scope--expansion-active (buffer-local flag)

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Test Infrastructure

(defvar queue-test--transient-calls nil
  "List of (violation-info callback patterns tool-name) from each transient-setup call.")

(defun queue-test--make-violation (tool-name resource)
  "Build a minimal violation-info plist for TOOL-NAME and RESOURCE."
  (list :tool tool-name
        :resource resource
        :reason "test denial"
        :validation-type 'bash))

(defun queue-test--capture-transient-setup ()
  "Install a spy on transient-setup that captures calls without displaying UI.
Returns the captured calls list (same reference as queue-test--transient-calls)."
  (setq queue-test--transient-calls nil)
  ;; We spy on transient-setup itself to capture what prompt-expansion passes
  (spy-on 'transient-setup
          :and-call-fake
          (lambda (prefix &rest _args)
            ;; Only capture our expansion menu calls
            (when (eq prefix 'jf/gptel-scope-expansion-menu)
              ;; Extract scope from the keyword args
              ;; transient-setup is called as: (transient-setup 'menu nil nil :scope SCOPE)
              ;; The spy receives all args after prefix
              (let* ((kwargs _args)
                     (scope (plist-get (cddr kwargs) :scope)))
                (push scope queue-test--transient-calls)))))
  queue-test--transient-calls)


;;; Test Suites

;; ============================================================
;; SUITE 1: Queue variables exist and are buffer-local
;; ============================================================

(describe "Expansion queue: variables"

  (it "jf/gptel-scope--expansion-queue is defined"
    (expect (boundp 'jf/gptel-scope--expansion-queue) :to-be t))

  (it "jf/gptel-scope--expansion-active is defined"
    (expect (boundp 'jf/gptel-scope--expansion-active) :to-be t))

  (it "queue is buffer-local (concurrent sessions don't interfere)"
    (let ((buf-a (generate-new-buffer "*queue-test-a*"))
          (buf-b (generate-new-buffer "*queue-test-b*")))
      (unwind-protect
          (progn
            ;; Set queue in buffer A
            (with-current-buffer buf-a
              (setq-local jf/gptel-scope--expansion-queue '(item-a)))
            ;; Buffer B should have empty queue
            (with-current-buffer buf-b
              (expect jf/gptel-scope--expansion-queue :to-be nil))
            ;; Buffer A still has its item
            (with-current-buffer buf-a
              (expect jf/gptel-scope--expansion-queue :to-equal '(item-a))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))


;; ============================================================
;; SUITE 2: First call shows transient, second queues
;; ============================================================

(describe "Expansion queue: prompt-expansion queuing behavior"

  (before-each
    (when (boundp 'jf/gptel-scope--expansion-queue)
      (setq jf/gptel-scope--expansion-queue nil))
    (when (boundp 'jf/gptel-scope--expansion-active)
      (setq jf/gptel-scope--expansion-active nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--expansion-queue)
      (setq jf/gptel-scope--expansion-queue nil))
    (when (boundp 'jf/gptel-scope--expansion-active)
      (setq jf/gptel-scope--expansion-active nil)))

  (it "first call shows transient immediately (sets active flag)"
    (queue-test--capture-transient-setup)

    (jf/gptel-scope-prompt-expansion
     (queue-test--make-violation "run_bash_command" "which brew:/")
     (lambda (_) nil)
     '("which brew:/")
     "run_bash_command")

    ;; Transient was called
    (expect (length queue-test--transient-calls) :to-equal 1)
    ;; Active flag is set
    (expect jf/gptel-scope--expansion-active :to-be t))

  (it "second call queues instead of showing transient"
    (queue-test--capture-transient-setup)

    ;; First call — shows transient
    (jf/gptel-scope-prompt-expansion
     (queue-test--make-violation "run_bash_command" "which brew:/")
     (lambda (_) nil)
     '("which brew:/")
     "run_bash_command")

    ;; Second call — should queue, NOT call transient-setup again
    (jf/gptel-scope-prompt-expansion
     (queue-test--make-violation "run_bash_command" "ls /etc")
     (lambda (_) nil)
     '("ls /etc")
     "run_bash_command")

    ;; Transient was called only once
    (expect (length queue-test--transient-calls) :to-equal 1)
    ;; Queue has the second item
    (expect (length jf/gptel-scope--expansion-queue) :to-equal 1))

  (it "third call also queues (queue grows)"
    (queue-test--capture-transient-setup)

    ;; First — transient
    (jf/gptel-scope-prompt-expansion
     (queue-test--make-violation "run_bash_command" "tool-1")
     (lambda (_) nil) '("tool-1") "run_bash_command")

    ;; Second — queued
    (jf/gptel-scope-prompt-expansion
     (queue-test--make-violation "run_bash_command" "tool-2")
     (lambda (_) nil) '("tool-2") "run_bash_command")

    ;; Third — also queued
    (jf/gptel-scope-prompt-expansion
     (queue-test--make-violation "run_bash_command" "tool-3")
     (lambda (_) nil) '("tool-3") "run_bash_command")

    (expect (length queue-test--transient-calls) :to-equal 1)
    (expect (length jf/gptel-scope--expansion-queue) :to-equal 2)))


;; ============================================================
;; SUITE 3: Process queue after user action
;; ============================================================

(describe "Expansion queue: process-queue drains queue"

  (before-each
    (when (boundp 'jf/gptel-scope--expansion-queue)
      (setq jf/gptel-scope--expansion-queue nil))
    (when (boundp 'jf/gptel-scope--expansion-active)
      (setq jf/gptel-scope--expansion-active nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--expansion-queue)
      (setq jf/gptel-scope--expansion-queue nil))
    (when (boundp 'jf/gptel-scope--expansion-active)
      (setq jf/gptel-scope--expansion-active nil)))

  (it "process-expansion-queue is defined"
    (expect (fboundp 'jf/gptel-scope--process-expansion-queue) :to-be t))

  (it "pops next item and shows transient"
    (queue-test--capture-transient-setup)

    ;; Manually set up state: active with one queued item
    (setq jf/gptel-scope--expansion-active t)
    (setq jf/gptel-scope--expansion-queue
          (list (list :violation (queue-test--make-violation "run_bash_command" "tool-2")
                      :callback (lambda (_) nil)
                      :patterns '("tool-2")
                      :tool-name "run_bash_command")))

    ;; Process queue (called after user responds to first transient)
    (jf/gptel-scope--process-expansion-queue)

    ;; Transient was called for the queued item
    (expect (length queue-test--transient-calls) :to-equal 1)
    ;; Queue is now empty
    (expect jf/gptel-scope--expansion-queue :to-be nil)
    ;; Active flag still set (new transient is showing)
    (expect jf/gptel-scope--expansion-active :to-be t))

  (it "clears active flag when queue is empty"
    ;; Active with empty queue — user just responded to last prompt
    (setq jf/gptel-scope--expansion-active t)
    (setq jf/gptel-scope--expansion-queue nil)

    (jf/gptel-scope--process-expansion-queue)

    ;; Active flag cleared
    (expect jf/gptel-scope--expansion-active :to-be nil)))


;; ============================================================
;; SUITE 4: Full round-trip — two prompts, two callbacks
;; ============================================================

(describe "Expansion queue: full round-trip with two tools"

  (before-each
    (when (boundp 'jf/gptel-scope--expansion-queue)
      (setq jf/gptel-scope--expansion-queue nil))
    (when (boundp 'jf/gptel-scope--expansion-active)
      (setq jf/gptel-scope--expansion-active nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--expansion-queue)
      (setq jf/gptel-scope--expansion-queue nil))
    (when (boundp 'jf/gptel-scope--expansion-active)
      (setq jf/gptel-scope--expansion-active nil)))

  (it "both callbacks fire when user denies both sequentially"
    (let ((callback-1-result nil)
          (callback-2-result nil))

      ;; Capture transient calls so we can simulate user responses
      (queue-test--capture-transient-setup)

      ;; Fire both prompt-expansions (simulating gptel's mapc)
      (jf/gptel-scope-prompt-expansion
       (queue-test--make-violation "run_bash_command" "which brew:/")
       (lambda (json) (setq callback-1-result
                            (json-parse-string json :object-type 'plist)))
       '("which brew:/")
       "run_bash_command")

      (jf/gptel-scope-prompt-expansion
       (queue-test--make-violation "run_bash_command" "ls /etc")
       (lambda (json) (setq callback-2-result
                            (json-parse-string json :object-type 'plist)))
       '("ls /etc")
       "run_bash_command")

      ;; Only first transient shown
      (expect (length queue-test--transient-calls) :to-equal 1)

      ;; Simulate user denying tool #1:
      ;; Extract callback from the transient scope and invoke it
      (let* ((scope-1 (car queue-test--transient-calls))
             (cb-1 (plist-get scope-1 :callback)))
        (funcall cb-1
                 (json-serialize (list :success :false :user_denied t))))

      ;; After tool #1 action, process-expansion-queue should show tool #2
      (jf/gptel-scope--process-expansion-queue)

      ;; Tool #1 callback should have fired
      (expect callback-1-result :to-be-truthy)
      (expect (plist-get callback-1-result :user_denied) :to-be t)

      ;; Second transient should now be shown
      (expect (length queue-test--transient-calls) :to-equal 2)

      ;; Simulate user denying tool #2
      ;; (car because push adds to front — latest call is first)
      (let* ((scope-2 (car queue-test--transient-calls))
             (cb-2 (plist-get scope-2 :callback)))
        (funcall cb-2
                 (json-serialize (list :success :false :user_denied t))))

      (jf/gptel-scope--process-expansion-queue)

      ;; Tool #2 callback should have fired
      (expect callback-2-result :to-be-truthy)
      (expect (plist-get callback-2-result :user_denied) :to-be t)

      ;; Queue drained, active cleared
      (expect jf/gptel-scope--expansion-queue :to-be nil)
      (expect jf/gptel-scope--expansion-active :to-be nil))))


;; ============================================================
;; SUITE 5: Transient suffix actions call process-queue
;; ============================================================

(describe "Expansion queue: suffix actions trigger queue processing"

  ;; The deny, allow-once, and add-to-scope transient suffixes must call
  ;; process-expansion-queue after invoking their callback. Without this,
  ;; the queue is never drained even if the data structure is correct.

  (before-each
    (when (boundp 'jf/gptel-scope--expansion-queue)
      (setq jf/gptel-scope--expansion-queue nil))
    (when (boundp 'jf/gptel-scope--expansion-active)
      (setq jf/gptel-scope--expansion-active nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--expansion-queue)
      (setq jf/gptel-scope--expansion-queue nil))
    (when (boundp 'jf/gptel-scope--expansion-active)
      (setq jf/gptel-scope--expansion-active nil)))

  (it "deny-expansion calls process-expansion-queue"
    (spy-on 'jf/gptel-scope--process-expansion-queue)
    (spy-on 'transient-scope
            :and-return-value (list :callback (lambda (_) nil)
                                    :violation (queue-test--make-violation "test" "res")))
    (spy-on 'transient-quit-one)

    (jf/gptel-scope--deny-expansion)

    (expect 'jf/gptel-scope--process-expansion-queue :to-have-been-called))

  (it "allow-once-action calls process-expansion-queue"
    (spy-on 'jf/gptel-scope--process-expansion-queue)
    (spy-on 'transient-scope
            :and-return-value (list :callback (lambda (_) nil)
                                    :violation (queue-test--make-violation "test" "res")))
    (spy-on 'transient-quit-one)

    (jf/gptel-scope--allow-once-action)

    (expect 'jf/gptel-scope--process-expansion-queue :to-have-been-called)))

(provide 'expansion-queue-spec)

;;; expansion-queue-spec.el ends here
