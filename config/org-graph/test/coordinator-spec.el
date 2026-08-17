;;; coordinator-spec.el --- Write-coordinator lock tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Sequential, in-process tests for `org-graph-coordinator/with-file-lock'.
;; Concurrency is simulated by inspecting/seeding the in-process lock
;; table rather than spawning real timers.  Covers the four behaviours
;; from the coordinator-lock task: BODY runs to completion under the
;; lock, a busy path blocks then times out, an error in BODY releases the
;; lock, and locks on distinct paths are independent.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (require 'org-graph-coordinator (expand-file-name "coordinator.el" module-dir)))

(describe "org-graph-coordinator/with-file-lock"

  (before-each
    ;; Start every spec with an empty lock table so cases never leak state.
    (clrhash org-graph-coordinator--locks))

  (it "runs BODY to completion and releases the lock afterward"
    (let ((path "/tmp/org-graph-coordinator-a.org")
          (ran nil))
      (org-graph-coordinator/with-file-lock path
        (setq ran t))
      (expect ran :to-be t)
      (expect (gethash (org-graph-coordinator--canonical path)
                       org-graph-coordinator--locks)
              :to-be nil)))

  (it "returns the value of BODY"
    (expect (org-graph-coordinator/with-file-lock "/tmp/org-graph-coordinator-a.org"
              (+ 1 2))
            :to-equal 3))

  (it "blocks on a busy path and times out with a structured error"
    (let* ((path "/tmp/org-graph-coordinator-b.org")
           (key (org-graph-coordinator--canonical path)))
      ;; Simulate a concurrent holder by seeding the lock table directly.
      (puthash key t org-graph-coordinator--locks)
      (let ((org-graph-coordinator-timeout 0.1))
        (expect (org-graph-coordinator/with-file-lock path
                  (error "should never run while held"))
                :to-throw 'org-graph-coordinator-lock-timeout))
      ;; The simulated holder's lock is untouched by the timed-out waiter.
      (expect (gethash key org-graph-coordinator--locks) :to-be t)))

  (it "releases the lock when BODY signals an error"
    (let* ((path "/tmp/org-graph-coordinator-c.org")
           (key (org-graph-coordinator--canonical path)))
      (expect (org-graph-coordinator/with-file-lock path
                (error "boom"))
              :to-throw)
      (expect (gethash key org-graph-coordinator--locks) :to-be nil)))

  (it "permits a write to a distinct path while another is held"
    (let* ((held "/tmp/org-graph-coordinator-d.org")
           (other "/tmp/org-graph-coordinator-e.org")
           (held-key (org-graph-coordinator--canonical held))
           (ran nil))
      ;; Hold one path; a different path must acquire immediately.
      (puthash held-key t org-graph-coordinator--locks)
      (org-graph-coordinator/with-file-lock other
        (setq ran t))
      (expect ran :to-be t)
      ;; The unrelated held lock is undisturbed.
      (expect (gethash held-key org-graph-coordinator--locks) :to-be t)))

  (it "keys the lock on the canonicalised path"
    ;; Two spellings of the same path collide on one lock key.
    (let ((key (org-graph-coordinator--canonical "/tmp/../tmp/org-graph-coordinator-f.org")))
      (expect key
              :to-equal (org-graph-coordinator--canonical
                         "/tmp/org-graph-coordinator-f.org")))))

;;; coordinator-spec.el ends here
