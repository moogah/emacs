;;; workspace-integration-spec.el --- org-graph workspace integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural tests for the org-graph workspace integration
;; (workspace-integration task / RE-2 / RE-5).  org-graph is a CONSUMER
;; of two published seams: the workspaces integration registry and the
;; `workspace-assistant' preset `:tools' slot.  Neither workspaces nor
;; gptel is loaded in the test process, so these specs exercise the
;; consumer side directly:
;;
;; - The registration body (`org-graph-workspace-integration-register')
;;   is called with `workspace-register-integration' stubbed via
;;   `cl-letf', capturing the registered plist to assert its shape.
;; - The `:on-create' handler is fed a fake anchor payload with
;;   `vulpea-db-sync-update-directory' stubbed; assertions cover the
;;   watch-add (append + watcher install + `ok'), the option-off skip,
;;   and the no-home skip — all PUSH-only (the handler reads the payload,
;;   never global state).
;; - The `:menu' handler is invoked with `org-graph/configure-sync'
;;   stubbed.
;; - The tools-slot population is driven against a fake
;;   `gptel--known-presets' with `org-graph/agent-tools' stubbed,
;;   covering the populate path, the empty-list tolerance, and the
;;   absent-preset tolerance.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'org-graph-workspace-integration
           (expand-file-name "workspace-integration.el" module-dir)))

;; Owned by the loader defcustom, which is not loaded standalone; declare
;; special so the on-create handler's `boundp' guard sees it when bound by
;; a spec and falls through to `skipped' otherwise.
(defvar org-graph-watch-workspace-homes)
;; Soft-dependency specials the handlers/populator reference.
(defvar vulpea-db-sync-directories)
(defvar gptel--known-presets)

(describe "org-graph workspace integration"

  (describe "registration"

    (it "registers the org-graph integration with :label, :on-create, :menu"
      (let (captured-id captured-plist)
        (cl-letf (((symbol-function 'workspace-register-integration)
                   (lambda (id &rest plist)
                     (setq captured-id id captured-plist plist)
                     id)))
          (org-graph-workspace-integration-register))
        (expect captured-id :to-be 'org-graph)
        (expect (plist-get captured-plist :label) :to-equal "org-graph")
        (expect (plist-get captured-plist :on-create)
                :to-be #'org-graph-workspace-integration--on-create)))

    (it "registers a :menu (KEY . COMMAND) cons with a string key"
      (let (captured-plist)
        (cl-letf (((symbol-function 'workspace-register-integration)
                   (lambda (_id &rest plist) (setq captured-plist plist) _id)))
          (org-graph-workspace-integration-register))
        (let ((menu (plist-get captured-plist :menu)))
          (expect (consp menu) :to-be-truthy)
          (expect (car menu) :to-equal "G")
          (expect (cdr menu) :to-be #'org-graph-workspace-integration--menu)))))

  (describe ":on-create handler"

    (it "appends the payload :home to vulpea-db-sync-directories and watches it"
      (let ((org-graph-watch-workspace-homes t)
            (vulpea-db-sync-directories (list "/existing/root/"))
            (watched nil)
            (home (file-name-as-directory (expand-file-name "/tmp/ws-home"))))
        (cl-letf (((symbol-function 'vulpea-db-sync-update-directory)
                   (lambda (dir) (setq watched dir))))
          (let ((outcome (org-graph-workspace-integration--on-create
                          (list :name "ws" :home "/tmp/ws-home"
                                :sessions-dir "/tmp/ws-home/sessions/"
                                :context 'fresh))))
            (expect outcome :to-be 'ok)
            (expect (member home vulpea-db-sync-directories) :to-be-truthy)
            ;; appended (roam-vault-style priority preserved), not prepended
            (expect (car vulpea-db-sync-directories) :to-equal "/existing/root/")
            (expect watched :to-equal home)))))

    (it "is idempotent: re-running does not duplicate the home"
      (let ((org-graph-watch-workspace-homes t)
            (vulpea-db-sync-directories nil)
            (home (file-name-as-directory (expand-file-name "/tmp/ws-home"))))
        (cl-letf (((symbol-function 'vulpea-db-sync-update-directory) #'ignore))
          (org-graph-workspace-integration--on-create '(:home "/tmp/ws-home"))
          (org-graph-workspace-integration--on-create '(:home "/tmp/ws-home")))
        (expect (cl-count home vulpea-db-sync-directories :test #'equal)
                :to-equal 1)))

    (it "skips (no mutation) when org-graph-watch-workspace-homes is nil"
      (let ((org-graph-watch-workspace-homes nil)
            (vulpea-db-sync-directories (list "/existing/root/"))
            (watched nil))
        (cl-letf (((symbol-function 'vulpea-db-sync-update-directory)
                   (lambda (dir) (setq watched dir))))
          (let ((outcome (org-graph-workspace-integration--on-create
                          '(:home "/tmp/ws-home"))))
            (expect outcome :to-be 'skipped)
            (expect vulpea-db-sync-directories :to-equal (list "/existing/root/"))
            (expect watched :to-be nil)))))

    (it "skips when the payload carries no :home"
      (let ((org-graph-watch-workspace-homes t)
            (vulpea-db-sync-directories (list "/existing/root/"))
            (watched nil))
        (cl-letf (((symbol-function 'vulpea-db-sync-update-directory)
                   (lambda (dir) (setq watched dir))))
          (let ((outcome (org-graph-workspace-integration--on-create
                          '(:name "ws" :context fresh))))
            (expect outcome :to-be 'skipped)
            (expect vulpea-db-sync-directories :to-equal (list "/existing/root/"))
            (expect watched :to-be nil))))))

  (describe ":menu handler"

    (it "re-indexes via org-graph/configure-sync and returns ok"
      (let ((called nil))
        (cl-letf (((symbol-function 'org-graph/configure-sync)
                   (lambda () (setq called t))))
          (let ((outcome (org-graph-workspace-integration--menu
                          '(:name "ws" :home "/tmp/ws-home" :context menu-invoke))))
            (expect called :to-be-truthy)
            (expect outcome :to-be 'ok))))))

  (describe "workspace-assistant :tools population"

    (it "sets the preset :tools slot to org-graph/agent-tools"
      (let* ((fake-tools '(tool-a tool-b))
             (gptel--known-presets
              (list (cons 'workspace-assistant
                          (list :description "wa" :model 'm)))))
        (cl-letf (((symbol-function 'org-graph/agent-tools)
                   (lambda () fake-tools)))
          (let ((outcome (org-graph-workspace-integration--populate-assistant-tools)))
            (expect outcome :to-be 'ok)
            (let ((plist (cdr (assq 'workspace-assistant gptel--known-presets))))
              (expect (plist-get plist :tools) :to-equal fake-tools)
              ;; other slots preserved (additive)
              (expect (plist-get plist :description) :to-equal "wa")
              (expect (plist-get plist :model) :to-be 'm))))))

    (it "tolerates an empty tool list (gptel not loaded): skipped, no mutation"
      (let ((gptel--known-presets
             (list (cons 'workspace-assistant (list :description "wa")))))
        (cl-letf (((symbol-function 'org-graph/agent-tools) (lambda () nil)))
          (let ((outcome (org-graph-workspace-integration--populate-assistant-tools)))
            (expect outcome :to-be 'skipped)
            (expect (plist-member (cdr (assq 'workspace-assistant gptel--known-presets))
                                  :tools)
                    :to-be nil)))))

    (it "skips silently when the preset is absent"
      (let ((gptel--known-presets nil))
        (cl-letf (((symbol-function 'org-graph/agent-tools) (lambda () '(tool-a))))
          (expect (org-graph-workspace-integration--populate-assistant-tools)
                  :to-be 'skipped))))))

(provide 'workspace-integration-spec)
;;; workspace-integration-spec.el ends here
