;;; workspace-integration-spec.el --- Tests for gptel workspace integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Consumer-side contract for the gptel-session workspace integration
;; (`config/gptel/sessions/workspace-integration.org').  gptel is the
;; first real consumer of the workspace integration registry; this
;; module attaches `:on-create' and `:menu' handlers from the gptel
;; side of the directionality boundary
;; (`register/boundary/gptel-sessions-workspace-consult').
;;
;; Assertions:
;;   - `:on-create' calls `jf/gptel--create-session-core' with
;;     project-root = `:home' and a session dir under `:sessions-dir'.
;;   - `:on-create' returns `skipped' when `:context' is
;;     `anchored-existing' OR when the initial preset is nil.
;;   - `:menu' handler prompts (name + preset) and creates a scoped
;;     session under the payload's `:sessions-dir' with
;;     project-root = `:home'.
;;   - Behavioral round-trip: a session produced by the REAL builder
;;     opens in `gptel-chat-mode' with the preset applied
;;     (`register/shape/session-sibling-system-prompt-file').

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-session-filesystem)
(require 'gptel-session-commands)
(require 'jf-gptel-workspace-integration)

;; Real-mode round-trip needs the actual chat-mode + preset machinery.
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-registry)
(require 'gptel-chat-mode)
(require 'gptel-chat-menu)
(require 'gptel)

(describe "gptel-session :on-create handler"

  (it "calls the session builder with project-root = home and a dir under sessions-dir"
    (let* ((calls nil)
           (jf/gptel-workspace-initial-preset 'executor)
           (payload (list :name "demo"
                          :home "/ws/demo"
                          :sessions-dir "/ws/demo/.gptel/sessions"
                          :context 'fresh)))
      (cl-letf (((symbol-function 'jf/gptel--create-session-core)
                 (lambda (session-id session-dir preset-name
                          &optional _ic _wp project-root _parent)
                   (push (list :id session-id
                               :dir session-dir
                               :preset preset-name
                               :project-root project-root)
                         calls)
                   (list :session-id session-id))))
        (expect (jf/gptel--workspace-on-create payload) :to-equal 'ok)
        (expect (length calls) :to-equal 1)
        (let ((c (car calls)))
          (expect (plist-get c :preset) :to-equal 'executor)
          (expect (plist-get c :project-root) :to-equal "/ws/demo")
          ;; Session dir lands under the payload's sessions-dir.
          (expect (file-name-directory
                   (directory-file-name (plist-get c :dir)))
                  :to-equal "/ws/demo/.gptel/sessions/")))))

  (it "returns 'skipped for an adopted (anchored-existing) workspace"
    (let ((jf/gptel-workspace-initial-preset 'executor)
          (built nil))
      (cl-letf (((symbol-function 'jf/gptel--create-session-core)
                 (lambda (&rest _) (setq built t) nil)))
        (expect (jf/gptel--workspace-on-create
                 (list :name "adopted"
                       :home "/ws/adopted"
                       :sessions-dir "/ws/adopted/.gptel/sessions"
                       :context 'anchored-existing))
                :to-equal 'skipped)
        (expect built :to-be nil))))

  (it "returns 'skipped when the initial preset is nil"
    (let ((jf/gptel-workspace-initial-preset nil)
          (built nil))
      (cl-letf (((symbol-function 'jf/gptel--create-session-core)
                 (lambda (&rest _) (setq built t) nil)))
        (expect (jf/gptel--workspace-on-create
                 (list :name "no-preset"
                       :home "/ws/np"
                       :sessions-dir "/ws/np/.gptel/sessions"
                       :context 'fresh))
                :to-equal 'skipped)
        (expect built :to-be nil)))))

(describe "gptel-session :menu add-session handler"

  (it "prompts for name + preset and creates a scoped session under sessions-dir"
    (let* ((calls nil)
           (payload (list :name "demo"
                          :home "/ws/demo"
                          :sessions-dir "/ws/demo/.gptel/sessions"
                          :context 'fresh)))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "my chat"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "researcher"))
                ((symbol-function 'jf/gptel--create-session-core)
                 (lambda (session-id session-dir preset-name
                          &optional _ic _wp project-root _parent)
                   (push (list :id session-id
                               :dir session-dir
                               :preset preset-name
                               :project-root project-root)
                         calls)
                   (list :session-id session-id))))
        (expect (jf/gptel--workspace-add-session payload) :to-equal 'ok)
        (expect (length calls) :to-equal 1)
        (let ((c (car calls)))
          (expect (plist-get c :preset) :to-equal 'researcher)
          (expect (plist-get c :project-root) :to-equal "/ws/demo")
          ;; Generated id is slug-based from the prompted name.
          (expect (plist-get c :id) :to-match "\\`my-chat-")
          (expect (file-name-directory
                   (directory-file-name (plist-get c :dir)))
                  :to-equal "/ws/demo/.gptel/sessions/"))))))

(describe "gptel-session integration: behavioral round-trip"

  ;; Uses the REAL builder (no mock) to prove the :on-create path
  ;; produces a real scoped session that opens in gptel-chat-mode with
  ;; the preset applied.

  (let ((temp-root nil)
        (sessions-dir nil)
        (buf nil)
        (rt-preset 'ws-integration-roundtrip-preset))

    (before-each
      (setq temp-root (make-temp-file "gptel-ws-integration-" t))
      (setq sessions-dir (expand-file-name ".gptel/sessions" temp-root))
      (make-directory sessions-dir t)
      (gptel-make-preset rt-preset :temperature 0.63))

    (after-each
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (setq buf nil)
      (setq gptel--known-presets
            (assq-delete-all rt-preset gptel--known-presets))
      (when (and temp-root (file-directory-p temp-root))
        (delete-directory temp-root t)))

    (it "produces a session.org that opens in chat-mode with the preset applied"
      (let* ((jf/gptel-workspace-initial-preset rt-preset)
             (payload (list :name "roundtrip"
                            :home temp-root
                            :sessions-dir sessions-dir
                            :context 'fresh)))
        (expect (jf/gptel--workspace-on-create payload) :to-equal 'ok)
        ;; Exactly one session directory was created under sessions-dir.
        (let* ((entries (cl-remove-if-not
                         (lambda (d) (file-directory-p
                                      (expand-file-name d sessions-dir)))
                         (directory-files sessions-dir nil "\\`roundtrip-")))
               (session-dir (expand-file-name (car entries) sessions-dir))
               (session-file
                (expand-file-name "branches/main/session.org" session-dir)))
          (expect (length entries) :to-equal 1)
          ;; Drawer present in the produced file (real session shape).
          (let ((content (with-temp-buffer
                           (insert-file-contents session-file)
                           (buffer-string))))
            (expect content :to-match ":PROPERTIES:")
            (expect content
                    :to-match ":GPTEL_PRESET: ws-integration-roundtrip-preset\n"))
          ;; Opens in chat-mode with the preset applied.
          (setq buf (find-file-noselect session-file))
          (with-current-buffer buf
            (expect (derived-mode-p 'gptel-chat-mode) :to-be-truthy)
            (expect gptel--preset :to-equal rt-preset)
            (expect gptel-temperature :to-equal 0.63))
          ;; Registry cleanup for the auto-initialised buffer.
          (let ((sid (file-name-nondirectory
                      (directory-file-name session-dir))))
            (remhash (jf/gptel--registry-key sid "main")
                     jf/gptel--session-registry)))))))

(provide 'workspace-integration-spec)
;;; workspace-integration-spec.el ends here
