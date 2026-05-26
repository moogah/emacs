;;; buffer-membership-spec.el --- Behavioral tests for workspace buffer membership -*- lexical-binding: t; -*-

;; Bufferlo provides the buffer-list scoping; in tests we mock its
;; surface with `cl-letf' rather than relying on the package being
;; installed at test time.  The surfaces we depend on:
;;
;;   - `bufferlo-buffer-list' — current-tab buffer list
;;   - `bufferlo-remove'      — remove a buffer from the current tab's list
;;
;; The buffer-membership module also installs `workspace--on-buffer-killed'
;; on `kill-buffer-hook'.  This file exercises that path with real
;; (live) buffers.

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el"        dir))
  (load (expand-file-name "../tabs.el"              dir))
  ;; buffer-membership.el calls `(use-package bufferlo ...)' which
  ;; would fail in the test runner; stub the form by binding the
  ;; symbol before load.  Real bufferlo is only used in the running
  ;; configuration, not in tests.
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defun bm-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
  ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
  (spy-on 'workspace-scaffold :and-call-fake
          (lambda (home _name &rest _) (make-directory home t) home))
  (setq workspaces-default-parent-directory
        (make-temp-file "ws-bm-spec-" t)))

(defun bm-spec--with-membership (alist body-fn)
  "Run BODY-FN while bufferlo's per-tab list is mocked from ALIST.
ALIST maps workspace-name (string) → list of buffer objects."
  (cl-letf* ((current-list nil)
             ((symbol-function 'bufferlo-buffer-list)
              (lambda (&rest _) current-list))
             ((symbol-function 'bufferlo-remove)
              (lambda (buf) (setq current-list (remq buf current-list)))))
    (cl-flet ((activate (name)
                (setq current-list (cdr (assoc name alist)))))
      (funcall body-fn #'activate))))

(describe "displayed buffers become workspace members (synced via :buffer-files)"
  (before-each (bm-spec--reset))

  (it "syncs file paths from bufferlo into the workspace plist"
    (let* ((tmpfile (make-temp-file "ws-bm-"))
           (resolved (file-truename tmpfile)))
      (unwind-protect
          (let ((buf (find-file-noselect tmpfile)))
            (workspace-new "alpha")
            (bm-spec--with-membership
             `(("alpha" . (,buf)))
             (lambda (activate)
               (funcall activate "alpha")
               (workspace--sync-registry-from-bufferlo "alpha")
               (let ((ws (gethash "alpha" workspace--registry)))
                 (expect (workspace--buffer-files ws)
                         :to-equal (list resolved)))))
            (kill-buffer buf))
        (delete-file tmpfile)))))

(describe "non-file buffers are members in-session but omitted from :buffer-files"
  (before-each (bm-spec--reset))

  (it "skips *Messages* and the like during sync"
    (let ((buf (get-buffer-create "*ws-test-non-file*")))
      (unwind-protect
          (progn
            (workspace-new "alpha")
            (bm-spec--with-membership
             `(("alpha" . (,buf)))
             (lambda (activate)
               (funcall activate "alpha")
               (workspace--sync-registry-from-bufferlo "alpha")
               (let ((ws (gethash "alpha" workspace--registry)))
                 (expect (workspace--buffer-files ws) :to-equal nil)))))
        (kill-buffer buf)))))

(describe "workspace-remove-buffer"
  (before-each (bm-spec--reset))

  (it "removes from current workspace only; does not kill the buffer"
    (let* ((tmpfile (make-temp-file "ws-rm-"))
           (resolved (file-truename tmpfile))
           (buf (find-file-noselect tmpfile)))
      (unwind-protect
          (progn
            (workspace-new "alpha")
            (workspace-new "beta")
            (bm-spec--with-membership
             `(("alpha" . (,buf))
               ("beta"  . (,buf)))
             (lambda (activate)
               ;; Sync membership into both workspaces.
               (workspace-switch "alpha")
               (funcall activate "alpha")
               (workspace--sync-registry-from-bufferlo "alpha")
               (workspace-switch "beta")
               (funcall activate "beta")
               (workspace--sync-registry-from-bufferlo "beta")
               ;; Now remove from alpha.
               (workspace-switch "alpha")
               (funcall activate "alpha")
               (workspace-remove-buffer buf)
               (workspace--sync-registry-from-bufferlo "alpha")
               (expect (workspace--buffer-files
                        (gethash "alpha" workspace--registry))
                       :to-equal nil)
               (expect (workspace--buffer-files
                        (gethash "beta" workspace--registry))
                       :to-equal (list resolved))
               (expect (buffer-live-p buf) :to-be t))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-file tmpfile))))

  (it "errors when not on a workspace tab"
    (let ((buf (get-buffer "*scratch*")))
      (expect (workspace-remove-buffer buf) :to-throw 'user-error))))

(describe "kill-buffer does not mutate workspace membership"
  ;; Design change: kill-buffer is no longer destructive to
  ;; :buffer-files (the previous Story A).  Killing a buffer is a
  ;; transient session action and should not modify the workspace's
  ;; saved file list.  Explicit removal flows through
  ;; `workspace-remove-buffer'.  The save-time bufferlo sync
  ;; (workspace--autosave-current-layout) updates :buffer-files only
  ;; when the user actually saves.
  (before-each (bm-spec--reset))

  (it "leaves :buffer-files untouched when a buffer is killed"
    (let* ((tmpfile (make-temp-file "ws-kill-"))
           (resolved (file-truename tmpfile))
           (buf (find-file-noselect tmpfile)))
      (unwind-protect
          (progn
            (workspace-new "alpha")
            (workspace-new "beta")
            (puthash "alpha"
                     (workspace--add-buffer-file
                      (gethash "alpha" workspace--registry) resolved)
                     workspace--registry)
            (puthash "beta"
                     (workspace--add-buffer-file
                      (gethash "beta" workspace--registry) resolved)
                     workspace--registry)
            (kill-buffer buf)
            (expect (workspace--buffer-files
                     (gethash "alpha" workspace--registry))
                    :to-equal (list resolved))
            (expect (workspace--buffer-files
                     (gethash "beta" workspace--registry))
                    :to-equal (list resolved)))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-file tmpfile)))))

(describe "kill-buffer is not advised by workspaces"
  (it "has no workspace advice on kill-buffer"
    (let (advised)
      (advice-mapc (lambda (fn _props)
                     (when (and (symbolp fn)
                                (string-prefix-p "workspace" (symbol-name fn)))
                       (push fn advised)))
                   'kill-buffer)
      (expect advised :to-equal nil))))

(provide 'buffer-membership-spec)
;;; buffer-membership-spec.el ends here
