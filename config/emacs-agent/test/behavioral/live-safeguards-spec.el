;;; live-safeguards-spec.el --- A dispatched call disturbs no frame, window or tab -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D12 and § Seams:
;; seam/live-no-disruption.
;;
;; The live session is never driven by a test.  This spec dispatches the
;; preset tools that need no note database (the introspection tools and
;; the two workspaces tools, against a workspace registered in a
;; registry bound for the spec) in the test process, with a split window
;; and a non-trivial tab, and checks that the selected frame, selected
;; window, window configuration and current tab are as they were: the
;; contract standing in for agent-target-policy § Live call leaves the
;; user's windows alone.  The org-graph and org-roam tools read a note
;; database and are left to their own modules' specs.  Pending until the
;; dispatcher and the introspection tools exist.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))
(require 'tab-bar)

(defvar workspace--registry)
(defvar workspace-state-directory-override)

(defun live-safeguards-spec--state ()
  "What the user sees: frame, window, window configuration, tab, buffer."
  (list :frame (selected-frame)
        :window (selected-window)
        :configuration (current-window-configuration)
        :tab (alist-get 'name (tab-bar--current-tab))
        :buffer (window-buffer (selected-window))))

(defun live-safeguards-spec--same-p (before after)
  "Non-nil when AFTER shows the user what BEFORE did."
  (and (eq (plist-get before :frame) (plist-get after :frame))
       (eq (plist-get before :window) (plist-get after :window))
       (compare-window-configurations (plist-get before :configuration)
                                      (plist-get after :configuration))
       (equal (plist-get before :tab) (plist-get after :tab))
       (eq (plist-get before :buffer) (plist-get after :buffer))))


(describe "Dispatched calls leave the user's windows alone (seam/live-no-disruption)"

  (it "leaves frame, window, window configuration and tab as they were across the preset tools (Scenario: agent-target-policy § Live call leaves the user's windows alone)"
    (let* ((home (file-name-as-directory (make-temp-file "ea-home" t)))
           (pane (expand-file-name "pane.txt" home))
           (workspace--registry (make-hash-table :test 'equal))
           (workspace-state-directory-override (make-temp-file "ea-state" t))
           (server-name "server")
           (saved (current-window-configuration))
           (user-buffer (get-buffer-create "*emacs-agent-test user*")))
      (unwind-protect
          (progn
            (with-temp-file pane (insert "pane\n"))
            (puthash "agent-demo" (workspace--make "agent-demo" home)
                     workspace--registry)
            (delete-other-windows)
            (switch-to-buffer user-buffer)
            (split-window-right)
            (let ((before (live-safeguards-spec--state)))
              (dolist (call
                       (list (list "call" "emacs_describe_symbol" "{\"name\":\"car\"}")
                             (list "call" "emacs_find_definition"
                                   "{\"name\":\"jf/resolve-module-path\"}")
                             (list "call" "emacs_variable_value" "{\"name\":\"fill-column\"}")
                             (list "call" "emacs_apropos"
                                   "{\"pattern\":\"jf/agent-\",\"kind\":\"function\"}")
                             (list "call-write" "workspace_add_layout"
                                   (format "{\"workspace\":\"agent-demo\",\"layout_name\":\"ask/demo\",\"split\":\"columns\",\"panes\":[{\"file\":%S}]}"
                                           pane))
                             (list "call-write" "workspace_remove_layout"
                                   "{\"workspace\":\"agent-demo\",\"layout_name\":\"ask/demo\"}")))
                (let ((reply (emacs-agent-test-envelope (apply #'jf/agent-call call))))
                  (expect (cons (nth 1 call) (plist-get reply :ok))
                          :to-equal (cons (nth 1 call) t))
                  (expect (cons (nth 1 call)
                                (live-safeguards-spec--same-p
                                 before (live-safeguards-spec--state)))
                          :to-equal (cons (nth 1 call) t))))))
        (set-window-configuration saved)
        (kill-buffer user-buffer)
        (delete-directory home t)
        (delete-directory workspace-state-directory-override t)))))

;;; live-safeguards-spec.el ends here
