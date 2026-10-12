;; -*- lexical-binding: t; -*-

;; org-roam is sensitive to org version changes
;; Ensure we have a compatible version of org loaded
(straight-use-package 'org)

;; ===============================================================================
;; Configure Org-Roam Core
;; ===============================================================================

(use-package org-roam
  :straight (org-roam :host github :repo "org-roam/org-roam" :tag "v2.3.1")
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n d" . org-roam-dailies-goto-today))
  :config
  (setq org-roam-directory (file-truename (expand-file-name "roam" jf/org-directory)))

  ;; Ensure gptel subdirectory exists for agent-created notes
  (let ((gptel-dir (expand-file-name "gptel" org-roam-directory)))
    (unless (file-directory-p gptel-dir)
      (make-directory gptel-dir t)))

  ;; Ensure reference subdirectory exists for reference material summaries
  (let ((reference-dir (expand-file-name "reference" org-roam-directory)))
    (unless (file-directory-p reference-dir)
      (make-directory reference-dir t)))

  (setq find-file-visit-truename t)
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

;; Set the directory for daily notes
;; this may be overridden by machine specific config

(setq org-roam-dailies-directory "dailies/")

;; Configure daily note templates
(setq org-roam-dailies-capture-templates
      `(("d" "default" plain
         (file ,(expand-file-name "templates/org-roam-dailies-default.org" jf/emacs-dir))
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n\n")
         :unnarrowed t)))

;; Enable org-roam-protocol
(require 'org-roam-protocol)

(defun jf/server-name-for-instance ()
  "Return the server name this Emacs serves from init, or nil for none.
Nil for a daemon (it serves its daemon name after init) and for a batch
Emacs.  \"isolated-CHECKOUT\" for an instance bin/emacs-isolated.sh
launched (EMACS_USER_DIRECTORY set), CHECKOUT the name of the directory
holding this configuration with each character a target name may not
hold made `-', and with \"-PID\" added when that name is already served.
\"server\", the default socket, for any other Emacs: the live session."
  (cond
   ((or (daemonp) noninteractive) nil)
   ((getenv "EMACS_USER_DIRECTORY")
    (let ((name (concat "isolated-"
                        (replace-regexp-in-string
                         "[^A-Za-z0-9._-]" "-"
                         (file-name-nondirectory (directory-file-name jf/emacs-dir))))))
      (if (server-running-p name)
          (format "%s-%d" name (emacs-pid))
        name)))
   (t "server")))

;; Start the server the protocol needs, under the name this instance may
;; serve; the live session leaves a `server' another Emacs holds alone.
(let ((name (jf/server-name-for-instance)))
  (when (and name (not (server-running-p name)))
    (setq server-name name)
    (server-start)))

;; Configure standard capture templates
(setq org-roam-capture-templates
 `(("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)
   ("f" "foo" plain
    (file ,(expand-file-name "templates/org-roam-default.org" jf/emacs-dir))
    :if-new (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n ${body}")
    :unnarrowed t)))

;; Templates for capturing web content
(setq org-roam-capture-ref-templates
      '(("r" "ref" plain
        "%?"
        :target
        (file+head "${slug}.org" "#+title: ${title}\n${body}")
        :unnarrowed t)
        ("b" "browser-history" plain
        "%?"
        :target
        (file+head "${slug}.org" "#+title: ${title}\n\n")
        :unnarrowed t)))

;; Configure automatic Git synchronization
;;(use-package git-sync-mode
;;  :straight (git-sync-mode :host github :repo "justinbarclay/git-sync-mode")
;;  :config
;;  (git-sync-global-mode)
;;  (add-to-list 'git-sync-allow-list '"~/org/roam"))
