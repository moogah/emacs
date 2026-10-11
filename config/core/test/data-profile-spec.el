;;; data-profile-spec.el --- EMACS_AGENT_DATA: the one parser and the one applier -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D15, D21, D22,
;; D23 and § Seams: seam/data-profile.
;;
;; `jf/data-profile-parse' (config/core/defaults.org) is the only reader
;; of the profile vocabulary (`live' | `none' | `scratch:<dir>'), and
;; `jf/data-profile-apply' the only place a profile becomes settings.  The
;; test process has loaded defaults.el through init.el; every variable the
;; applier sets is bound around the call, so nothing outlives a spec.  The
;; full-configuration effect is in config/emacs-agent/test/behavioral/
;; data-profile-spec.el.  Pending until defaults.org defines them.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(defvar jf/org-directory)
(defvar jf/data-profile)
(defvar jf/data-state-directory)
(defvar workspace-state-directory-override)
(defvar org-graph-watch-workspace-homes)
(defvar org-graph-sync-enabled)
(defvar org-roam-db-location)
(defvar org-id-locations-file)

(defmacro data-profile-spec--bound (&rest body)
  "Run BODY with every variable the applier sets bound to a sentinel."
  (declare (indent 0))
  `(let ((jf/org-directory "/sentinel/org")
         (jf/data-profile 'sentinel)
         (jf/data-state-directory nil)
         (workspace-state-directory-override nil)
         (org-graph-watch-workspace-homes t)
         (org-graph-sync-enabled t)
         (org-roam-db-location "/sentinel/org-roam.db")
         (org-id-locations-file "/sentinel/.org-id-locations"))
     (unwind-protect (progn ,@body)
       (when (and (stringp jf/data-state-directory)
                  (string-prefix-p (expand-file-name temporary-file-directory)
                                   (expand-file-name jf/data-state-directory)))
         (delete-directory jf/data-state-directory t)))))

(defun data-profile-spec--under-p (dir path)
  "Non-nil when PATH lies under DIR."
  (and (stringp dir) (stringp path)
       (string-prefix-p (file-name-as-directory (expand-file-name dir))
                        (expand-file-name path))))


(describe "jf/data-profile-parse (seam/data-profile)"

  (it "reads live, none and scratch:<absolute dir> into a profile plist (seam/data-profile)"
    (expect (jf/data-profile-parse "live") :to-equal '(:kind live))
    (expect (jf/data-profile-parse "none") :to-equal '(:kind none))
    (expect (jf/data-profile-parse "scratch:/tmp/emacs-agent-vault")
            :to-equal '(:kind scratch :dir "/tmp/emacs-agent-vault/")))

  (it "answers nil for an unset or empty value (seam/data-profile)"
    (expect (jf/data-profile-parse nil) :to-be nil)
    (expect (jf/data-profile-parse "") :to-be nil))

  (it "reads an unrecognised value or a relative scratch directory as none, warning with the value (seam/data-profile)"
    (dolist (value '("bogus" "scratch:" "scratch:relative/vault" "Live" "scratch"))
      (let ((warnings nil))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type message &rest _) (push message warnings))))
          (expect (cons value (plist-get (jf/data-profile-parse value) :kind))
                  :to-equal (cons value 'none)))
        (expect (cons value (and (cl-some (lambda (m) (string-search value m)) warnings) t))
                :to-equal (cons value t))))))


(describe "jf/data-profile-apply (seam/data-profile)"

  (it "points the note root at the scratch dir, moves state to a temp dir and turns off home watching (seam/data-profile)"
    (data-profile-spec--bound
      (let ((vault (file-name-as-directory (make-temp-file "ea-vault" t))))
        (unwind-protect
            (progn
              (jf/data-profile-apply (list :kind 'scratch :dir vault))
              (expect jf/data-profile :to-equal (list :kind 'scratch :dir vault))
              (expect (file-name-as-directory jf/org-directory) :to-equal vault)
              (expect (file-directory-p jf/data-state-directory) :to-be t)
              (expect (data-profile-spec--under-p vault jf/data-state-directory) :to-be nil)
              (dolist (path (list workspace-state-directory-override
                                  org-roam-db-location org-id-locations-file))
                (expect (cons path (data-profile-spec--under-p jf/data-state-directory path))
                        :to-equal (cons path t)))
              (expect org-graph-watch-workspace-homes :to-be nil)
              (expect org-graph-sync-enabled :to-be t))
          (delete-directory vault t)))))

  (it "gives none an empty, existing note root in its state dir and turns graph sync off (seam/data-profile)"
    (data-profile-spec--bound
      (jf/data-profile-apply (list :kind 'none))
      (expect (file-directory-p jf/org-directory) :to-be t)
      (expect (directory-files jf/org-directory nil "\\`[^.]") :to-be nil)
      (expect (data-profile-spec--under-p jf/data-state-directory jf/org-directory) :to-be t)
      (expect org-graph-watch-workspace-homes :to-be nil)
      (expect org-graph-sync-enabled :to-be nil)))

  (it "changes nothing for live or for no profile (seam/data-profile)"
    (dolist (profile (list nil (list :kind 'live)))
      (data-profile-spec--bound
        (jf/data-profile-apply profile)
        (expect (list jf/org-directory jf/data-state-directory
                      workspace-state-directory-override org-graph-watch-workspace-homes
                      org-graph-sync-enabled org-roam-db-location org-id-locations-file)
                :to-equal (list "/sentinel/org" nil nil t t
                                "/sentinel/org-roam.db" "/sentinel/.org-id-locations"))))))

;;; data-profile-spec.el ends here
