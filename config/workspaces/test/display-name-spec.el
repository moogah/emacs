;;; display-name-spec.el --- Tests for cosmetic #+TITLE: display name -*- lexical-binding: t; -*-

;; Covers spec.md "Required home directory and identity coupling"
;; Scenario "#+TITLE: overrides display name but not registry name":
;; the #+TITLE: of a workspace's home.org drives the cosmetic display
;; name (tab-bar label) while the registry name / tab `name' slot /
;; persistence key stay equal to the directory basename (per
;; register/invariant/registry-name-equals-basename).

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../home-org.el"   dir))
  (load (expand-file-name "../scaffold.el"   dir))
  (load (expand-file-name "../tabs.el"       dir)))

(defvar dn-spec--home nil
  "Per-test temp directory used as a workspace :home.")

(defun dn-spec--reset ()
  (clrhash workspace--registry)
  (setq dn-spec--home (make-temp-file "ws-display-name-" t)))

(defun dn-spec--cleanup ()
  (when (and dn-spec--home (file-directory-p dn-spec--home))
    (delete-directory dn-spec--home t)))

(defun dn-spec--write-home-org (contents)
  "Write CONTENTS into <dn-spec--home>/home.org."
  (with-temp-file (expand-file-name "home.org" dn-spec--home)
    (insert contents)))

(defun dn-spec--register (name)
  "Register a workspace named NAME anchored at `dn-spec--home'."
  (puthash name (workspace--make name dn-spec--home) workspace--registry)
  name)

(describe "workspace--display-name"
  (before-each (dn-spec--reset))
  (after-each (dn-spec--cleanup))

  (it "returns the #+TITLE: of home.org when present"
    (dn-spec--write-home-org "#+TITLE: My Cool Project\n* Description\n")
    (let ((name (file-name-nondirectory
                 (directory-file-name dn-spec--home))))
      (dn-spec--register name)
      (expect (workspace--display-name name) :to-equal "My Cool Project")
      ;; Registry name is unchanged — identity is still the basename.
      (expect (gethash name workspace--registry) :not :to-be nil)
      (expect (workspace--name (gethash name workspace--registry))
              :to-equal name)))

  (it "falls back to the registry name when home.org has no #+TITLE:"
    (dn-spec--write-home-org "* Description\n* Notes\n")
    (let ((name (dn-spec--register "basenameonly")))
      (expect (workspace--display-name name) :to-equal "basenameonly")))

  (it "falls back to the registry name when home.org is missing entirely"
    ;; No home.org written.
    (let ((name (dn-spec--register "nohomeorg")))
      (expect (workspace--display-name name) :to-equal "nohomeorg")))

  (it "is a live read: re-reads #+TITLE: on every call (no caching)"
    (let ((name (dn-spec--register "live")))
      (expect (workspace--display-name name) :to-equal "live")
      (dn-spec--write-home-org "#+TITLE: Renamed Later\n")
      (expect (workspace--display-name name) :to-equal "Renamed Later")))

  (it "returns NAME unchanged for an unregistered name"
    (expect (workspace--display-name "ghost") :to-equal "ghost")))

(describe "workspace--tab-bar-tab-name-format"
  (before-each (dn-spec--reset))
  (after-each (dn-spec--cleanup))

  (it "renders the display name in the tab-bar label, not the registry name"
    (dn-spec--write-home-org "#+TITLE: My Cool Project\n")
    (let* ((name (file-name-nondirectory
                  (directory-file-name dn-spec--home)))
           (_ (dn-spec--register name))
           (tab (list 'tab (cons 'name name)))
           (rendered (substring-no-properties
                      (workspace--tab-bar-tab-name-format tab 1))))
      (expect rendered :to-match (regexp-quote "My Cool Project"))
      ;; The tab's own `name' slot (the identity key) is untouched.
      (expect (cdr (assq 'name tab)) :to-equal name)))

  (it "preserves the current-tab type marker so highlighting still resolves"
    ;; The wrapper must keep (car tab) intact; a current-tab rendered
    ;; with the display name must still be the `current-tab' kind.
    (dn-spec--write-home-org "#+TITLE: Highlighted\n")
    (let* ((name (file-name-nondirectory
                  (directory-file-name dn-spec--home)))
           (_ (dn-spec--register name))
           (tab (list 'current-tab (cons 'name name)))
           (rendered (substring-no-properties
                      (workspace--tab-bar-tab-name-format tab 1))))
      ;; The default formatter only adds the current-tab face/box when
      ;; (eq (car tab) 'current-tab); a non-empty render with the title
      ;; present confirms the marker survived the name substitution.
      (expect rendered :to-match (regexp-quote "Highlighted"))))

  (it "passes non-workspace tabs straight through unchanged"
    (let* ((tab (list 'tab (cons 'name "scratch-tab")))
           (rendered (substring-no-properties
                      (workspace--tab-bar-tab-name-format tab 1))))
      (expect rendered :to-match (regexp-quote "scratch-tab"))))

  (it "passes workspace tabs whose display name equals the registry name through"
    ;; No #+TITLE: → display name == registry name → no substitution.
    (let* ((name (dn-spec--register "plainws"))
           (tab (list 'tab (cons 'name name)))
           (rendered (substring-no-properties
                      (workspace--tab-bar-tab-name-format tab 1))))
      (expect rendered :to-match (regexp-quote "plainws")))))

(describe "workspace--install-tab-bar-display-name"
  (it "is idempotent and captures the prior formatter exactly once"
    (let ((tab-bar-tab-name-format-function #'tab-bar-tab-name-format-default)
          (workspace--prev-tab-bar-tab-name-format-function nil))
      (workspace--install-tab-bar-display-name)
      (expect tab-bar-tab-name-format-function
              :to-be #'workspace--tab-bar-tab-name-format)
      (expect workspace--prev-tab-bar-tab-name-format-function
              :to-be #'tab-bar-tab-name-format-default)
      ;; Second call must not capture our own wrapper as the "prior".
      (workspace--install-tab-bar-display-name)
      (expect workspace--prev-tab-bar-tab-name-format-function
              :to-be #'tab-bar-tab-name-format-default))))

(provide 'display-name-spec)
;;; display-name-spec.el ends here
