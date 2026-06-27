;;; preset-scope-key-extraction-spec.el --- :org-roam-patterns is not extracted; legacy keys warn -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Regression spec for scope-rearch-followups Bug 3.
;;
;; Cycle-3 removed the scope.yml writer and the scope-yaml module. But
;; jf/gptel-preset--extract-scope (config/gptel/presets/registration.el)
;; still listed :org-roam-patterns in its `scope-keys' set, silently
;; harvesting the key into `jf/gptel-preset--scope-defaults' even though
;; nothing downstream consumes it.
;;
;; This spec pins the new contract:
;;   - :org-roam-patterns is NOT extracted into scope-defaults.
;;   - Presets that carry legacy scope keys trigger a display-warning
;;     naming the preset and the ignored key(s).
;;
;; Routing note: the user-prompt-suggested location
;; `config/gptel/scope/test/yaml/' no longer exists post-cycle-3 (the
;; YAML subsystem was retired alongside the writer). This spec lives in
;; test/integration/ since the regression spans preset-registration
;; (outside config/gptel/scope/) → the scope subsystem's expectations
;; about what shows up in scope-defaults.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (gptel-dir (expand-file-name "../../../" test-dir)))
  (require 'gptel-preset-registration
           (expand-file-name "presets/registration.el" gptel-dir)))

(describe "jf/gptel-preset--extract-scope"

  (before-each
    (setq jf/gptel-preset--scope-defaults nil))

  (after-each
    (setq jf/gptel-preset--scope-defaults nil))

  (describe "extraction key set"

    (it "extracts :paths, :scope-profile, :shell-commands, :bash-tools"
      ;; Live keys flow through to scope-defaults under the preset name.
      (let* ((plist '(:paths (:read ("/tmp/**"))
                      :scope-profile "coding"
                      :shell-commands (:allow ("ls"))
                      :bash-tools (:deny ("rm"))
                      :system "ignored-non-scope-key"))
             (_ (jf/gptel-preset--extract-scope plist 'test-preset))
             (extracted (cdr (assq 'test-preset jf/gptel-preset--scope-defaults))))
        (expect (plist-get extracted :paths) :to-equal '(:read ("/tmp/**")))
        (expect (plist-get extracted :scope-profile) :to-equal "coding")
        (expect (plist-get extracted :shell-commands) :to-equal '(:allow ("ls")))
        (expect (plist-get extracted :bash-tools) :to-equal '(:deny ("rm")))))

    (it "does NOT extract :org-roam-patterns"
      ;; The key is dead code: nothing in scope-validation /
      ;; scope-expansion / scope-shell-tools consumes it post-cycle-3.
      ;; Extracting it propagates a phantom shape into scope-defaults.
      (let* ((plist '(:paths (:read ("/tmp/**"))
                      :org-roam-patterns (:tags ("gptel"))))
             (_ (jf/gptel-preset--extract-scope plist 'test-preset))
             (extracted (cdr (assq 'test-preset jf/gptel-preset--scope-defaults))))
        (expect (plist-get extracted :paths) :to-equal '(:read ("/tmp/**")))
        (expect (plist-member extracted :org-roam-patterns) :to-be nil))))

  (describe "legacy-key warning"

    (it "warns when a preset carries :org-roam-patterns"
      (spy-on 'display-warning)
      (jf/gptel-preset--extract-scope
       '(:paths (:read ("/tmp/**"))
         :org-roam-patterns (:tags ("gptel")))
       'legacy-preset)
      (expect 'display-warning :to-have-been-called)
      (let ((args (spy-calls-args-for 'display-warning 0)))
        ;; Warning identifies preset and the ignored key so the user can
        ;; clean up the preset definition.
        (expect (format "%S" args) :to-match "legacy-preset")
        (expect (format "%S" args) :to-match ":org-roam-patterns")))

    (it "does NOT warn for presets without legacy scope keys"
      (spy-on 'display-warning)
      (jf/gptel-preset--extract-scope
       '(:paths (:read ("/tmp/**")) :scope-profile "coding")
       'clean-preset)
      (expect 'display-warning :not :to-have-been-called))))

(provide 'preset-scope-key-extraction-spec)

;;; preset-scope-key-extraction-spec.el ends here
