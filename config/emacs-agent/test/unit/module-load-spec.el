;;; module-load-spec.el --- The emacs-agent module in the configuration -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D8, D20 and
;; § Seams: seam/module-load.
;;
;; The test process has loaded init.el, so these specs read the module as
;; the full configuration loads it.  Pending until config/emacs-agent/
;; exists and is in `jf/enabled-modules'.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defconst module-load-spec--dir
  (expand-file-name "config/emacs-agent/" emacs-agent-test-root)
  "The module directory.")


(describe "The emacs-agent module (seam/module-load)"

  (xit "is enabled after org-graph and loads with its presets and introspection tools (seam/module-load)"
    (let ((order (mapcar #'car jf/enabled-modules)))
      (expect (member "emacs-agent/emacs-agent" order) :not :to-be nil)
      (expect (member "emacs-agent/emacs-agent" (member "org-graph/org-graph" order))
              :not :to-be nil))
    (expect (featurep 'emacs-agent) :to-be t)
    (expect (gptel-get-preset 'emacs-agent-read) :not :to-be nil)
    (expect (gptel-get-preset 'emacs-agent-write) :not :to-be nil)
    (expect (gptel-tool-name (gptel-get-tool "emacs_describe_symbol"))
            :to-equal "emacs_describe_symbol"))

  (it "has one .org source per tangled file, each starting with the lexical-binding line (seam/module-load)"
    (let ((tangled (directory-files module-load-spec--dir t "\\.el\\'")))
      (expect (sort (mapcar #'file-name-base tangled) #'string<)
              :to-equal '("action-log" "dispatcher" "emacs-agent" "introspection-tools"
                          "manifest" "presets" "target-support"))
      (dolist (file tangled)
        (expect (cons file (file-exists-p (concat (file-name-sans-extension file) ".org")))
                :to-equal (cons file t))
        (expect (cons file (with-temp-buffer
                             (insert-file-contents file nil 0 300)
                             (goto-char (point-min))
                             (and (looking-at-p ";;;.*-\\*- lexical-binding: t; -\\*-") t)))
                :to-equal (cons file t))))))

;;; module-load-spec.el ends here
