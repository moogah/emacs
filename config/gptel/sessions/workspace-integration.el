;;; workspace-integration.el --- gptel session workspace integration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel-session-filesystem)
(require 'gptel-session-commands)

(defcustom jf/gptel-workspace-initial-preset 'executor
  "Preset for the session auto-created at workspace birth; nil disables it."
  :type '(choice (const :tag "No auto-session" nil) symbol)
  :group 'gptel)

(defun jf/gptel--workspace-on-create (payload)
  "Build a scoped gptel session at workspace birth from PAYLOAD.
PAYLOAD is the anchor plist (`:name' `:home' `:sessions-dir'
`:context').  Returns `skipped' for an adopted workspace
\(`:context' = `anchored-existing') or when
`jf/gptel-workspace-initial-preset' is nil; otherwise builds one
real session scoped to the workspace home via
`jf/gptel--create-session-core' (project-root = home) and returns
`ok'.  Non-interactive: defaults only, never prompts."
  (pcase (plist-get payload :context)
    ('anchored-existing 'skipped)         ; never inject into an adopted workspace
    (_ (if (null jf/gptel-workspace-initial-preset)
           'skipped
         (let* ((home (plist-get payload :home))
                (sdir (plist-get payload :sessions-dir))
                (id   (jf/gptel--generate-session-id
                       (or (plist-get payload :name) "initial")))
                (sd   (expand-file-name id sdir)))
           (jf/gptel--create-session-core
            id sd jf/gptel-workspace-initial-preset nil nil home nil) ; project-root=home → scoped
           'ok)))))

(defun jf/gptel--workspace-read-preset ()
  "Prompt for a registered preset, returning its symbol.
Candidates and annotation mirror the interactive session command."
  (let* ((presets (mapcar #'car gptel--known-presets))
         (annotator (lambda (name)
                      (let* ((preset (gptel-get-preset (intern name)))
                             (desc (plist-get preset :description)))
                        (when desc (format "  -- %s" desc)))))
         (completion-extra-properties
          (list :annotation-function annotator)))
    (intern (completing-read "Select preset: "
                             (mapcar #'symbol-name presets)
                             nil t))))

(defun jf/gptel--workspace-add-session (payload)
  "Create a gptel session in the current workspace from PAYLOAD.
Prompts for a session name (`read-string') and a preset
\(`completing-read' over `gptel--known-presets'), then builds a
scoped session under the workspace's `:sessions-dir' with
project-root = `:home' via `jf/gptel--create-session-core'.
Returns `ok'."
  (let* ((name  (read-string "Session name: "))
         (preset (jf/gptel--workspace-read-preset))
         (home  (plist-get payload :home))
         (sdir  (plist-get payload :sessions-dir))
         (id    (jf/gptel--generate-session-id
                 (if (string-empty-p name) "session" name)))
         (sd    (expand-file-name id sdir)))
    (jf/gptel--create-session-core
     id sd preset nil nil home nil)       ; project-root=home → scoped
    'ok))

(with-eval-after-load 'workspaces
  (workspace-register-integration 'gptel-session
    :label "gptel session"
    :on-create #'jf/gptel--workspace-on-create
    :menu (cons "g" #'jf/gptel--workspace-add-session)))

(provide 'jf-gptel-workspace-integration)
