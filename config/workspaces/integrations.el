;;; integrations.el --- Workspace integration registry -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'workspace-data-model)

(defvar workspace--integrations nil
  "Alist of (ID . PLIST) registered workspace integrations, in order.
ID is a symbol.  PLIST may carry `:label' (string), `:on-create'
\(payload -> ok|skipped|(failed . reason)) and `:menu' ((KEY . COMMAND)).
Insertion order is significant: dispatch and menu rendering both
follow it, and re-registering an existing ID updates in place.")

(cl-defun workspace-register-integration (id &key label on-create menu)
  "Register workspace integration ID (a symbol).
LABEL is a display string.  ON-CREATE is an optional handler called
with the anchor payload at workspace-creation time, returning `ok',
`skipped', or (failed . REASON).  MENU is an optional (KEY . COMMAND)
pair for the integration transient.

At least one of ON-CREATE or MENU must be supplied; otherwise signal a
`user-error' (a registration with neither surface is inert and almost
certainly a mistake).

Re-registering an already-present ID replaces its plist IN PLACE,
preserving the entry's position in `workspace--integrations'.  A new
ID is appended to the end, so registration order == dispatch/menu
order.  Returns ID."
  (unless (or on-create menu)
    (user-error "workspace-register-integration: %s has neither :on-create nor :menu" id))
  (let ((plist (list :label label :on-create on-create :menu menu))
        (cell (assq id workspace--integrations)))
    (if cell
        (setcdr cell plist)
      (setq workspace--integrations
            (append workspace--integrations (list (cons id plist))))))
  id)

(defun workspace--integration-payload (name home context)
  "Build the anchor payload plist for a workspace creation event.
NAME is the workspace name, HOME its filesystem anchor directory,
CONTEXT one of the symbols `fresh', `anchored-scaffolded', or
`anchored-existing'.  `:sessions-dir' is derived from HOME via
`workspace--sessions-dir' (no path logic is duplicated here).  The
plist is flat and extensible: handlers read only the keys they need."
  (list :name name
        :home home
        :sessions-dir (workspace--sessions-dir home)
        :context context))

(defun workspace--run-one-integration (entry payload)
  "Run ENTRY's `:on-create' on PAYLOAD; return (ID . OUTCOME).
ENTRY is a (ID . PLIST) cell.  OUTCOME is `ok', `skipped', or
\(failed . REASON).  The handler call is wrapped in `condition-case':
a signalled error normalizes to (failed . ERROR-MESSAGE).  Handler
return values are mapped as `ok' -> ok, `skipped' -> skipped,
\(failed . R) -> (failed . R); any other value is treated leniently
as `ok'."
  (let ((id (car entry))
        (handler (plist-get (cdr entry) :on-create)))
    (condition-case err
        (cons id
              (pcase (funcall handler payload)
                ('ok 'ok)
                ('skipped 'skipped)
                (`(failed . ,reason) (cons 'failed reason))
                (_ 'ok)))
      (error (cons id (cons 'failed (error-message-string err)))))))

(defun workspace--dispatch-create-integrations (name home context)
  "Dispatch all integration `:on-create' handlers for a new workspace.
NAME, HOME, CONTEXT describe the just-created workspace (see
`workspace--integration-payload').  Each registered integration with
an `:on-create' handler is run once, in registration order, on the
shared payload.  Entries lacking `:on-create' are skipped.  Each
\(failed . REASON) outcome is surfaced via `message' as
\"workspace: integration %s failed: %s\".  Returns the alist of
\(ID . OUTCOME) results.  NEVER signals: handler errors are caught by
`workspace--run-one-integration'."
  (let ((payload (workspace--integration-payload name home context))
        (results nil))
    (dolist (entry workspace--integrations)
      (when (plist-get (cdr entry) :on-create)
        (let* ((result (workspace--run-one-integration entry payload))
               (outcome (cdr result)))
          (when (and (consp outcome) (eq (car outcome) 'failed))
            (message "workspace: integration %s failed: %s"
                     (car result) (cdr outcome)))
          (push result results))))
    (nreverse results)))

(provide 'workspace-integrations)
;;; integrations.el ends here
