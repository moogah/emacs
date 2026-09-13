;;; integrations.el --- Workspace integration registry -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'workspace-data-model)

(defvar workspace--registry)

(defvar workspace--integrations nil
  "Alist of (ID . PLIST) registered workspace integrations, in order.
ID is a symbol.  PLIST may carry `:label' (string), `:on-create'
\(payload -> ok|skipped|(failed . reason)), `:menu' ((KEY . COMMAND)),
and `:on-purge' (payload -> ok|skipped|(failed . reason)).
Insertion order is significant: dispatch and menu rendering both
follow it, and re-registering an existing ID updates in place.")

(cl-defun workspace-register-integration (id &key label on-create menu on-purge)
  "Register workspace integration ID (a symbol).
LABEL is a display string.  ON-CREATE is an optional handler called
with the anchor payload at workspace-creation time, returning `ok',
`skipped', or (failed . REASON).  MENU is an optional (KEY . COMMAND)
pair for the integration transient.  ON-PURGE is an optional handler
called with the anchor payload at workspace-purge (teardown) time,
returning `ok', `skipped', or (failed . REASON).

At least one of ON-CREATE, MENU, or ON-PURGE must be supplied;
otherwise signal a `user-error' (a registration with none of the three
surfaces is inert and almost certainly a mistake).

Re-registering an already-present ID replaces its plist IN PLACE,
preserving the entry's position in `workspace--integrations'.  A new
ID is appended to the end, so registration order == dispatch/menu
order.  Returns ID."
  (unless (or on-create menu on-purge)
    (user-error "workspace-register-integration: %s has none of :on-create, :menu, :on-purge" id))
  (let ((plist (list :label label :on-create on-create :menu menu :on-purge on-purge))
        (cell (assq id workspace--integrations)))
    (if cell
        (setcdr cell plist)
      (setq workspace--integrations
            (append workspace--integrations (list (cons id plist))))))
  id)

(defun workspace--integration-payload (ws context)
  "Build the anchor payload plist for workspace record WS.
WS is the live workspace plist (see `workspace--make'); CONTEXT is the
symbol describing why the payload is being built (`fresh',
`anchored-scaffolded', `anchored-existing', `menu-invoke', `purge').
The payload carries WS's self-knowledge only: `:name', `:home',
`:node-id' (an opaque string or nil, read from WS at call time), and
`:context'.  It is flat and extensible: handlers read only the keys
they need.  Callers that must observe a `:node-id' written mid-flight
rebuild the payload from the registry's CURRENT record for WS's name
\(see `workspace--integration-live-record') rather than caching one."
  (list :name (workspace--name ws)
        :home (workspace--home ws)
        :node-id (workspace-node-id ws)
        :context context))

(defun workspace--integration-live-record (ws)
  "Return the registry's current record for workspace WS, else WS.
WS is a workspace plist.  Look up its `:name' in `workspace--registry'
and return the record stored there, so a caller rebuilding a payload
mid-dispatch observes replacements made by copy-on-write mutators and
any `:node-id' written to the replacement.  Fall back to WS itself
when the name is unregistered (purge-time dispatch) or when
`workspace--registry' is not bound (the registry module is not
loaded)."
  (or (and (boundp 'workspace--registry)
           (gethash (workspace--name ws) workspace--registry))
      ws))

(defun workspace--run-one-integration (entry payload &optional surface-key)
  "Run ENTRY's SURFACE-KEY handler on PAYLOAD; return (ID . OUTCOME).
ENTRY is a (ID . PLIST) cell.  SURFACE-KEY names the handler slot to
run and defaults to `:on-create'; pass `:on-purge' to run the purge
surface.  OUTCOME is `ok', `skipped', or (failed . REASON).  The
handler call is wrapped in `condition-case': a signalled error
normalizes to (failed . ERROR-MESSAGE).  Handler return values are
mapped as `ok' -> ok, `skipped' -> skipped, (failed . R) ->
\(failed . R); any other value is treated leniently as `ok'."
  (let ((id (car entry))
        (handler (plist-get (cdr entry) (or surface-key :on-create))))
    (condition-case err
        (cons id
              (pcase (funcall handler payload)
                ('ok 'ok)
                ('skipped 'skipped)
                (`(failed . ,reason) (cons 'failed reason))
                (_ 'ok)))
      (error (cons id (cons 'failed (error-message-string err)))))))

(defun workspace--dispatch-create-integrations (ws context)
  "Dispatch all integration `:on-create' handlers for new workspace WS.
WS is the just-registered live workspace record; CONTEXT is the
creation context symbol (see `workspace--integration-payload').  Each
registered integration with an `:on-create' handler is run once, in
registration order, on a payload rebuilt immediately before that
handler runs from the registry's CURRENT record for WS's name (see
`workspace--integration-live-record') — so a `:node-id' written by an
earlier handler via `workspace-set-node-id' is present in every later
handler's payload, even if a copy-on-write mutator replaced the
registry entry in between.  Entries lacking `:on-create' are skipped.  Each (failed . REASON)
outcome is surfaced via `message' as
\"workspace: integration %s failed: %s\".  Returns the alist of
\(ID . OUTCOME) results.  NEVER signals: handler errors are caught by
`workspace--run-one-integration'."
  (let ((results nil))
    (dolist (entry workspace--integrations)
      (when (plist-get (cdr entry) :on-create)
        (let* ((payload (workspace--integration-payload
                         (workspace--integration-live-record ws) context))
               (result (workspace--run-one-integration entry payload))
               (outcome (cdr result)))
          (when (and (consp outcome) (eq (car outcome) 'failed))
            (message "workspace: integration %s failed: %s"
                     (car result) (cdr outcome)))
          (push result results))))
    (nreverse results)))

(defun workspace--dispatch-purge-integrations (ws context)
  "Dispatch all integration `:on-purge' handlers for purged workspace WS.
WS is the workspace record being torn down (already removed from the
registry by `workspace-purge'; the record itself still carries `:home'
and `:node-id'); CONTEXT is the teardown context symbol (see
`workspace--integration-payload').  Each registered integration with
an `:on-purge' handler is run once, in registration order, on a
payload rebuilt per handler via `workspace--integration-live-record'
\(which falls back to WS, the name being unregistered) and run via
`workspace--run-one-integration' (surface key `:on-purge').  Entries lacking `:on-purge' are skipped.  Each
\(failed . REASON) outcome is surfaced via `message' as
\"workspace: integration %s purge-teardown failed: %s\".  Returns the
alist of (ID . OUTCOME) results.  NEVER signals: handler errors are
caught by `workspace--run-one-integration'."
  (let ((results nil))
    (dolist (entry workspace--integrations)
      (when (plist-get (cdr entry) :on-purge)
        (let* ((payload (workspace--integration-payload
                         (workspace--integration-live-record ws) context))
               (result (workspace--run-one-integration entry payload :on-purge))
               (outcome (cdr result)))
          (when (and (consp outcome) (eq (car outcome) 'failed))
            (message "workspace: integration %s purge-teardown failed: %s"
                     (car result) (cdr outcome)))
          (push result results))))
    (nreverse results)))

(provide 'workspace-integrations)
;;; integrations.el ends here
