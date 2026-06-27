;;; scope-expansion-action-handler.el --- Boundary scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/boundary/scope-expansion-action-handler
;; generated-at: 2026-04-29T15:50:00Z (cycle-2 plan)
;; license: implementor-may-revise
;;
;; The action-layer dispatch for the expansion UI's "add to scope" key.
;; Three pre-conditions block the writer:
;;   1. nil :operation       → refuse, surface alternative action
;;   2. :match-pattern       → redirect to sibling :read-directory
;;   3. valid (op . resource) → delegate to scope-pattern-writer
;;
;; This entry is speculated until cycle-3 task
;; `harden-add-to-scope-action-handler` lands.

(defun jf/gptel-scope--add-to-scope/scaffold (violation cluster)
  "Speculated action-handler entry point.

VIOLATION is a register/shape/violation-info plist.
CLUSTER is the list of sibling violations the UI surfaced together
(the entire add-to-scope context, used for :match-pattern redirect).

Returns one of:
  (:refused :reason <user-message>) — stage 1 or stage 2 short-circuit
  (:redirected :writer-call <plist>) — stage 2 redirect succeeded
  (:delegated :writer-call <plist>) — stage 3 normal delegation"
  (let ((op (plist-get violation :operation))
        (vt (plist-get violation :validation-type)))
    (cond
     ;; Stage 1: nil-operation refuse
     ((null op)
      (pcase vt
        (:cloud-auth
         (error "speculated; not implemented — should prompt 'add provider <provider> to allow-list' and write to :GPTEL_SCOPE_CLOUD_PROVIDERS:"))
        (:parse-incomplete
         (error "speculated; not implemented — should user-error 'this command could not be parsed; review and edit it manually'"))
        (_
         (error "speculated; not implemented — should user-error 'no operation associated with this violation; cannot add to scope'"))))

     ;; Stage 2: :match-pattern redirect
     ((eq op :match-pattern)
      (let ((sibling (jf/gptel-scope--find-sibling-read-directory/scaffold cluster)))
        (if sibling
            (error "speculated; not implemented — should redirect: substitute SIBLING's :resource for VIOLATION's, then delegate to writer with :operation :read-directory")
          (error "speculated; not implemented — should user-error 'this pattern was evaluated against an unknown root; scope the search root explicitly via --add-path-to-scope instead'"))))

     ;; Stage 3: delegate to writer
     (t
      (error "speculated; not implemented — should call jf/gptel-scope--write-pattern-to-drawer with (current-buffer), VIOLATION's :operation, and VIOLATION's :resource")))))

(defun jf/gptel-scope--find-sibling-read-directory/scaffold (cluster)
  "Speculated helper: find the :read-directory sibling in CLUSTER for a
:match-pattern violation. Returns the matching violation plist or nil."
  (error "speculated; not implemented — should scan CLUSTER for the first plist where (plist-get v :operation) is :read-directory"))

(provide 'scope-expansion-action-handler/scaffold)
;;; scope-expansion-action-handler.el ends here
