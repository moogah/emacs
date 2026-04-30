;;; expansion-transient-scope.el --- Shape predicate scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/shape/expansion-transient-scope
;; generated-at: 2026-04-29T17:30:00Z (cycle-3 plan)
;; license: implementor-may-revise
;;
;; The five-key plist that lives on (transient-scope) for the
;; expansion transient menu. Producers: --prompt-expansion (initial)
;; and --process-expansion-queue (re-emit). Consumers: every terminal
;; action handler under * Expansion Actions in scope-expansion.org.
;;
;; This entry is speculated until cycle-3 task
;; `harden-add-to-scope-action-handler` lands and exercises the shape
;; in its new refusal / redirect / no-op branches.

(require 'buttercup)

(defun shape/validate-expansion-transient-scope/scaffold (val)
  "Speculated shape predicate.
VAL is the plist passed as :scope to `transient-setup' for
`jf/gptel-scope-expansion-menu'.  Return nil on success or an error
symbol identifying the first failed precondition.  See
register/shape/expansion-transient-scope for the canonical key set."
  (cond ((not (plistp val)) 'not-a-plist)
        ((not (plistp (plist-get val :violation))) 'violation-not-plist)
        ((not (functionp (plist-get val :callback))) 'callback-not-function)
        ((not (listp (plist-get val :patterns))) 'patterns-not-list)
        ((not (stringp (plist-get val :tool-name))) 'tool-name-not-string)
        ((not (and (bufferp (plist-get val :chat-buffer))
                   (buffer-live-p (plist-get val :chat-buffer))))
         'chat-buffer-not-live-buffer)
        (t nil)))

(describe "shape: expansion-transient-scope"

  (it "rejects values that are not plists"
    (error
     "speculated; not implemented — call shape/validate-expansion-transient-scope on a non-plist (vector, string, alist with stray symbols), assert the returned symbol is `not-a-plist`"))

  (it "rejects plists missing a :violation sub-plist"
    (error
     "speculated; not implemented — fixture a plist with :callback :patterns :tool-name :chat-buffer but no :violation; assert returned symbol is `violation-not-plist`"))

  (it "rejects plists whose :callback is not a function"
    (error
     "speculated; not implemented — fixture a plist with :callback set to a string; assert returned symbol is `callback-not-function`"))

  (it "rejects plists whose :patterns is not a list"
    (error
     "speculated; not implemented — fixture a plist with :patterns set to a string; assert returned symbol is `patterns-not-list`"))

  (it "rejects plists whose :tool-name is not a string"
    (error
     "speculated; not implemented — fixture a plist with :tool-name set to a symbol; assert returned symbol is `tool-name-not-string`"))

  (it "rejects plists whose :chat-buffer is not a live buffer"
    (error
     "speculated; not implemented — fixture (a) a plist with :chat-buffer set to a string and (b) a plist with a killed buffer; assert returned symbol is `chat-buffer-not-live-buffer` in both"))

  (it "accepts the canonical five-key plist constructed by --prompt-expansion"
    (error
     "speculated; not implemented — fixture a complete plist (:violation a violation-info plist; :callback (lambda (_) nil); :patterns '(\"/a/**\"); :tool-name \"read_file\"; :chat-buffer (current-buffer)); assert validator returns nil"))

  (it "tolerates extra keys (forward-compat)"
    (error
     "speculated; not implemented — add :extra-key 'unused to the canonical plist; assert validator still returns nil (extra keys are not rejected; only required keys are checked)")))

(provide 'expansion-transient-scope)
;;; expansion-transient-scope.el ends here
