;;; buffer-reincarnation-fallback-chain.el --- speculated vocabulary -*- lexical-binding: t; -*-

;; scaffolding-of: register/vocabulary/buffer-reincarnation-fallback-chain
;; generated-at: 2026-05-24T20:06:31Z
;; license: implementor-may-revise
;;
;; Canonical mapping function shell: reincarnation step symbol →
;; restorer function. The closed, ordered chain is asserted by the
;; structure of `workspace--reincarnate-leaf' (the `or' below); the
;; pcase here is the per-step dispatch for diagnostics, logging, and
;; for the scaffolded test that asserts the chain is tried in order.
;;
;; The Implementor replaces each (error "TODO ...") arm with the real
;; restorer function and removes the diagnostic error in
;; `workspace--reincarnate-leaf' itself.

(defun workspace--reincarnate-via-bookmark (wb)
  "Step 1: restore via bookmark-handle-bookmark on (workspace-buffer-bookmark WB).
Wrap in condition-case-unless-debug so help-mode bug#56643 falls through to
step 2 instead of aborting the tree."
  (ignore wb)
  (error "TODO: implement workspace--reincarnate-via-bookmark — \
bookmark-handle-bookmark on the saved record; return the resulting buffer \
on success, nil on failure"))

(defun workspace--reincarnate-via-filename (wb)
  "Step 2: restore via find-file-noselect on (workspace-buffer-filename WB)."
  (ignore wb)
  (error "TODO: implement workspace--reincarnate-via-filename — \
find-file-noselect; return the buffer on success, nil if the file is missing \
or filename is nil"))

(defun workspace--reincarnate-via-name (wb)
  "Step 3: restore via get-buffer on (workspace-buffer-name WB)."
  (ignore wb)
  (error "TODO: implement workspace--reincarnate-via-name — \
get-buffer on the saved name; return the live buffer or nil"))

(defun workspace--reincarnation-error-buffer (wb)
  "Step 4: terminating fallback. Return a named, visible buffer describing
the failure. MUST always return a live buffer; never nil."
  (ignore wb)
  (error "TODO: implement workspace--reincarnation-error-buffer — \
get-buffer-create a named buffer (e.g. \"*workspace-reincarnation-error: \
NAME*\") with a short description of which steps failed; return the buffer"))

(defun workspace--reincarnate-leaf (wb)
  "Speculated chain. Try each step in order; first non-nil wins. The
terminating step (`workspace--reincarnation-error-buffer') cannot return
nil, so this function cannot return nil for any valid workspace-buffer."
  (or (workspace--reincarnate-via-bookmark wb)
      (workspace--reincarnate-via-filename wb)
      (workspace--reincarnate-via-name wb)
      (workspace--reincarnation-error-buffer wb)))

(defun workspace--reincarnation-step (step wb)
  "Pcase dispatch on a step symbol — used by tests that exercise the
chain step-by-step to assert ordering and behaviour at each stage."
  (pcase step
    ('bookmark      (workspace--reincarnate-via-bookmark wb))
    ('filename      (workspace--reincarnate-via-filename wb))
    ('name          (workspace--reincarnate-via-name wb))
    ('error-buffer  (workspace--reincarnation-error-buffer wb))
    (_
     (error "workspace--reincarnation-step: unknown step %S \
(closed vocabulary; add to \
register/vocabulary/buffer-reincarnation-fallback-chain if genuinely new, \
not a typo)" step))))

(provide 'buffer-reincarnation-fallback-chain)
;;; buffer-reincarnation-fallback-chain.el ends here
