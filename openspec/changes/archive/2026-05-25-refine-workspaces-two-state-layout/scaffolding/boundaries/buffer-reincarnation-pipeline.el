;;; buffer-reincarnation-pipeline.el --- speculated boundary -*- lexical-binding: t; -*-

;; scaffolding-of: register/boundary/buffer-reincarnation-pipeline
;; generated-at: 2026-05-24T20:06:31Z
;; license: implementor-may-revise
;;
;; Boundary shell: the four-stage pipeline from live frame → persisted
;; workspace-buffer structs → reincarnated frame. Each stage gets a
;; function shell with the speculated input/output contract; the
;; Implementor fills in the body or revises the contract.
;;
;; Stage 4 (defer-apply) is load-bearing — `run-at-time' wraps the
;; window-state-put to avoid racing against bookmark--jump-via's
;; buffer-display call. The shell pins the run-at-time and the
;; generation-counter guard.

(defvar workspace--restore-generation 0
  "Monotonic counter incremented on every restore. The deferred apply
checks `(= captured-gen workspace--restore-generation)' and no-ops on
stale runs, so a user mashing workspace-restore cannot collide two
window-state-put calls on the same frame.")

;; Stage 1: capture
(defun workspace--pipeline-capture (frame)
  "Stage 1: walk FRAME's window-state and emit an enriched form whose
leaves carry a workspace-buffer in their `parameters' slot.

Speculated output: (window-state-get) form with leaf parameters
extended by `(workspace-buffer . <struct>)'."
  (ignore frame)
  (error "TODO: implement workspace--pipeline-capture — \
window-state-get on FRAME, then recursive descent (detecting the \
one-window-frame edge via (cl-position 'leaf form)), enriching each leaf's \
parameters alist with a workspace-buffer struct built by \
workspace--capture-leaf."))

;; Stage 2: serialize
(defun workspace--pipeline-serialize (enriched-state)
  "Stage 2: pretty-print ENRICHED-STATE to the persistence file.

Speculated: the standard workspace--write-state path (pp). No new code
needed; this shell is here to pin the boundary's stage-2 contract."
  (ignore enriched-state)
  (error "TODO: confirm workspace--write-state handles the workspace-buffer \
struct via cl-defstruct's default reader/writer. If the embedded bookmark \
record requires custom printing, document the divergence."))

;; Stage 3: restore-walk
(defun workspace--pipeline-restore-walk (persisted-state)
  "Stage 3: walk PERSISTED-STATE and replace each leaf's buffer
reference with a freshly reincarnated buffer via the fallback chain.

Per-leaf `condition-case-unless-debug' guards against help-mode
bug#56643. Short-circuits on first non-nil per leaf (the chain itself
is in scaffolding/vocabularies/buffer-reincarnation-fallback-chain.el)."
  (ignore persisted-state)
  (error "TODO: implement workspace--pipeline-restore-walk — \
recursive descent, condition-case-unless-debug per leaf, call \
workspace--reincarnate-leaf, replace the leaf's buffer reference with \
the returned live buffer."))

;; Stage 4: defer-apply
(defun workspace--pipeline-defer-apply (reincarnated-state frame)
  "Stage 4: defer the `window-state-put' call via `run-at-time nil nil'
so the apply happens after the current command loop tick, avoiding the
race with bookmark--jump-via's buffer-display call.

The `copy-tree' on REINCARNATED-STATE prevents
`--bufferize-window-state's side effects from mutating the in-memory
registry. The generation-counter check no-ops stale deferred runs."
  (ignore reincarnated-state frame)
  (error "TODO: implement workspace--pipeline-defer-apply — \
(let ((gen (cl-incf workspace--restore-generation)) \
      (state (copy-tree reincarnated-state))) \
  (run-at-time nil nil \
               (lambda () \
                 (when (= gen workspace--restore-generation) \
                   (window-state-put state frame))))) — \
the lambda's gen-check is the concurrent-restore guard."))

(provide 'buffer-reincarnation-pipeline)
;;; buffer-reincarnation-pipeline.el ends here
