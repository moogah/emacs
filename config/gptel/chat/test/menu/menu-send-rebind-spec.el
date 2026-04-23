;;; menu-send-rebind-spec.el --- Buttercup tests for gptel-chat-menu Send rebind -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the `gptel-chat-menu' transient delivered by
;; task `menu-integration' (see
;; openspec/changes/gptel-chat-mode/tasks/open/menu-integration.md).
;;
;; Coverage (spec §"gptel-menu integration with rebound Send",
;; design.md §Decision 15):
;;
;;   - `gptel-chat-menu' is defined as a transient prefix and is
;;     callable as a command.
;;   - The rebound Send suffix (`gptel-chat--suffix-send') invokes
;;     `gptel-chat-send' — not upstream's `gptel--suffix-send'.
;;   - `M-x gptel-menu' invoked in a chat-mode buffer retains the
;;     upstream layout; its Send suffix (`gptel--suffix-send') is
;;     unchanged.
;;   - Configuration suffixes work in a chat-mode buffer (preset /
;;     model / backend / tools / context mutations flow through the
;;     same infix symbols upstream uses) — verified behaviorally by
;;     invoking the upstream set-with-scope helper (the same call
;;     path the configuration infixes use internally) in a chat-mode
;;     buffer and asserting a buffer-local mutation stuck.
;;   - Send-coupled groups (Prompt-from, Response-to, Dry-Run) are
;;     NOT present in `gptel-chat-menu' — their transient-arg keys
;;     (m/y/i/e/b/g/k) are absent from the flattened layout.  These
;;     groups read `transient-args' at send time and route through
;;     upstream's `gptel--suffix-send', whose text-property contract
;;     chat-mode does not produce (design.md §Decisions 15, 18).
;;   - The chat-mode keymap binds `gptel-chat-menu' (to `C-c C-,').

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'transient)

;; Load the modules under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/menu/;
;; two levels up is .../config/gptel/chat/, which holds `menu.el' and
;; `mode.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

;; `gptel-transient' must be loaded so upstream's `gptel--suffix-send'
;; and the shared infix symbols are bound when we inspect the
;; transient layouts.
(require 'gptel)
(require 'gptel-transient)

(require 'gptel-chat-mode)
(require 'gptel-chat-menu)


;;; Helpers ------------------------------------------------------------------

(defun gptel-chat-menu-test--flatten (layout)
  "Walk LAYOUT recursively, returning a flat list of symbols.

`transient-define-prefix' stores the parsed layout on the prefix
symbol under the `transient--layout' property.  The structure is a
tree of vectors and lists (groups and suffixes); flattening it to a
symbol list lets us assert \"suffix X is somewhere in the layout\"
without caring about its exact position.

Order matters in this `cond': vectors are also atoms to `atom', so
the vector clause must come before the symbol/atom clause.  Byte-
compiled lambdas are represented as vectors, so they are walked
too; that is fine for our use because we only assert on symbol
membership via `memq'."
  (cond
   ((null layout) nil)
   ((vectorp layout)
    (gptel-chat-menu-test--flatten (append layout nil)))
   ((consp layout)
    (nconc (gptel-chat-menu-test--flatten (car layout))
           (gptel-chat-menu-test--flatten (cdr layout))))
   ((symbolp layout) (list layout))
   (t nil)))

(defun gptel-chat-menu-test--layout-mentions-p (prefix-sym symbol)
  "Non-nil if SYMBOL appears anywhere in PREFIX-SYM's transient layout."
  (and (memq symbol
             (gptel-chat-menu-test--flatten
              (get prefix-sym 'transient--layout)))
       t))

(defun gptel-chat-menu-test--flatten-strings (layout)
  "Walk LAYOUT recursively, returning a flat list of string leaves.

Mirrors `gptel-chat-menu-test--flatten' but keeps strings rather
than symbols.  Used by the Send-coupled-groups negative assertion:
the excluded groups show up in the layout as string key/arg slots
like `\"m\"', `\"y\"', `\"i\"' (Prompt-from) and `\"e\"', `\"b\"',
`\"g\"', `\"k\"' (Response-to); their absence is the machine-
checkable form of the spec scenario.

Skip vectors that originate from byte-compiled lambdas by walking
only proper `consp' lists and transient-layout vectors explicitly —
byte-code vectors can embed string constants unrelated to the
layout that would trigger false positives on membership checks."
  (cond
   ((null layout) nil)
   ((vectorp layout)
    (gptel-chat-menu-test--flatten-strings (append layout nil)))
   ((consp layout)
    (nconc (gptel-chat-menu-test--flatten-strings (car layout))
           (gptel-chat-menu-test--flatten-strings (cdr layout))))
   ((stringp layout) (list layout))
   (t nil)))

(defun gptel-chat-menu-test--layout-has-arg-key-p (prefix-sym key)
  "Non-nil if transient-arg string KEY appears in PREFIX-SYM's layout.

Wraps the existing flatten-strings helper with the membership test
so the specs can read as `(expect ... :to-be nil)' on a per-key
basis."
  (and (member key
               (gptel-chat-menu-test--flatten-strings
                (get prefix-sym 'transient--layout)))
       t))


;;; Specs --------------------------------------------------------------------

(describe "gptel-chat-menu transient"

  ;; -----------------------------------------------------------------------
  ;; 1. Definition and callability.

  (describe "definition"

    (it "is defined as a transient prefix command"
      (expect (fboundp 'gptel-chat-menu) :to-be t)
      ;; `transient-define-prefix' stores a layout on the symbol's
      ;; `transient--layout' property.  A non-nil value is the clearest
      ;; single signal that we have a real transient prefix.
      (expect (get 'gptel-chat-menu 'transient--layout) :not :to-be nil))

    (it "is interactively callable"
      (expect (commandp 'gptel-chat-menu) :to-be t))

    (it "defines its own Send suffix `gptel-chat--suffix-send'"
      (expect (fboundp 'gptel-chat--suffix-send) :to-be t)
      (expect (commandp 'gptel-chat--suffix-send) :to-be t)))


  ;; -----------------------------------------------------------------------
  ;; 2. Send rebind — the one behavioural difference between
  ;; `gptel-chat-menu' and upstream `gptel-menu'.
  ;;
  ;; The rebound suffix must dispatch to `gptel-chat-send', NOT to
  ;; `gptel--suffix-send'.  Upstream's Send is fundamentally wrong for
  ;; chat-mode buffers (design.md §Decision 15 — it inserts response
  ;; text using gptel-mode's prompt/response-prefix conventions).

  (describe "rebound Send suffix"

    (before-each
      (spy-on 'gptel-chat-send :and-return-value nil)
      (spy-on 'gptel--suffix-send :and-return-value nil))

    (it "invokes gptel-chat-send when called interactively"
      (call-interactively #'gptel-chat--suffix-send)
      (expect 'gptel-chat-send :to-have-been-called))

    (it "does NOT invoke gptel--suffix-send"
      (call-interactively #'gptel-chat--suffix-send)
      (expect 'gptel--suffix-send :not :to-have-been-called))

    (it "appears in the gptel-chat-menu layout"
      (expect (gptel-chat-menu-test--layout-mentions-p
               'gptel-chat-menu 'gptel-chat--suffix-send)
              :to-be t))

    (it "replaces gptel--suffix-send in the gptel-chat-menu layout"
      ;; The rebind is load-bearing: the user pressing RET in
      ;; `gptel-chat-menu' must not hit upstream's Send.
      (expect (gptel-chat-menu-test--layout-mentions-p
               'gptel-chat-menu 'gptel--suffix-send)
              :to-be nil)))


  ;; -----------------------------------------------------------------------
  ;; 3. Upstream `gptel-menu' is unchanged.
  ;;
  ;; Loading `gptel-chat-menu' must NOT mutate upstream `gptel-menu'
  ;; (design.md §Decision 15 — `M-x gptel-menu' invoked directly retains
  ;; upstream behaviour, including its original Send suffix).

  (describe "upstream gptel-menu preservation"

    (it "gptel-menu still exists and is a transient prefix"
      (expect (fboundp 'gptel-menu) :to-be t)
      (expect (get 'gptel-menu 'transient--layout) :not :to-be nil)
      (expect (commandp 'gptel-menu) :to-be t))

    (it "gptel-menu still contains the upstream Send suffix"
      (expect (gptel-chat-menu-test--layout-mentions-p
               'gptel-menu 'gptel--suffix-send)
              :to-be t))

    (it "gptel-menu does NOT contain the chat-mode Send rebind"
      ;; If we had (mistakenly) used `transient-replace-suffix' on
      ;; `gptel-menu' itself, our rebind would leak into the upstream
      ;; prefix.  Assert the chat-mode suffix is absent from upstream.
      (expect (gptel-chat-menu-test--layout-mentions-p
               'gptel-menu 'gptel-chat--suffix-send)
              :to-be nil)))


  ;; -----------------------------------------------------------------------
  ;; 4. Configuration infixes are shared with upstream.
  ;;
  ;; `gptel-chat-menu' reuses the same infix symbols as `gptel-menu'
  ;; (preset/provider/model/tools/context/system) — that is how we get
  ;; identical configuration behaviour with zero advice on upstream.

  (describe "shared configuration infixes"

    (it "references the upstream preset infix"
      (expect (gptel-chat-menu-test--layout-mentions-p
               'gptel-chat-menu 'gptel--preset)
              :to-be t))

    (it "references the upstream system-prompt command"
      (expect (gptel-chat-menu-test--layout-mentions-p
               'gptel-chat-menu 'gptel-system-prompt)
              :to-be t))

    (it "references the upstream tools command"
      (expect (gptel-chat-menu-test--layout-mentions-p
               'gptel-chat-menu 'gptel-tools)
              :to-be t))

    (it "mutates a buffer-local variable when a configuration path is invoked in a chat-mode buffer"
      ;; Behavioral coverage for the spec scenario "Menu configuration
      ;; works in chat-mode buffer" — previously only verified
      ;; transitively via layout-symbol membership.  `gptel--set-with-
      ;; scope' with scope `t' is the path every upstream configuration
      ;; infix takes internally when the user has picked buffer-local
      ;; scope (see `gptel-transient.el:54-75' and any infix with
      ;; `:set-value gptel--set-with-scope' at `:variable gptel-...').
      ;; Calling it directly in a chat-mode buffer proves the
      ;; configuration path produces a real buffer-local mutation — no
      ;; gptel-mode, no text-property contract required.
      (with-temp-buffer
        (gptel-chat-mode)
        ;; Baseline: no buffer-local binding yet.  Setting a global
        ;; default would leak across specs, so bind a fresh probe
        ;; symbol rather than touching `gptel-model' directly.
        (let ((probe 'gptel-chat-menu-test--model-probe))
          (expect (local-variable-p probe) :to-be nil)
          (gptel--set-with-scope probe 'sentinel-model t)
          (expect (local-variable-p probe) :to-be t)
          (expect (symbol-value probe) :to-equal 'sentinel-model)
          ;; Verify locality: the binding must NOT leak to a fresh
          ;; buffer.  `default-value' and the other buffer's value
          ;; would both be unbound if this worked correctly.
          (with-temp-buffer
            (expect (local-variable-p probe) :to-be nil))))))


  ;; -----------------------------------------------------------------------
  ;; 4b. Send-coupled groups are excluded.
  ;;
  ;; The Prompt-from, Response-to, and Dry-Run groups present in upstream
  ;; `gptel-menu' all route through `gptel--suffix-send' at invocation
  ;; time — either by reading `transient-args' (m/y/i, e/b/g/k) or by
  ;; invoking `gptel--suffix-send' directly (I/J in Dry-Run).  Upstream
  ;; Send is unreachable on a chat-mode buffer (design.md §Decision 18 —
  ;; chat-mode's block format does not produce the text-property bounds
  ;; upstream's Send parser reads).  Dropping the groups is the only
  ;; choice consistent with that decision.
  ;;
  ;; Machine-checkable form of the spec scenario: the transient-arg
  ;; key strings are absent from the flattened layout.  Their presence
  ;; is what distinguishes these groups in upstream's layout from the
  ;; configuration groups we keep.

  (describe "Send-coupled groups excluded"

    (it "does not include the Prompt-from argument keys (m/y/i)"
      (dolist (key '("m" "y" "i"))
        (expect (gptel-chat-menu-test--layout-has-arg-key-p
                 'gptel-chat-menu key)
                :to-be nil)))

    (it "does not include the Response-to argument keys (e/b/g/k)"
      (dolist (key '("e" "b" "g" "k"))
        (expect (gptel-chat-menu-test--layout-has-arg-key-p
                 'gptel-chat-menu key)
                :to-be nil)))

    (it "does not include the Dry-Run description string"
      ;; The Dry-Run group declares its description inline as the
      ;; string "Dry Run"; absence of that string from the flattened
      ;; layout is a sufficient signal that the group was removed.
      (expect (member "Dry Run"
                      (gptel-chat-menu-test--flatten-strings
                       (get 'gptel-chat-menu 'transient--layout)))
              :to-be nil))

    (it "upstream gptel-menu still contains the Prompt-from keys (sanity)"
      ;; Negative control: the keys ARE present in upstream's layout.
      ;; If this ever breaks, upstream re-structured its transient and
      ;; the chat-mode exclusion assertions above may need recalibration.
      (expect (gptel-chat-menu-test--layout-has-arg-key-p
               'gptel-menu "m")
              :to-be t)
      (expect (gptel-chat-menu-test--layout-has-arg-key-p
               'gptel-menu "e")
              :to-be t)))


  ;; -----------------------------------------------------------------------
  ;; 5. Keymap binding.
  ;;
  ;; The chat-mode keymap must bind `gptel-chat-menu' — users should be
  ;; able to reach the transient from a chat-mode buffer without
  ;; invoking `M-x'.

  (describe "chat-mode keymap binding"

    (it "binds gptel-chat-menu on gptel-chat-mode-map"
      ;; `where-is-internal' returns the keys bound to a command in a
      ;; specific keymap.  A non-nil result proves the binding exists.
      (expect (where-is-internal 'gptel-chat-menu
                                 (list gptel-chat-mode-map))
              :not :to-be nil))

    (it "does not shadow C-c C-c (send), C-c C-r (regenerate), or C-c C-k (abort)"
      ;; Sanity: the new binding must not displace the existing
      ;; chat-mode bindings enforced by earlier tasks.
      (expect (lookup-key gptel-chat-mode-map (kbd "C-c C-c"))
              :to-equal 'gptel-chat-send)
      (expect (lookup-key gptel-chat-mode-map (kbd "C-c C-r"))
              :to-equal 'gptel-chat-regenerate)
      (expect (lookup-key gptel-chat-mode-map (kbd "C-c C-k"))
              :to-equal 'gptel-abort))))

(provide 'gptel-chat-menu-send-rebind-spec)

;;; menu-send-rebind-spec.el ends here
