;;; menu.el --- GPTEL Chat-Mode Menu and Preset Wiring -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Applies a declared preset on `gptel-chat-mode' activation and (in a
;; later task) provides the `gptel-chat-menu' transient prefix.  See
;; `openspec/changes/gptel-chat-mode/architecture.md' §Components
;; (subsection `gptel-chat-menu (menu, preset-wiring)') and design.md
;; §Decisions 15, 16.

;;; Code:

;; Dependencies

;; Load =gptel= softly (=nil t=) so this module remains load-safe in
;; contexts where =gptel= has not been fully initialised yet. The
;; symbols we need (=gptel--apply-preset=, =gptel-get-preset=,
;; =gptel--preset=) are resolved at hook-run time, not load time.

;; We deliberately *do not* =require= =org= here. The property-drawer
;; parser in =gptel-chat--declared-preset= is native =re-search-forward=
;; and has no org dependency, so this module stays load-safe in minimal
;; contexts where =org= has not been pulled in.

;; =transient= is required eagerly — the =transient-define-prefix= and
;; =transient-define-suffix= forms below expand to code that needs the
;; transient package present at load time. =cl-lib= is required for
;; =cl-delete-if= used inside the =gptel-chat-menu= body (mirroring
;; upstream =gptel-menu='s context sanitisation).


;; [[file:menu.org::*Dependencies][Dependencies:1]]
(require 'gptel nil t)
(require 'transient)
(require 'cl-lib)
;; Dependencies:1 ends here

;; Forward declarations

;; Suppress byte-compiler warnings for upstream symbols. They resolve at
;; call time once =gptel= has loaded.


;; [[file:menu.org::*Forward declarations][Forward declarations:1]]
(declare-function gptel--apply-preset "gptel" (preset &optional setter))
(declare-function gptel-get-preset "gptel" (name))
(declare-function gptel-org--entry-properties "gptel-org" (&optional pt))
(declare-function gptel-org-set-properties "gptel-org" (pt &optional msg))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-entry-put "org" (pom property value))
(defvar gptel--preset)
;; Declared (not defined) here — session-local defined in
;; `config/gptel/sessions/constants.org'.  Declaring lets this module
;; compile and write the binding without pulling sessions in.
(defvar jf/gptel--parent-session-id)
;; Forward declarations:1 ends here

;; Preset resolution

;; =gptel-chat--declared-preset= returns the preset name (a symbol)
;; declared in the current buffer, or =nil= if none is declared.

;; Resolution order (design.md §Decision 15; task =preset-wiring=):

;; 1. An Org-style =:PROPERTIES:= drawer at =point-min= with a
;;    =:GPTEL_PRESET: name= line. Parsed via a native regex scoped to
;;    the first drawer at =point-min= — no dependency on =org-mode=
;;    being loaded (task =preset-wiring-robustness=).
;; 2. A file-local =gptel--preset= variable, populated by Emacs's
;;    standard =hack-local-variables= from the =-*- ... -*-= header or a
;;    =Local Variables:= block. =gptel--preset= is already declared
;;    =safe-local-variable= upstream (=gptel.el:336=), so no additional
;;    authorisation is needed.

;; Precedence rationale: the property drawer is authored into the
;; buffer content, while file-locals may be inherited from a parent
;; =.dir-locals.el=. Explicit drawer intent overrides inherited default.

;; The function must be robust to non-org buffers (no drawer parse
;; error) and to stringy values in the drawer (coerce to symbol).

;; Native parsing rationale (task =preset-wiring-robustness=, Finding 2):
;; previously this used =org-entry-get=, which works on non-org buffers
;; only because it internally falls back to regex. Using =re-search-
;; forward= directly removes the implicit coupling to =org-mode= being
;; loaded (chat-mode buffers derive from =text-mode= in some
;; configurations) and decouples us from any future change to org's
;; drawer API. We scope the search to the first =:PROPERTIES: ... :END:=
;; block at =point-min=, matching how upstream =gptel-org--entry-
;; properties= uses the =selective= scope.


;; [[file:menu.org::*Preset resolution][Preset resolution:1]]
(defun gptel-chat--declared-preset ()
  "Return the preset symbol declared in the current buffer, or nil.

Resolution order:

  1. A `:PROPERTIES:' drawer at `point-min' containing a
     `:GPTEL_PRESET: name' line (parsed with a native regex — no
     dependency on `org-mode' being loaded).
  2. The file-local `gptel--preset' variable, if bound.

The property-drawer value wins when both are present — the drawer is
authored into the buffer content while file-locals may be inherited
from a parent `.dir-locals.el'.

A non-nil return is always a symbol; stringy drawer values are
coerced via `intern'."
  (let ((drawer-val
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             ;; Skip leading blank lines only — the drawer must be
             ;; the first non-blank content for us to recognise it,
             ;; matching how upstream `gptel-org--entry-properties'
             ;; scopes its read with `selective'.
             (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
               (forward-line 1))
             (when (looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")
               (let ((drawer-end
                      (save-excursion
                        (when (re-search-forward
                               "^[ \t]*:END:[ \t]*$" nil t)
                          (line-beginning-position)))))
                 (when drawer-end
                   (when (re-search-forward
                          "^[ \t]*:GPTEL_PRESET:[ \t]*\\(\\S-+\\)[ \t]*$"
                          drawer-end t)
                     (match-string-no-properties 1)))))))))
    (cond
     ((and drawer-val (not (string-empty-p drawer-val)))
      (intern drawer-val))
     ((and (boundp 'gptel--preset) gptel--preset)
      (if (symbolp gptel--preset)
          gptel--preset
        (intern (format "%s" gptel--preset)))))))
;; Preset resolution:1 ends here

;; Drawer overrides overlay

;; =gptel-chat--apply-drawer-overrides= overlays *every* drawer-present
;; configuration property buffer-locally. The drawer wins over the
;; preset for every key it carries (Decision 3 of
;; =openspec/changes/gptel-drawer-as-source-of-truth/design.md=,
;; =register/invariant/drawer-overlay-wins-over-preset=). When the
;; drawer omits a key, the preset's value (already installed by the
;; caller =gptel-chat--apply-declared-preset=) survives intact.

;; The asymmetric exception is =:GPTEL_SYSTEM:=. The chat-mode save
;; path never *writes* it (Decision 2 / =register/invariant/drawer-
;; system-key-write-exclusion=), but the overlay still *reads* it for
;; back-compat: a manually authored =:GPTEL_SYSTEM:= line in a fresh
;; or restored session continues to install =gptel--system-message=
;; buffer-locally.

;; It is the restore-path counterpart to the save hook
;; (=gptel-chat--save-state= → =gptel-chat--write-config-drawer=) and
;; is called from =gptel-chat--apply-declared-preset= after preset
;; application.

;; Upstream =gptel-org--entry-properties= already returns a structured
;; tuple of the configuration keys
;; (=preset=, =system=, =backend=, =model=, =temperature=, =tokens=,
;; =num=, =tools=). Reusing it keeps the drawer shape bit-for-bit
;; compatible with what upstream's own =gptel-org--restore-state= reads
;; (design.md §Decision 5).

;; We deliberately *discard* the preset field from the tuple at this
;; layer — the caller (=gptel-chat--apply-declared-preset=) has already
;; applied it via =gptel--apply-preset=; re-applying would undo the
;; preset's composite logic.

;; =GPTEL_PARENT_SESSION_ID= is our chat-mode extension, not in
;; upstream's tuple. It reads via a targeted =org-entry-get= call and,
;; when present, installs =jf/gptel--parent-session-id= as a
;; buffer-local value (design.md §Decision 3).

;; Every overlaid value is set via =make-local-variable= so global
;; defaults in other buffers are never touched.

;; The implementation has no "if drawer matches preset, skip" check.
;; Every drawer-present (non-nil) key triggers a buffer-local set.
;; This is the structural reason the drawer is a credible WYSIWYG
;; source of truth — adding a comparison would silently flip authority
;; back to the preset under conditions the user can't see.


;; [[file:menu.org::*Drawer overrides overlay][Drawer overrides overlay:1]]
(defun gptel-chat--apply-drawer-overrides ()
  "Overlay every drawer-present configuration property buffer-locally.

Reads the `:PROPERTIES:' drawer at `point-min' via upstream
`gptel-org--entry-properties' and, for each non-nil upstream-
compatible key (`:system', `:backend', `:model', `:temperature',
`:max-tokens', `:num-messages-to-send', `:tools'), installs the
value as a buffer-local binding.  The drawer wins over the preset
for every key it carries (Decision 3 of gptel-drawer-as-source-of-
truth, register/invariant/drawer-overlay-wins-over-preset).  No
\"if drawer matches preset, skip\" check — every non-nil key
triggers a buffer-local set.

Additionally reads the chat-mode extension key `GPTEL_PARENT_SESSION_ID'
via `org-entry-get' and, when non-nil and non-empty, sets
`jf/gptel--parent-session-id' buffer-locally.

Absent fields are no-ops — the buffer-local setter is never called
with a nil value, so the preset's value (already installed by the
caller) remains in effect.  This is the asymmetric contract for
`:GPTEL_SYSTEM:': the chat-mode save path never writes the key
(Decision 2 / register/invariant/drawer-system-key-write-exclusion),
so the drawer typically omits it and the preset's `:system' wins.
A manually authored `:GPTEL_SYSTEM:' line is still respected here
for back-compat — when the upstream tuple's `system' field is non-
nil, the drawer entry wins over the preset.

The preset field from `gptel-org--entry-properties' is intentionally
discarded here; it was already applied by the caller
(`gptel-chat--apply-declared-preset').  Re-applying would undo the
preset's composite-key logic (design.md §Decision 5)."
  (when (fboundp 'gptel-org--entry-properties)
    (pcase-let
        ((`(,_preset ,system ,backend ,model ,temperature ,tokens ,num ,tools)
          (gptel-org--entry-properties (point-min))))
      (when system
        (set (make-local-variable 'gptel--system-message) system))
      (when backend
        (set (make-local-variable 'gptel-backend) backend))
      (when model
        (set (make-local-variable 'gptel-model) model))
      (when temperature
        (set (make-local-variable 'gptel-temperature) temperature))
      (when tokens
        (set (make-local-variable 'gptel-max-tokens) tokens))
      (when num
        (set (make-local-variable 'gptel--num-messages-to-send) num))
      (when tools
        (set (make-local-variable 'gptel-tools) tools))))
  (let ((parent-id (and (fboundp 'org-entry-get)
                        (org-entry-get (point-min)
                                       "GPTEL_PARENT_SESSION_ID"
                                       'selective))))
    (when (and parent-id (stringp parent-id)
               (not (string-empty-p parent-id)))
      (set (make-local-variable 'jf/gptel--parent-session-id) parent-id))))
;; Drawer overrides overlay:1 ends here

;; System Prompt heading restore

;; design.md §Addendum (Finding B / Decision B) relocates the system
;; prompt from a drawer property into a visible =* System Prompt=
;; heading body, and makes that body *authoritative*
;; (=register/invariant/system-prompt-heading-authoritative=).

;; On mode activation the restore precedence is, highest first:

;;   1. =* System Prompt= heading body, when non-blank
;;   2. legacy =:GPTEL_SYSTEM:= drawer entry, when present
;;   3. preset =:system=

;; =gptel-chat--apply-drawer-overrides= already handles levels 2 and 3:
;; when the drawer carries =:GPTEL_SYSTEM:= the overlay installs it,
;; otherwise the preset's =:system= (already applied by the caller)
;; stands. =gptel-chat--apply-system-prompt-heading= adds level 1 — it
;; runs *after* the overlay and supersedes both when the heading body is
;; authored. A blank (whitespace-only or absent) heading body is treated
;; as "not authored" and is a no-op, so an empty heading never silently
;; wipes the prompt.

;; The body is read with a narrow/regexp scan — =org-narrow-to-subtree=
;; to the =* System Prompt= heading, then skip its property drawer — not
;; =org-element-parse-buffer= (chat-mode parser Decision 1: the parser
;; stays a regexp state machine, no org-element dependency).

;; The four-step scan (anchor heading → heading-end → next-heading-or-EOB
;; subtree-end → skip the heading's own =:PROPERTIES:= drawer) is shared
;; with the save-side writer =gptel-chat--write-system-prompt-heading=
;; via the helper =gptel-chat--system-prompt-heading-body-region=. The
;; helper returns the post-drawer body region verbatim; the *reader*
;; trims trailing whitespace and treats a blank result as "not authored"
;; (per =register/invariant/system-prompt-heading-authoritative=); the
;; *writer* pads with the leading/trailing newlines the document layout
;; requires (per =register/shape/session-document-layout=). Single-
;; sourcing the scan keeps reader/writer agreement on what "body" means
;; visible at one decision point — silent divergence on body bounds is
;; exactly the shape-corruption class the shape register guards against.


;; [[file:menu.org::*System Prompt heading restore][System Prompt heading restore:1]]
(defun gptel-chat--system-prompt-heading-body-region ()
  "Return (BODY-START . SUBTREE-END) for `* System Prompt', or nil.

Locates the first top-level `* System Prompt' heading in the current
buffer.  BODY-START is the buffer position immediately after the
heading's own `:PROPERTIES:'/`:END:' drawer (when present), or the
position immediately after the heading line otherwise.  SUBTREE-END
is the start of the next `^\\* ' heading, or `point-max'.

Returns nil when the `* System Prompt' heading is absent.  The
returned region is *verbatim* — no trim, no pad: the reader
(`gptel-chat--system-prompt-heading-body') trims and treats a blank
result as \"not authored\" per register/invariant/system-prompt-
heading-authoritative; the writer
(`gptel-chat--write-system-prompt-heading') pads with the leading/
trailing newlines required by register/shape/session-document-layout.
Centralising the scan keeps reader and writer agreed on what \"body\"
means — silent divergence on body bounds is exactly the corruption
class the shape register guards against.

Uses a narrow/regexp scan, not `org-element-parse-buffer' (chat-mode
parser Decision 1)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^\\* System Prompt[ \t]*$" nil t)
        (let* ((heading-end (line-end-position))
               (subtree-end
                (save-excursion
                  (goto-char heading-end)
                  (if (re-search-forward "^\\* " nil t)
                      (line-beginning-position)
                    (point-max))))
               (body-start
                (save-excursion
                  (goto-char heading-end)
                  (forward-line 1)
                  ;; Skip the heading's own `:PROPERTIES:' drawer.
                  (when (and (< (point) subtree-end)
                             (looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$"))
                    (when (re-search-forward "^[ \t]*:END:[ \t]*$"
                                             subtree-end t)
                      (forward-line 1)))
                  (point))))
          (cons body-start subtree-end))))))

(defun gptel-chat--system-prompt-heading-body ()
  "Return the body text of the `* System Prompt' heading, or nil.

Locates the first top-level `* System Prompt' heading in the current
buffer, skips its `:PROPERTIES:' ... `:END:' drawer (and any blank
lines after it), and returns the remaining subtree text up to — but
not including — the next heading.  Trailing whitespace is trimmed.

Returns nil when there is no `* System Prompt' heading, or when the
heading body is blank (absent or whitespace-only) — a blank body is
treated as \"not authored\" so it never overrides the preset
(register/invariant/system-prompt-heading-authoritative).

Delegates the heading-and-drawer scan to
`gptel-chat--system-prompt-heading-body-region'; the only work here is
substring extraction, the trim policy, and the blank-body check."
  (when-let* ((region (gptel-chat--system-prompt-heading-body-region))
              (body-start (car region))
              (subtree-end (cdr region))
              ((< body-start subtree-end))
              (body (buffer-substring-no-properties body-start subtree-end))
              ((not (string-blank-p body))))
    (string-trim-right body)))

(defun gptel-chat--apply-system-prompt-heading ()
  "Install the `* System Prompt' heading body as `gptel--system-message'.

When the current buffer carries a `* System Prompt' heading with a
non-blank body, set `gptel--system-message' buffer-locally to that
body — the document is the authoritative source of the system prompt
(register/invariant/system-prompt-heading-authoritative).

A no-op when the heading is absent or its body is blank: the value
installed by an earlier step — the preset's `:system' or a legacy
`:GPTEL_SYSTEM:' drawer entry applied by
`gptel-chat--apply-drawer-overrides' — is left in effect.  An empty
heading never silently wipes the prompt.

Intended to run from `gptel-chat--apply-declared-preset' after the
drawer overlay, so the heading body wins over both the preset and a
legacy drawer entry (design.md §Addendum, Decision B)."
  (when-let* ((body (gptel-chat--system-prompt-heading-body)))
    (set (make-local-variable 'gptel--system-message) body)))
;; System Prompt heading restore:1 ends here

;; Preset application hook

;; =gptel-chat--apply-declared-preset= is the hook entry point. It
;; looks up the declared preset and, if one is found, applies it with a
;; buffer-local setter matching the canonical pattern in
;; =config/gptel/tools/persistent-agent.org:646-650=.

;; The setter =(lambda (sym val) (set (make-local-variable sym) val))=
;; is critical: the default (global =set=) would leak preset-configured
;; values into other buffers (design.md §Decision 16). The buffer-local
;; setter installs =:backend=, =:model=, =:system=, =:tools=,
;; =:temperature=, etc. as buffer-local values; subsequent
;; =gptel-request= calls read them directly.

;; This function does *not* enable =gptel-mode= (design.md §Decision
;; 16). Chat-mode owns the major-mode role exclusively; running
;; =gptel-mode= on top would produce a mixed-format file that neither
;; parser can read cleanly.

;; If no preset is declared, the function is a no-op — the buffer
;; inherits whatever global or dir-local configuration is in effect.

;; =ignore-errors= around =gptel--apply-preset= mirrors upstream's
;; tolerance in =gptel-org--restore-state= (=gptel-org.el:587-588=): a
;; preset name that no longer resolves should warn but not break mode
;; activation.

;; After preset application, the overlay
;; =gptel-chat--apply-drawer-overrides= is invoked so any non-preset
;; drawer properties (tools, model, tokens, etc.) and the chat-mode
;; extension =GPTEL_PARENT_SESSION_ID= are restored on top of the
;; preset's baseline. The overlay also fires in the no-preset branch —
;; that covers buffers whose drawer carries only non-preset keys or
;; only =GPTEL_PARENT_SESSION_ID= (design.md §Decisions 2, 3, 5).

;; Finally =gptel-chat--apply-system-prompt-heading= runs *last*, after
;; the overlay, so an authored =* System Prompt= heading body supersedes
;; both the preset's =:system= and a legacy =:GPTEL_SYSTEM:= drawer
;; entry — the document is authoritative
;; (=register/invariant/system-prompt-heading-authoritative=, design.md
;; §Addendum Decision B). It is a no-op when the heading is absent or
;; blank, so old sessions degrade gracefully to the preset.


;; [[file:menu.org::*Preset application hook][Preset application hook:1]]
(defun gptel-chat--apply-declared-preset ()
  "Apply a buffer-declared gptel preset, if any, then overlay deltas.

Intended to run from `gptel-chat-mode-hook'.  Looks up the declared
preset via `gptel-chat--declared-preset'; when one is found, calls
`gptel--apply-preset' with a buffer-local setter so the preset's
keys (`:backend', `:model', `:system', `:tools', `:temperature', ...)
are installed as buffer-local values.

After preset application (or unconditionally when no preset is
declared), calls `gptel-chat--apply-drawer-overrides' to install any
non-preset drawer properties and `GPTEL_PARENT_SESSION_ID' as
buffer-local bindings on top of the preset baseline (design.md
§Decisions 2, 3, 5).

Finally calls `gptel-chat--apply-system-prompt-heading' so an
authored `* System Prompt' heading body wins over the preset's
`:system' and over any legacy `:GPTEL_SYSTEM:' drawer entry — the
restore precedence is heading body > drawer entry > preset
(register/invariant/system-prompt-heading-authoritative, design.md
§Addendum Decision B).  The heading read is a no-op when no `* System
Prompt' heading exists or its body is blank, so pre-Addendum sessions
fall back to the preset.

Does NOT enable `gptel-mode' (design.md §Decision 16).  Chat-mode
owns the major-mode role exclusively.

A preset name that does not resolve (`gptel-get-preset' returns nil)
triggers a `display-warning' rather than an error, matching upstream
`gptel-org--restore-state' behaviour."
  (when-let* ((preset (gptel-chat--declared-preset)))
    (if (and (fboundp 'gptel-get-preset) (gptel-get-preset preset))
        (progn
          (gptel--apply-preset
           preset
           (lambda (sym val) (set (make-local-variable sym) val)))
          (gptel-chat--apply-drawer-overrides))
      (display-warning
       '(gptel-chat presets)
       (format "Could not activate gptel preset `%s' in buffer \"%s\""
               preset (buffer-name)))))
  ;; Always run the overlay — covers no-preset buffers whose drawer
  ;; carries only non-preset keys (e.g. `GPTEL_TOOLS' alone) or only
  ;; `GPTEL_PARENT_SESSION_ID'.  Re-running after the preset-apply
  ;; branch is harmless: the overlay re-installs the same values.
  (gptel-chat--apply-drawer-overrides)
  ;; Heading body is authoritative — runs last so it supersedes both
  ;; the preset's `:system' and a legacy `:GPTEL_SYSTEM:' drawer entry
  ;; (register/invariant/system-prompt-heading-authoritative).
  (gptel-chat--apply-system-prompt-heading))
;; Preset application hook:1 ends here

;; Save state hook

;; =gptel-chat--save-state= is the =before-save-hook= entry that
;; writes a configuration =:PROPERTIES:= drawer at =point-min= before
;; the buffer is written to disk.

;; The save path writes the *full snapshot* of current buffer-local
;; configuration on every save, with no delta-from-preset deletion
;; semantics. The drawer is the WYSIWYG source of truth — opening
;; the file shows the configuration the chat is using right now
;; (Decision 1 of gptel-drawer-as-source-of-truth).

;; The six *snapshot keys* — =GPTEL_MODEL=, =GPTEL_BACKEND=,
;; =GPTEL_TOOLS=, =GPTEL_TEMPERATURE=, =GPTEL_MAX_TOKENS=, and
;; =GPTEL_NUM_MESSAGES_TO_SEND= — are emitted through the shared
;; registry =jf/gptel-scope-profile--snapshot-spec=. The writer reads
;; the current buffer-local configuration via
;; =jf/gptel-scope-profile--buffer-snapshot-plist=, turns it into
;; ordered, type-filtered entries with
;; =jf/gptel-scope-profile--snapshot-entries=, and for each registry
;; key either writes the entry or deletes a stale one. This is the
;; *same* enumeration the preset-spec renderer and the buffer-mode
;; applicator consume, so the snapshot key set cannot drift between the
;; save path and session creation, and adding a seventh key is a single
;; edit to the registry.

;; Two further keys are chat-mode-specific and written directly:

;; - =GPTEL_PRESET= — from =gptel--preset= (the preset *name*, not
;;   preset-derived configuration, so not part of the snapshot set).
;; - =GPTEL_PARENT_SESSION_ID= — from =jf/gptel--parent-session-id=
;;   (written by =gptel-chat--save-state=, see below).

;; Deliberate exclusions:

;; - =GPTEL_SYSTEM= is *never* written (Decision 2;
;;   =register/invariant/drawer-system-key-write-exclusion=). Long,
;;   multi-line, special-character system prompts are unwieldy as a
;;   single org property value, so the writer never round-trips them.
;;   The read-side overlay (=gptel-chat--apply-drawer-overrides=)
;;   still respects manually authored entries for back-compat.
;; - =GPTEL_BOUNDS= is never written. Chat-mode stores response text
;;   inside =#+begin_assistant= blocks rather than text-property-
;;   bounded spans (spec §"Save path never writes GPTEL_BOUNDS").

;; For each key, if the source variable is nil or wrong-typed, the
;; drawer entry is *deleted* via =org-entry-delete= so the saved
;; drawer never carries stale values. =GPTEL_SYSTEM= is the
;; exception — it is neither written nor deleted, leaving any user-
;; authored entry as-is.

;; Design rationale (design.md §Decisions 1, 2, 3):

;; - A dedicated chat-mode writer (rather than wrapping the upstream
;;   =gptel-org-set-properties= helper) is the smallest surface that
;;   gives the WYSIWYG contract. The upstream helper's delta-from-
;;   preset semantics defeat the design goal — fresh sessions never
;;   display =:GPTEL_TOOLS:= or =:GPTEL_MODEL:= when the buffer-local
;;   values match the preset, hiding the active configuration.
;; - The snapshot keys route through the shared registry
;;   =jf/gptel-scope-profile--snapshot-spec= rather than being
;;   enumerated inline. Before this, the writer was a third independent
;;   producer of the same six-key set, alongside the preset-spec
;;   renderer and the buffer-mode applicator; a reviewer flagged that
;;   adding a snapshot key meant editing the writer *and* the scope-
;;   profile module. Routing every producer through one registry
;;   retires that drift surface
;;   (=harden-snapshot-emission-cross-stage-parity=, Finding 1). The
;;   writer lazily =(require 'gptel-scope-profiles)= for the registry;
;;   in production that module is already loaded by =gptel.org=.
;; - =(require 'gptel-org)= is still needed: the read-side overlay
;;   (=gptel-chat--apply-drawer-overrides=) consumes
;;   =gptel-org--entry-properties=, so the feature must be loaded.
;; - =save-excursion= + =org-with-wide-buffer= mirror upstream's
;;   framing so the save is safe regardless of buffer narrowing or
;;   point position.
;; - The =derived-mode-p= guard is defense-in-depth. The hook is
;;   registered buffer-locally from
;;   =gptel-chat--install-preset-hooks= (see below), so in practice
;;   it only fires in chat-mode buffers, but a direct caller — or a
;;   future accidental global registration — should still no-op
;;   outside chat-mode.


;; [[file:menu.org::*Save state hook][Save state hook:1]]
(declare-function org-entry-delete "org" (pom property))
(declare-function org-entry-put-multivalued-property "org"
                  (pom property &rest values))
(declare-function jf/gptel-scope-profile--snapshot-entries
                  "gptel-scope-profiles" (snapshot-plist))
(declare-function jf/gptel-scope-profile--snapshot-keys
                  "gptel-scope-profiles" ())
(declare-function jf/gptel-scope-profile--buffer-snapshot-plist
                  "gptel-scope-profiles" ())
(defvar gptel--preset)
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel-temperature)
(defvar gptel-max-tokens)
(defvar gptel--num-messages-to-send)

(defun gptel-chat--put-or-delete (pt key value)
  "Write VALUE under KEY at PT via `org-entry-put', or delete the entry.

When VALUE is a non-empty string, write it under property name KEY
(KEY is the bare property name, no surrounding colons).  When VALUE
is nil or empty, delete the property entry so the drawer never
carries stale values across saves.  Used by
`gptel-chat--write-config-drawer' for scalar keys."
  (if (and (stringp value) (not (string-empty-p value)))
      (org-entry-put pt key value)
    (org-entry-delete pt key)))

(defun gptel-chat--write-config-drawer ()
  "Write the chat-mode configuration drawer at `point-min'.

Emits the full upstream-compatible drawer key set from current
buffer-local state (no delta-from-preset deletion).  See the
section commentary for the full key list and exclusions
(`:GPTEL_SYSTEM:' and `:GPTEL_BOUNDS:' are deliberately never
written; Decision 2 of gptel-drawer-as-source-of-truth, register/
invariant/drawer-system-key-write-exclusion).

The six snapshot keys (`:GPTEL_MODEL:', `:GPTEL_BACKEND:',
`:GPTEL_TOOLS:', `:GPTEL_TEMPERATURE:', `:GPTEL_MAX_TOKENS:',
`:GPTEL_NUM_MESSAGES_TO_SEND:') are sourced through the shared
registry `jf/gptel-scope-profile--snapshot-spec': current buffer-
local state is read by `jf/gptel-scope-profile--buffer-snapshot-plist'
and ordered by `jf/gptel-scope-profile--snapshot-entries' — the same
enumeration the preset-spec renderer and the buffer-mode applicator
consume, so the snapshot key set cannot drift between producers
(`harden-snapshot-emission-cross-stage-parity', Finding 1).  For each
registry key the present entry is written, or a stale one deleted via
`org-entry-delete', so the saved drawer reflects current buffer state
precisely.  Multi-valued keys (`:GPTEL_TOOLS:') are written via
`org-entry-put-multivalued-property'.

`:GPTEL_PRESET:' is written directly: it is the preset *name*, not
preset-derived configuration, and is not part of the snapshot set.

Caller is responsible for the surrounding `save-excursion' /
`org-with-wide-buffer' framing and for calling this only in
chat-mode buffers.  The dedicated writer (rather than upstream
`gptel-org-set-properties') is Decision 1 of
gptel-drawer-as-source-of-truth: the WYSIWYG contract requires
full-snapshot writes, not deletes-when-preset-matches."
  (require 'gptel-scope-profiles)
  (let* ((pt (point-min))
         (entries (jf/gptel-scope-profile--snapshot-entries
                   (jf/gptel-scope-profile--buffer-snapshot-plist))))
    ;; :GPTEL_PRESET: — the preset name, not a snapshot key.
    (gptel-chat--put-or-delete
     pt "GPTEL_PRESET"
     (and (bound-and-true-p gptel--preset)
          (cond ((symbolp gptel--preset) (symbol-name gptel--preset))
                ((stringp gptel--preset) gptel--preset))))
    ;; Snapshot keys: single-sourced through
    ;; `jf/gptel-scope-profile--snapshot-spec'.  For each canonical
    ;; key write the present entry (dispatching on KIND exactly like
    ;; `jf/gptel-scope-profile--apply-to-drawer') or delete a stale
    ;; one.  No inline key enumeration lives here — adding a seventh
    ;; snapshot key is a single edit to the registry.
    (dolist (key (jf/gptel-scope-profile--snapshot-keys))
      (if-let* ((entry (assoc key entries)))
          (pcase-let ((`(,_key ,value ,kind) entry))
            (pcase kind
              (:scalar
               (org-entry-put pt key value))
              (:multivalued
               (apply #'org-entry-put-multivalued-property
                      pt key value))))
        (org-entry-delete pt key)))
    ;; :GPTEL_SYSTEM: deliberately not touched.  Writers never emit
    ;; it (Decision 2; long multi-line strings are unwieldy as
    ;; org property values).  Reads still honor manually authored
    ;; entries via the chat-mode overlay for back-compat.
    ;; :GPTEL_BOUNDS: deliberately not touched.  Chat-mode uses
    ;; block-based response storage, not text-property bounds.
    nil))
;; Save state hook:1 ends here

;; System Prompt heading save

;; The save-side counterpart to =gptel-chat--apply-system-prompt-heading=:
;; on =before-save-hook=, after the config drawer is written, the
;; current buffer-local =gptel--system-message= is serialised back into
;; the =* System Prompt= heading body. The document is the authoritative
;; source of the system prompt
;; (=register/invariant/system-prompt-heading-authoritative=).

;; Two cases:

;; - *Heading present* (a current-layout session) — the body between the
;;   heading's =:PROPERTIES:= drawer and the next heading is replaced
;;   with the current =gptel--system-message=. The body region is located
;;   via =gptel-chat--system-prompt-heading-body-region= — the *same*
;;   helper the reader uses, so reader and writer agree on what "body"
;;   means (the four-step anchor/heading-end/subtree-end/drawer-skip scan
;;   exists once). The writer then pads with the leading/trailing
;;   newlines the layout requires; the reader trims. Single-sourcing the
;;   scan removes a documented duplication finding (arch-cycle-
;;   1779477564-5) and keeps the trim/pad asymmetry visible at one
;;   decision point.
;; - *Heading absent* (a pre-Addendum session — graceful degrade,
;;   Decision 6 / Decision B) — the =* System Prompt= and =* Chat=
;;   headings are *materialised*. Everything currently after the config
;;   drawer (the existing =#+begin_user= / =#+begin_assistant= turn
;;   blocks) is moved under a freshly inserted =* Chat= heading; the
;;   =* System Prompt= heading carrying =:VISIBILITY: folded= is inserted
;;   before it with the system-prompt body. The heading shape is built
;;   with =jf/gptel--session-headings-block= — the *same* helper the
;;   session-creation renderer uses, so the heading literals and the
;;   =:VISIBILITY: folded= property are single-sourced
;;   (=register/shape/session-document-layout=).

;; This *never* writes a =:GPTEL_SYSTEM:= drawer line — the drawer
;; write-exclusion (=register/invariant/drawer-system-key-write-
;; exclusion=) stands. That invariant governs the *drawer*; this code
;; writes the *heading body*. The two compose without conflict.

;; The materialise path lazily =(require 'gptel-session-commands)= for
;; the heading helper; in production that module is loaded by
;; =gptel.org=. The require is wrapped in =(... nil t)= so a chat-mode
;; buffer saved in a minimal context with the sessions stack absent
;; falls back to skipping materialisation rather than erroring — the
;; worst case is an old session keeps its pre-Addendum layout for one
;; more save, never a broken file.


;; [[file:menu.org::*System Prompt heading save][System Prompt heading save:1]]
(declare-function jf/gptel--session-headings-block
                  "gptel-session-commands" (system-prompt user-block))

(defun gptel-chat--config-drawer-end ()
  "Return the buffer position just after the config drawer's `:END:'.

Assumes the file-level `:PROPERTIES:' drawer is at `point-min' (the
canonical session layout — `gptel-chat--write-config-drawer' has
already run by the time this is called on the save path).  Returns
the position at the start of the line following `:END:', or nil when
no drawer is found."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")
        (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
          (forward-line 1)
          (point))))))

(defun gptel-chat--write-system-prompt-heading ()
  "Serialise `gptel--system-message' into the `* System Prompt' body.

When the buffer already has a `* System Prompt' heading, replace its
body text (between the heading's own `:PROPERTIES:' drawer and the
next heading) with the current buffer-local `gptel--system-message'.
The body region is located via
`gptel-chat--system-prompt-heading-body-region', the shared
reader/writer scan helper — guarantees reader and writer agree on
where \"body\" starts and ends (register/shape/session-document-
layout).  The writer pads with the leading/trailing newlines the
layout requires; the reader trims.

When the buffer has no `* System Prompt' heading — a pre-Addendum
session — materialise the `* System Prompt' and `* Chat' headings
after the config drawer using `jf/gptel--session-headings-block', the
shared heading-shape helper (register/shape/session-document-layout).
Existing turn blocks below the config drawer are moved under the new
`* Chat' heading.

Never writes a `:GPTEL_SYSTEM:' drawer line — composes with
register/invariant/drawer-system-key-write-exclusion (that invariant
governs the drawer; this writes the heading body).

Caller supplies the `save-excursion' / `org-with-wide-buffer' framing
and calls this only in chat-mode buffers."
  (let ((system (and (bound-and-true-p gptel--system-message)
                     (stringp gptel--system-message)
                     gptel--system-message)))
    (save-excursion
      (save-restriction
        (widen)
        (if-let* ((region (gptel-chat--system-prompt-heading-body-region)))
            ;; Heading present — replace the body in place.  The helper
            ;; returns the post-drawer region verbatim; the writer pads
            ;; with the leading/trailing newlines required by
            ;; register/shape/session-document-layout.
            (let ((body-start (car region))
                  (subtree-end (cdr region)))
              (delete-region body-start subtree-end)
              (goto-char body-start)
              ;; A non-blank body is written verbatim followed by a
              ;; blank line separating it from `* Chat'; a blank/nil
              ;; system prompt leaves only the separating blank line.
              (insert (if (and system (not (string-blank-p system)))
                          (concat (string-trim-right system) "\n\n")
                        "\n")))
          ;; Heading absent — materialise the canonical headings.
          (when (require 'gptel-session-commands nil t)
            (when-let* ((drawer-end (gptel-chat--config-drawer-end)))
              (let ((user-block (buffer-substring-no-properties
                                 drawer-end (point-max))))
                (delete-region drawer-end (point-max))
                (goto-char drawer-end)
                ;; A blank line separates the config drawer from the
                ;; `* System Prompt' heading.
                (unless (eq drawer-end (point-min))
                  (insert "\n"))
                (insert (jf/gptel--session-headings-block
                         system user-block))))))))))

(defun gptel-chat--save-state ()
  "Write the chat-mode configuration drawer before saving the buffer.

Intended to run from a buffer-local `before-save-hook' installed by
`gptel-chat--install-preset-hooks'.  Delegates to
`gptel-chat--write-config-drawer' for the upstream-compatible key set
(no delta-from-preset deletion — Decision 1 of gptel-drawer-as-
source-of-truth: full-snapshot writes for the WYSIWYG drawer).  Then
writes the chat-mode extension `GPTEL_PARENT_SESSION_ID' via
`org-entry-put' when `jf/gptel--parent-session-id' is bound to a
non-empty string.

Does NOT write `GPTEL_BOUNDS'.  Chat-mode stores response text
inside `#+begin_assistant' blocks rather than text-property-bounded
spans, so bounds persistence is intentionally excluded.

Does NOT write a `:GPTEL_SYSTEM:' drawer line.  Decision 2: long,
multi-line system prompts are unwieldy as a single org property
value; the writer never round-trips them as a drawer property.  The
system prompt is instead serialised into the `* System Prompt'
heading body by `gptel-chat--write-system-prompt-heading' (design.md
§Addendum Decision B) — the drawer write-exclusion
(register/invariant/drawer-system-key-write-exclusion) and the
heading-body write compose without conflict.

Does NOT enable `gptel-mode'.  Chat-mode owns the major-mode role
exclusively (design.md §Decision 16)."
  (when (derived-mode-p 'gptel-chat-mode)
    ;; `gptel-org' is not pulled in by anything else on the chat-mode
    ;; load path — chat-mode derives from `org-mode', not from any
    ;; gptel feature path that upstream `(require 'gptel-org)`s.  Pull
    ;; it in lazily on first save so the read-side overlay (which
    ;; consumes `gptel-org--entry-properties') has the feature
    ;; available (regression caught by `save-hook-require-gptel-org' /
    ;; `regression-sweep-and-manual-smoke', 2026-04-25).
    (require 'gptel-org)
    (save-excursion
      (org-with-wide-buffer
       (gptel-chat--write-config-drawer)
       (when (and (bound-and-true-p jf/gptel--parent-session-id)
                  (stringp jf/gptel--parent-session-id)
                  (not (string-empty-p jf/gptel--parent-session-id)))
         (org-entry-put (point-min)
                        "GPTEL_PARENT_SESSION_ID"
                        jf/gptel--parent-session-id))
       ;; Serialise the system prompt into the `* System Prompt'
       ;; heading body — runs after the drawer write so it acts on the
       ;; canonical layout (register/invariant/system-prompt-heading-
       ;; authoritative).  Never emits a `:GPTEL_SYSTEM:' drawer line.
       (gptel-chat--write-system-prompt-heading)))))
;; System Prompt heading save:1 ends here

;; Hook wiring

;; Preset detection runs at mode activation via
;; =gptel-chat-mode-hook=. This catches the common cases:

;; - =M-x gptel-chat-mode= on an existing buffer with a property drawer.
;; - =gptel-chat-new= (no preset declared; the hook is a no-op).
;; - File opened with =-*- gptel-chat -*-= and a property drawer at
;;   point-min.

;; A second hook on =hack-local-variables-hook= handles the file-local
;; =gptel--preset= case. In the =normal-mode= sequence (files.el), the
;; major mode is activated first (firing =gptel-chat-mode-hook=), then
;; =hack-local-variables= runs the full local-variables pass (firing
;; =hack-local-variables-hook=). So a file-local =gptel--preset=
;; declaration is not yet bound when =gptel-chat-mode-hook= runs on a
;; fresh file open, and we need a second hook that runs after locals
;; land.

;; *Registration scope (task =preset-wiring-robustness=, Finding 1).*
;; We add the =hack-local-variables-hook= entry *buffer-locally from
;; within the mode-hook handler* — not at module top level. Global
;; registration would invoke =derived-mode-p= in every buffer Emacs ever
;; opens, regardless of whether the user ever interacts with chat-mode.
;; Registering buffer-locally from the mode hook means the registration
;; only happens in chat-mode buffers, and the post-locals re-apply still
;; fires because the mode hook is invoked before the full
;; =hack-local-variables= pass.

;; Calling =gptel-chat--apply-declared-preset= twice is safe —
;; =gptel--apply-preset= simply re-installs the same buffer-local
;; values. The post-locals invocation is load-bearing for the
;; =gptel--preset= file-local path (drawer-only buffers see a redundant
;; but harmless second apply; =gptel--preset=-only buffers see their
;; one and only apply from the post-locals path).


;; [[file:menu.org::*Hook wiring][Hook wiring:1]]
(defun gptel-chat--apply-declared-preset-after-locals ()
  "Re-apply the declared preset after `hack-local-variables' runs.

Registered buffer-locally on `hack-local-variables-hook' from the
`gptel-chat-mode-hook' entry, so this only fires in chat-mode
buffers.  Catches the file-local `gptel--preset' case — the mode-
hook entry alone cannot, because the `normal-mode' sequence runs
mode hooks before the full `hack-local-variables' pass.

The `derived-mode-p' guard is defence-in-depth: the buffer-local
registration already scopes this to chat-mode buffers, but a direct
caller (or a future global add-hook by mistake) should still no-op
outside chat-mode."
  (when (derived-mode-p 'gptel-chat-mode)
    (gptel-chat--apply-declared-preset)))

(defun gptel-chat--install-preset-hooks ()
  "Install buffer-local preset and save-state hooks for `gptel-chat-mode'.

Called from `gptel-chat-mode-hook'.  Applies any preset declared by
the buffer's property drawer immediately, registers a buffer-local
entry on `hack-local-variables-hook' so file-local `gptel--preset'
declarations are picked up after Emacs's full local-variables pass,
and registers a buffer-local entry on `before-save-hook' so every
save writes the configuration drawer via `gptel-chat--save-state'.

Registering hook entries buffer-locally (rather than at module top
level) avoids imposing a `derived-mode-p' check in every unrelated
buffer Emacs ever opens (task `preset-wiring-robustness', Finding 1;
design.md §Decision 10)."
  (gptel-chat--apply-declared-preset)
  (add-hook 'hack-local-variables-hook
            #'gptel-chat--apply-declared-preset-after-locals
            nil t)
  (add-hook 'before-save-hook
            #'gptel-chat--save-state
            nil t))

(add-hook 'gptel-chat-mode-hook #'gptel-chat--install-preset-hooks)
;; Hook wiring:1 ends here

;; Send suffix

;; =gptel-chat--suffix-send= is the narrow replacement for
;; =gptel--suffix-send=. Its only job is to call =gptel-chat-send= —
;; chat-mode's send command already encapsulates argument handling,
;; prompt construction, streaming, and FSM setup (see
;; =config/gptel/chat/send.org=). We do *not* pass =transient-args=
;; through to it: chat-mode's send takes no arguments and always reads
;; the current buffer as its source of truth.

;; The =:key "RET"= and =:description= options mirror upstream so the
;; rebound suffix lands in the same visual slot the user associates
;; with Send (architecture.md §=gptel-chat-menu=).

;; =gptel-chat-send= is loaded by a sibling module; we reference it at
;; call time inside the suffix body, so this module does not require
;; =send= to be loaded yet. =declare-function= below silences the
;; byte-compiler.


;; [[file:menu.org::*Send suffix][Send suffix:1]]
(declare-function gptel-chat-send "gptel-chat-send" ())

(transient-define-suffix gptel-chat--suffix-send ()
  "Dispatch Send from `gptel-chat-menu' to `gptel-chat-send'.

Replaces `gptel--suffix-send' on the chat-mode transient.  Unlike
upstream Send, this suffix takes no transient arguments — chat-mode
reads everything it needs from the current buffer (design.md
§Decision 15, architecture.md §`gptel-chat-menu')."
  :key "RET"
  :description "Send chat buffer"
  (interactive)
  (call-interactively #'gptel-chat-send))
;; Send suffix:1 ends here

;; Scope default: buffer-local for chat-menu lifetime

;; The transient body below binds upstream's =gptel--set-buffer-locally=
;; (defined in =gptel-transient.el=) to =t= for the lifetime of
;; =gptel-chat-menu=. Without this, "Select tools" and the other
;; configuration infixes default to =gptel--set-buffer-locally = nil=
;; (global scope), which means a tool toggle from the menu in a chat-mode
;; buffer mutates the global =gptel-tools= and kills any buffer-local
;; binding installed by the preset — invisible to the user, and the
;; drawer save then captures the wrong value. Decision 5 of the
;; gptel-drawer-as-source-of-truth design picks "default chat-menu to
;; buffer-local scope" so menu edits round-trip through the drawer.

;; Upstream =gptel-menu= invoked directly (=M-x gptel-menu=) is
;; unaffected — only this chat-mode wrapper changes the default. The
;; user retains the =gptel--infix-variable-scope= toggle inside the
;; menu to switch to global / oneshot per-invocation.

;; Implementation: the prefix body =setq='s the variable on entry and
;; registers a one-shot =transient-post-exit-hook= to restore the prior
;; value on exit. =transient-post-exit-hook= (as opposed to
;; =transient-exit-hook=) fires *only* when no transient is resuming —
;; i.e. only when the outermost prefix is fully done (see =transient.el=
;; =transient--post-exit=, around line 3081). That gives us the
;; "outermost menu fully done" semantics directly, without the
;; =transient-current-prefix= discriminator that the previous
;; implementation used. The discriminator was load-bearingly broken: by
;; the time =transient-exit-hook= fires on a normal commit-style exit,
;; =transient--export= has already set =transient-current-prefix= to the
;; prefix object, so the guard short-circuited and the restore never
;; ran.


;; [[file:menu.org::*Scope default: buffer-local for chat-menu lifetime][Scope default: buffer-local for chat-menu lifetime:1]]
(declare-function transient-setup "transient" (&optional name layout edit &rest params))
(defvar transient-post-exit-hook)
(defvar gptel--set-buffer-locally)

;; A single global defvar is safe under the hook-mechanism fix below:
;; `transient-post-exit-hook' only fires on the outermost prefix's
;; full exit, so single-frame re-entry cannot leak a stale prior
;; through this slot.  The remaining edge case is multi-frame
;; re-entry across recursive minibuffers, which the upstream
;; transient call path does not produce in chat-menu's current shape.
(defvar gptel-chat--scope-prior nil
  "Saved value of `gptel--set-buffer-locally' to restore on chat-menu exit.
Set by `gptel-chat-menu's body when it switches to buffer-local
scope; restored by `gptel-chat--restore-scope-on-exit', a one-shot
`transient-post-exit-hook'.  Nil between invocations.")

(defun gptel-chat--restore-scope-on-exit ()
  "Restore `gptel--set-buffer-locally' to its pre-menu value and self-remove.
Registered on `transient-post-exit-hook' by `gptel-chat-menu's body
when it sets the variable to t.  `transient-post-exit-hook' fires
only when the outermost transient is fully done (no sub-transient
resuming), so this hook is unconditional — no
`transient-current-prefix' discriminator is needed.  Sub-transients
(e.g. `gptel-tools' invoked from inside the chat menu) do not fire
this hook at all."
  (setq gptel--set-buffer-locally gptel-chat--scope-prior
        gptel-chat--scope-prior nil)
  (remove-hook 'transient-post-exit-hook
               #'gptel-chat--restore-scope-on-exit))
;; Scope default: buffer-local for chat-menu lifetime:1 ends here

;; Prefix definition

;; =gptel-chat-menu= mirrors the *configuration* portion of upstream
;; =gptel-menu= (=runtime/straight/repos/gptel/gptel-transient.el=: the
;; =transient-define-prefix gptel-menu= form) and replaces the final
;; =[(gptel--suffix-send)]= row with =[(gptel-chat--suffix-send)]=. The
;; groups we keep — system-prompt/context/tools at the top,
;; request-parameters (preset, provider, model, max-tokens, temperature,
;; use-context, include-reasoning, use-tools, track-response) in the
;; middle, logging below — are identical by symbol reference.

;; We deliberately omit upstream's Send-coupled groups (the prompt-
;; source selector, the response-destination selector, and the inspect-
;; query suffixes):

;; - The prompt-source selector (Minibuffer / Kill-ring / Respond-in-
;;   place) — its transient-args =m= / =y= / =i= are read by
;;   =gptel--suffix-send= at send time; =gptel-chat--suffix-send= takes
;;   no args, so these toggles would be silently discarded (design.md
;;   §Decision 15, enumerated exclusions).
;; - The response-destination selector (Echo area / Other buffer /
;;   gptel session / Kill-ring) — transient-args =e= / =b= / =g= / =k=
;;   follow the same silent-drop pattern.
;; - The inspect-query suffixes (Lisp / JSON) — they invoke
;;   =(gptel--suffix-send (cons "I" (transient-args ...)))= directly,
;;   so their preview reads upstream's =gptel= text-property bounds
;;   that chat-mode never emits (Decision 18 — block-based session
;;   format). The preview would effectively be "everything from
;;   point-min" for a send path that cannot run here.

;; We also omit the region-rewrite group and the response-history
;; commands. Their guard predicates test for gptel-mode's response-
;; insertion artifacts — rewrite overlays and the =gptel= text-property
;; on response spans. Chat-mode stores response text inside
;; =#+begin_assistant= blocks without those artifacts (Decision 18), so
;; the predicates never match and the groups would only be dead rows in
;; the layout. The region-rewrite command itself also depends on the
;; same response-overlay infrastructure downstream, so even the region-
;; active branch of its guard cannot route to a working command in a
;; chat-mode buffer.

;; The =:incompatible= declaration on the prefix is dropped together
;; with these groups — it only constrained the =m/y/i= and =e/g/b/k=
;; transient-args that no longer exist on this prefix.

;; We deliberately do *not* =require 'gptel-transient= at top level so
;; this module stays load-safe before the transient has initialised.
;; The infix symbols resolve at transient-setup time (when the user
;; invokes the prefix), at which point =gptel-transient= will have been
;; loaded by the upstream autoloads triggered by =gptel-menu= or
;; =gptel-mode=. Should a user somehow reach =gptel-chat-menu= without
;; =gptel-transient= loaded (unlikely in practice), =transient-setup=
;; will surface a clear error; we do not try to pre-empt that path.

;; The prefix body mirrors upstream's: sanitise the active model,
;; prune stale buffer-local context entries, then hand off to
;; =transient-setup=. Keeping the body identical preserves the
;; behavioural contract of the inherited infixes — they expect
;; =gptel-context= to be pre-cleaned and =gptel-model= to be
;; resolvable.


;; [[file:menu.org::*Prefix definition][Prefix definition:1]]
(declare-function gptel--sanitize-model "gptel" (&optional backend model shoosh))
(declare-function gptel-system-prompt--format "gptel-transient" (&optional message))
(declare-function gptel--describe-infix-context "gptel-transient" ())
(declare-function gptel--model-capable-p "gptel" (capability))
(defvar gptel-context)
(defvar gptel-use-tools)
(defvar gptel--known-tools)
(defvar gptel-tools)
(defvar gptel--fsm-last)
(defvar gptel-mode)
(defvar gptel-track-response)
(defvar gptel-expert-commands)
(defvar gptel-log-level)
(defvar gptel-backend)
(defvar gptel--log-buffer-name)

;;;###autoload (autoload 'gptel-chat-menu "gptel-chat-menu" nil t)
(transient-define-prefix gptel-chat-menu ()
  "Change parameters of the chat-mode prompt to send to the LLM.

Mirrors the layout of upstream `gptel-menu' but replaces the Send
suffix with `gptel-chat--suffix-send', which dispatches to
`gptel-chat-send'.  Configuration infixes (system message, context,
tools, preset, provider, temperature, etc.) are reused by symbol
from `gptel-transient.el', so their behaviour is identical to
upstream.

Bound on `gptel-chat-mode-map' (see `mode.org'); also callable via
`M-x gptel-chat-menu'.  Upstream `gptel-menu' remains unchanged —
`M-x gptel-menu' invoked directly retains its upstream Send suffix."
  [:description gptel-system-prompt--format
   [""
    :if (lambda () (not (gptel--model-capable-p 'nosystem)))
    "Instructions"
    ("s" "Set system message" gptel-system-prompt :transient t)
    (gptel--infix-add-directive)]
   [:pad-keys t ""
    (:info #'gptel--describe-infix-context
     :face transient-heading :format "%d")
    (gptel--infix-context-add-current-kill)
    (gptel--infix-context-add-region)
    (gptel--infix-context-add-buffer)
    (gptel--infix-context-add-file)
    (gptel--infix-context-remove-all)
    (gptel--suffix-context-buffer)]
   [:pad-keys t
    :if (lambda () (and gptel-use-tools
                   (or gptel--known-tools (featurep 'gptel-integrations))))
    "" (:info
        (lambda ()
          (concat
           "Tools" (and gptel-tools
                        (concat " (" (propertize (format "%d selected"
                                                         (length gptel-tools))
                                                 'face 'warning)
                                ")"))))
        :format "%d" :face transient-heading)
    ("t" "Select tools" gptel-tools :transient t)
    ("T" "Continue tool calls"
     (lambda () (interactive) (gptel--handle-tool-use gptel--fsm-last))
     :if (lambda () (and gptel--fsm-last
                    (eq (gptel-fsm-state gptel--fsm-last) 'TOOL))))]]
  [[(gptel--preset
     :key "@" :format "%d"
     :description
     (lambda ()
       (concat (propertize "Request Parameters" 'face 'transient-heading)
               (gptel--format-preset-string))))
    (gptel--infix-variable-scope)
    (gptel--infix-provider)
    (gptel--infix-max-tokens)
    (gptel--infix-num-messages-to-send
     :if (lambda () (and gptel-expert-commands
                    (or gptel-mode gptel-track-response))))
    (gptel--infix-temperature :if (lambda () gptel-expert-commands))
    (gptel--infix-use-context)
    (gptel--infix-include-reasoning)
    (gptel--infix-use-tools)
    (gptel--infix-track-response
     :if (lambda () (and gptel-expert-commands (not gptel-mode))))
    (gptel--infix-track-media :if (lambda () gptel-mode))]]
  [["Logging"
    :if (lambda () (or gptel-log-level gptel-expert-commands))
    ("-l" "Log level" "-l"
     :class gptel-lisp-variable
     :variable gptel-log-level
     :set-value gptel--set-with-scope
     :display-nil "Off"
     :prompt "Log level: "
     :reader
     (lambda (prompt _ _)
       "Manage gptel's logging."
       (let ((state (completing-read
                     prompt '("off" "info" "debug") nil t)))
         (message "Log level set to %s" state)
         (if (string= state "off") nil (intern state)))))
    ("L" "Inspect Log"
     (lambda () (interactive)
       (pop-to-buffer (get-buffer-create gptel--log-buffer-name)))
     :format "  %k %d")]]
  [(gptel-chat--suffix-send)]
  (interactive)
  (gptel--sanitize-model)
  (when gptel-context
    (setq gptel-context
          (cl-delete-if
           (lambda (entry)
             (let ((first (or (car-safe entry) entry)))
               (and (bufferp first) (not (buffer-live-p first)))))
           gptel-context)))
  (setq gptel-chat--scope-prior gptel--set-buffer-locally
        gptel--set-buffer-locally t)
  (add-hook 'transient-post-exit-hook #'gptel-chat--restore-scope-on-exit)
  (transient-setup 'gptel-chat-menu))
;; Prefix definition:1 ends here

;; Provide


;; [[file:menu.org::*Provide][Provide:1]]
(provide 'gptel-chat-menu)

;;; menu.el ends here
;; Provide:1 ends here
