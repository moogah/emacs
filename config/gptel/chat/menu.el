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


;; [[file:menu.org::*Dependencies][Dependencies:1]]
(require 'gptel nil t)
;; Dependencies:1 ends here

;; Forward declarations

;; Suppress byte-compiler warnings for upstream symbols. They resolve at
;; call time once =gptel= has loaded.


;; [[file:menu.org::*Forward declarations][Forward declarations:1]]
(declare-function gptel--apply-preset "gptel" (preset &optional setter))
(declare-function gptel-get-preset "gptel" (name))
(defvar gptel--preset)
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


;; [[file:menu.org::*Preset application hook][Preset application hook:1]]
(defun gptel-chat--apply-declared-preset ()
  "Apply a buffer-declared gptel preset, if any.

Intended to run from `gptel-chat-mode-hook'.  Looks up the declared
preset via `gptel-chat--declared-preset'; when one is found, calls
`gptel--apply-preset' with a buffer-local setter so the preset's
keys (`:backend', `:model', `:system', `:tools', `:temperature', ...)
are installed as buffer-local values.

Does NOT enable `gptel-mode' (design.md §Decision 16).  Chat-mode
owns the major-mode role exclusively.

A preset name that does not resolve (`gptel-get-preset' returns nil)
triggers a `display-warning' rather than an error, matching upstream
`gptel-org--restore-state' behaviour."
  (when-let* ((preset (gptel-chat--declared-preset)))
    (if (and (fboundp 'gptel-get-preset) (gptel-get-preset preset))
        (gptel--apply-preset
         preset
         (lambda (sym val) (set (make-local-variable sym) val)))
      (display-warning
       '(gptel-chat presets)
       (format "Could not activate gptel preset `%s' in buffer \"%s\""
               preset (buffer-name))))))
;; Preset application hook:1 ends here

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
  "Install buffer-local preset hooks for `gptel-chat-mode'.

Called from `gptel-chat-mode-hook'.  Applies any preset declared by
the buffer's property drawer immediately, and registers a buffer-
local entry on `hack-local-variables-hook' so file-local
`gptel--preset' declarations are picked up after Emacs's full
local-variables pass.

Registering the `hack-local-variables-hook' entry buffer-locally
(rather than at module top level) avoids imposing a
`derived-mode-p' check in every unrelated buffer Emacs ever opens
(task `preset-wiring-robustness', Finding 1)."
  (gptel-chat--apply-declared-preset)
  (add-hook 'hack-local-variables-hook
            #'gptel-chat--apply-declared-preset-after-locals
            nil t))

(add-hook 'gptel-chat-mode-hook #'gptel-chat--install-preset-hooks)
;; Hook wiring:1 ends here

;; Provide


;; [[file:menu.org::*Provide][Provide:1]]
;; TODO(gptel-chat-mode): implement `gptel-chat-menu' transient prefix
;; — delivered by the `menu-integration' task.

(provide 'gptel-chat-menu)

;;; menu.el ends here
;; Provide:1 ends here
