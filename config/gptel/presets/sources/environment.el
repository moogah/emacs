;;; environment.el --- Dynamic environment fragment -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: gptel, llm, presets

;;; Commentary:

;; The canonical dynamic fragment: the per-send "# Environment" block,
;; evaluated at compose time and placed at the tail of the chat/agent
;; default composition.  See environment.org for the full design.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(declare-function jf/gptel--scan-session-drawer-keys "gptel-session-filesystem" ())
(declare-function org-entry-get-multivalued-property "org" (pom property))

(defun jf/gptel-fragment-environment--render-scope-line (label patterns)
  "Render one scope bullet: LABEL with PATTERNS comma-joined.
PATTERNS is a list of verbatim glob strings (possibly nil/empty).
An empty list renders as a bare label with no patterns, so the block
shows which buckets the session declares even when one is empty."
  (format "- %s %s" label
          (if patterns (mapconcat #'identity patterns ", ") "")))

(defun jf/gptel-fragment-environment (&optional _context)
  "Return the markdown \"# Environment\" block for the current buffer.

The canonical dynamic fragment function: evaluated at compose time and
placed at the tail of the chat/agent default composition
\(`register/invariant/static-prerender-dynamic-compose').  _CONTEXT is
the compose-time context the composer passes to every dynamic
reference's :fn; it is ignored here because the block's inputs are
INTRINSIC to the buffer (`register/boundary/environment-block-input-
neutrality').

Built from INTRINSIC buffer inputs only (design.md D3):
`default-directory' for the working-directory line, and the point-min
`:PROPERTIES:' drawer's raw `GPTEL_SCOPE_READ' / `GPTEL_SCOPE_WRITE' /
`GPTEL_SCOPE_DENY' patterns for the scope lines.  Scope patterns are
rendered VERBATIM (comma-joined, as authored) — the function reads the
drawer directly via `org-entry-get-multivalued-property' rather than
through `jf/gptel-scope--load-config', whose deny-all defaulting is for
validation, not human display.

Degradation (design.md D5): when the drawer carries NONE of the three
rendered `GPTEL_SCOPE_*' keys, the three scope bullets collapse to a
single \"no scope restrictions\" line.  The function NEVER signals and
NEVER returns an empty string.

References NO workspaces-package symbol (`register/boundary/
environment-block-input-neutrality'): with the workspaces package
unloaded the function still returns a valid block."
  (require 'org)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let* ((keys (jf/gptel--scan-session-drawer-keys))
             (scoped (seq-find
                      (lambda (k)
                        (member (car k)
                                '("GPTEL_SCOPE_READ"
                                  "GPTEL_SCOPE_WRITE"
                                  "GPTEL_SCOPE_DENY")))
                      keys))
             (header
              (concat
               "# Environment\n"
               "You are working in the directory below. The file-access scope lists what you may\n"
               "read and write; this information is current as of this message.\n"
               "\n"
               (format "- Working directory: %s\n" default-directory))))
        (if scoped
            (concat
             header
             (jf/gptel-fragment-environment--render-scope-line
              "Readable:"
              (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_READ"))
             "\n"
             (jf/gptel-fragment-environment--render-scope-line
              "Writable:"
              (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_WRITE"))
             "\n"
             (jf/gptel-fragment-environment--render-scope-line
              "Denied:  "
              (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_DENY")))
          (concat
           header
           "- File access: no scope restrictions (this buffer is not a scoped session)"))))))

(require 'jf-gptel-fragments)
(setq jf/gptel-fragment-environment-fn #'jf/gptel-fragment-environment)

(provide 'jf-gptel-fragment-environment)
;;; environment.el ends here
