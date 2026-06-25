;;; fragments.el --- Fragment renderer/parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: gptel, llm, presets

;;; Commentary:

;; Fragment data model and backend-parametrized renderer for the
;; prompt-fragments capability.  See fragments.org for the full design.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst jf/gptel-fragment-kinds '(static dynamic)
  "Closed set of fragment kinds.
A fragment is either `static' (its rendered text is fixed at tangle
time) or `dynamic' (its text is produced by a function at compose
time).  Any source declaring a kind outside this set is malformed.")

(defconst jf/gptel-fragment-default-kind 'static
  "Kind assigned to a fragment whose source declares no kind.")

(defun jf/gptel-fragment--section-name-to-tag (name)
  "Return the snake_case tag derived from section NAME.
Downcase NAME and replace runs of whitespace with a single
underscore, e.g. \"Output Format\" -> \"output_format\".  Leading and
trailing whitespace is trimmed first so it does not produce stray
underscores."
  (let ((trimmed (string-trim (or name ""))))
    (replace-regexp-in-string "[ \t]+" "_" (downcase trimmed))))

(defun jf/gptel-fragment--read-kind (source)
  "Return the fragment kind declared in SOURCE, a string.
Looks for a `#+fragment_kind: KIND' keyword (case-insensitive on the
keyword, on the value).  Returns the matching symbol from
`jf/gptel-fragment-kinds', or `jf/gptel-fragment-default-kind' when no
recognized kind is declared."
  (if (string-match
       (rx bol (* (in " \t")) "#+fragment_kind:" (* (in " \t"))
           (group (+ (not (in " \t\n")))))
       source)
      (let ((declared (intern (downcase (match-string 1 source)))))
        (if (memq declared jf/gptel-fragment-kinds)
            declared
          jf/gptel-fragment-default-kind))
    jf/gptel-fragment-default-kind))

(defun jf/gptel-fragment--parse-sections (source)
  "Parse SOURCE (an Org string) into an ordered list of sections.
Each section is a cons cell (NAME . BODY): NAME is the text of a
top-level (single-star) Org heading; BODY is the whitespace-trimmed
content beneath it, up to the next top-level heading or end of input.
Returns the sections in source order.  Content before the first
heading is ignored."
  (let ((heading-re (rx bol "*" (+ (in " \t")) (group (* nonl)) eol))
        (sections '())
        (case-fold-search nil))
    (with-temp-buffer
      (insert source)
      (goto-char (point-min))
      (while (re-search-forward heading-re nil t)
        (let* ((name (string-trim (match-string 1)))
               (body-start (min (1+ (line-end-position)) (point-max)))
               (body-end (if (re-search-forward heading-re nil t)
                             (line-beginning-position)
                           (point-max))))
          (goto-char body-start)
          (let ((body (string-trim
                       (buffer-substring-no-properties
                        (min body-start body-end) body-end))))
            (push (cons name body) sections))
          (goto-char body-end))))
    (nreverse sections)))

(defun jf/gptel-fragment--parse-source (source)
  "Parse SOURCE into a fragment plist.
SOURCE is an Org source string, or the path of an `.org' file (read
verbatim).  Returns a plist (:kind SYMBOL :sections ((name . body) ...))
where :kind is one of `jf/gptel-fragment-kinds' (default
`jf/gptel-fragment-default-kind') and :sections is the ordered section
list from `jf/gptel-fragment--parse-sections'.

This function is batch-loadable: it performs no interactive Org work
and starts no major mode, so it is safe to call at tangle time when
static fragments are pre-rendered."
  (let ((text (if (and (stringp source)
                       (not (string-match-p "\n" source))
                       (file-readable-p source))
                  (with-temp-buffer
                    (insert-file-contents source)
                    (buffer-string))
                source)))
    (list :kind (jf/gptel-fragment--read-kind text)
          :sections (jf/gptel-fragment--parse-sections text))))

(defun jf/gptel-fragment--render-section-claude (section)
  "Render SECTION as a Claude-style XML block.
SECTION is a cons (NAME . BODY).  Returns \"<tag>\\nBODY\\n</tag>\"
where tag is `jf/gptel-fragment--section-name-to-tag' of NAME."
  (let ((tag (jf/gptel-fragment--section-name-to-tag (car section)))
        (body (cdr section)))
    (format "<%s>\n%s\n</%s>" tag body tag)))

(defun jf/gptel-fragment--render-claude (fragment)
  "Render FRAGMENT for the `claude' backend.
Renders each section with `jf/gptel-fragment--render-section-claude'
and joins them with a blank line, preserving section order."
  (mapconcat #'jf/gptel-fragment--render-section-claude
             (plist-get fragment :sections)
             "\n\n"))

(defun jf/gptel-fragment-render (fragment backend)
  "Render FRAGMENT for BACKEND and return the rendered string.
FRAGMENT is a fragment plist (see `jf/gptel-fragment--parse-source').
BACKEND is a backend symbol; only `claude' is implemented in this
change.

Stages: dispatch on BACKEND -> render each section into a
backend-delimited block -> join the blocks.  For `claude', each
section renders as an XML block (see
`jf/gptel-fragment--render-section-claude').

An unimplemented BACKEND is reported, not silently mis-rendered: this
function logs a warning via `jf/gptel--log' (when available) and then
signals `jf/gptel-fragment-unimplemented-backend'.  It never falls
through to Claude output, and it carries no default text format -- the
output format is whatever the target backend's rendering specifies."
  (pcase backend
    ('claude (jf/gptel-fragment--render-claude fragment))
    (_
     (when (fboundp 'jf/gptel--log)
       (jf/gptel--log 'warn
                      "fragment renderer: no rendering implemented for backend %S"
                      backend))
     (signal 'jf/gptel-fragment-unimplemented-backend (list backend)))))

(define-error 'jf/gptel-fragment-unimplemented-backend
  "No fragment rendering implemented for this backend"
  'error)

(defun jf/gptel-fragment-ref-static (text)
  "Return a static fragment reference wrapping pre-rendered TEXT.
TEXT is consumed verbatim by `jf/gptel-fragment-compose' with no
compose-time rendering."
  (list :kind 'static :text (or text "")))

(defun jf/gptel-fragment-ref-dynamic (fn)
  "Return a dynamic fragment reference wrapping FN.
FN is a function of one argument (the compose-time context).  The
composer calls (funcall FN CONTEXT) at compose time to produce the
reference's text.  Dynamic references default to the tail of a
composition for prompt-cache friendliness."
  (list :kind 'dynamic :fn fn))

(defun jf/gptel-fragment--realize-ref (ref context)
  "Return the per-fragment text for fragment reference REF.
A static REF yields its pre-rendered :text verbatim (no rendering).  A
dynamic REF yields (funcall (plist-get REF :fn) CONTEXT): a nil result
becomes the empty string (so a function like `ignore' contributes
nothing), a string passes through, anything else is coerced with
`format'.  CONTEXT is the compose-time context passed through to
dynamic functions.  Signals `jf/gptel-fragment-bad-reference' for a
reference whose :kind is neither `static' nor `dynamic'."
  (pcase (plist-get ref :kind)
    ('static (or (plist-get ref :text) ""))
    ('dynamic
     (let ((out (funcall (plist-get ref :fn) context)))
       (cond ((null out) "")
             ((stringp out) out)
             (t (format "%s" out)))))
    (_ (signal 'jf/gptel-fragment-bad-reference (list ref)))))

(define-error 'jf/gptel-fragment-bad-reference
  "Fragment reference has an unrecognized :kind"
  'error)

(defun jf/gptel-fragment-compose (composition backend &optional context)
  "Compose COMPOSITION into the effective system message string.
COMPOSITION is an ordered list of fragment references (see
`jf/gptel-fragment-ref-static' / `jf/gptel-fragment-ref-dynamic').
BACKEND is accepted for signature parity with the renderer and forward
compatibility; static references are already pre-rendered for their
backend and dynamic functions own their own formatting, so BACKEND is
not consulted here.  CONTEXT is passed through to each dynamic
reference's function.

Stages (register/boundary/composer-compose):
1. realize-fragments: each reference in order -> per-fragment text.  A
   static reference yields its pre-rendered :text verbatim; a dynamic
   reference yields (funcall :fn CONTEXT).  List order is preserved.
2. join: the per-fragment texts are joined in list order with a blank
   line.  References that realize to empty or whitespace-only text are
   skipped (see the empty-contribution policy), so an absent role
   fragment collapses cleanly without emitting a blank block.

The static prefix of a composition is therefore stable across sends
\(cacheable prefix); the only per-send work is the tail dynamic
reference(s) (invariant `static-prerender-dynamic-compose')."
  (ignore backend)
  (let ((parts '()))
    (dolist (ref composition)
      (let ((text (jf/gptel-fragment--realize-ref ref context)))
        (unless (string-empty-p (string-trim text))
          (push text parts))))
    (string-join (nreverse parts) "\n\n")))

(defvar jf/gptel-fragment-chat-prelude-text ""
  "Pre-rendered static text leading the chat default composition.
The `emacs-prelude' fragment's tangled text.  Wired by the source/
integration tasks; the composer reads it through this seam.")

(defvar jf/gptel-fragment-agent-preamble-text ""
  "Pre-rendered static text leading the agent default composition.
The `agent-preamble' fragment's tangled text.  Wired by the source/
integration tasks; the composer reads it through this seam.")

(defvar jf/gptel-fragment-environment-fn #'ignore
  "Dynamic function producing the trailing `environment' text.
A function of one argument (the compose-time context) returning the
environment-block string.  Defaults to `ignore' (empty contribution,
skipped) until wired by the source/integration tasks.")

(defun jf/gptel-fragment--default-composition (context &optional role-ref)
  "Return the default composition for send CONTEXT.
CONTEXT is `chat' or `agent'.  The returned composition is an ordered
list of fragment references:

  chat  -> [emacs-prelude (static), ROLE-REF, environment (dynamic)]
  agent -> [agent-preamble (static), ROLE-REF, environment (dynamic)]

ROLE-REF is the active preset's role fragment reference occupying the
role position.  When ROLE-REF is nil the role slot collapses to an
empty static contribution (which the compose step skips); the leading
prelude/preamble still leads.  Signals `jf/gptel-fragment-unknown-context'
for a CONTEXT that is neither `chat' nor `agent'.

This is the default; a caller MAY pass an explicit composition to
`jf/gptel-fragment-compose' to override the list (add/remove/reorder).
The trailing `environment' dynamic reference is at the tail by design
\(prompt-cache friendliness); non-tail dynamic placement via an explicit
composition is permitted but discouraged."
  (let* ((lead-text (pcase context
                      ('chat jf/gptel-fragment-chat-prelude-text)
                      ('agent jf/gptel-fragment-agent-preamble-text)
                      (_ (signal 'jf/gptel-fragment-unknown-context
                                 (list context)))))
         (role (or role-ref (jf/gptel-fragment-ref-static ""))))
    (list (jf/gptel-fragment-ref-static lead-text)
          role
          (jf/gptel-fragment-ref-dynamic jf/gptel-fragment-environment-fn))))

(define-error 'jf/gptel-fragment-unknown-context
  "Unknown send context for default composition"
  'error)

(provide 'jf-gptel-fragments)
;;; fragments.el ends here
