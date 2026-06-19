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

(provide 'jf-gptel-fragments)
;;; fragments.el ends here
