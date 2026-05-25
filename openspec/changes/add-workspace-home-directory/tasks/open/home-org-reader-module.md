---
name: home-org-reader-module
description: New home-org.org module with regex-based #+TITLE: reader (no org-mode parser)
change: add-workspace-home-directory
status: ready
relations: []
---

## Files to modify
- `config/workspaces/home-org.org` (new — literate source)
- `config/workspaces/home-org.el` (new — tangled)
- `config/workspaces/test/home-org-spec.el` (new — Buttercup)
- `init.org` (modify — register `workspaces/home-org` in `jf/enabled-modules`)

## Implementation steps

1. Create `config/workspaces/home-org.org` with the standard literate
   header:

   ```org
   #+title: Workspaces — home.org Reader
   #+author: Jeff Farr
   #+property: header-args:emacs-lisp :tangle home-org.el
   #+auto_tangle: y
   ```

2. Add a top-level lexical-binding declaration:

   ```elisp
   ;;; home-org.el --- Workspaces home.org reader -*- lexical-binding: t; -*-
   ```

3. Define the file-path helper:

   ```elisp
   (defun workspace-home-org-path (home)
     "Return the absolute path to `home.org' inside HOME directory."
     (expand-file-name "home.org" home))
   ```

4. Define the title reader using regex (NOT `org-collect-keywords` —
   this avoids loading `org-mode` and is ~10× faster per design D4):

   ```elisp
   (defun workspace-home-org-title (home)
     "Return the trimmed #+TITLE: keyword value from HOME/home.org, or nil.
   Returns nil if the file is missing, unreadable, or has no #+TITLE:
   keyword. Live read on every call — no caching."
     (let ((path (workspace-home-org-path home)))
       (when (file-readable-p path)
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (when (re-search-forward "^#\\+TITLE:[ \t]+\\(.*\\)$" nil t)
             (let ((title (string-trim (match-string 1))))
               (and (not (string-empty-p title)) title)))))))
   ```

5. Define a tiny existence helper used by anchoring logic:

   ```elisp
   (defun workspace-home-org-exists-p (home)
     "Return non-nil if HOME contains a `home.org' file."
     (file-readable-p (workspace-home-org-path home)))
   ```

6. End with `(provide 'workspace-home-org)`.

7. Create `test/home-org-spec.el` with cases (use a temp-dir helper —
   the same `workspace-test--with-tmp-home` pattern other tasks
   establish; if not yet present, inline a let-bound `make-temp-file
   (dir) t` and an `unwind-protect` `delete-directory`):

   - `workspace-home-org-title` on a missing file returns nil.
   - On a file with no `#+TITLE:` returns nil.
   - On a file with `#+TITLE: Foo` returns `"Foo"`.
   - On `#+TITLE:   spaces around   ` returns trimmed `"spaces around"`.
   - On `#+title: Lowercase` returns `"Lowercase"` (case-insensitive
     regex — verify your regex actually matches; if it does not by
     default, switch to a `case-fold-search` `let`).
   - On `#+TITLE:` (empty) returns nil.
   - On a file where `#+TITLE:` appears NOT on the first line (e.g.,
     comments above) the title is still found.
   - `workspace-home-org-exists-p` returns nil for nonexistent file, t
     for existing.

8. Register the module: add `"workspaces/home-org"` to
   `jf/enabled-modules` in `init.org` (place it after `"workspaces"` /
   before `"workspaces-mode"`).

9. Tangle and validate:
   ```bash
   ./bin/tangle-org.sh config/workspaces/home-org.org
   ```

## Design rationale

`home.org` is read lazily on every query (no caching) per the spec's
"live read" requirement. The regex approach (D4 in design.md) is chosen
over `org-collect-keywords` because:

- Reads happen during `completing-read` prompts and mode-line refreshes
  — they are user-perceptible.
- We only need one keyword (`#+TITLE:`), not a full parser.
- Loading `org-mode` for the buffer is heavy and pulls in transitive
  initialization (`org-load-hook`, etc.).
- Regex on a temp buffer is order-of-magnitude faster and has no side
  effects.

The module is intentionally kept separate from `data-model.org` (which
is the side-effect-free layer) because it does file I/O. Other modules
that need to display the workspace's name read `(or
(workspace-home-org-title home) basename)`.

## Design pattern

`insert-file-contents` into `with-temp-buffer` is the standard
side-effect-free file read in Emacs Lisp. Pattern:

```elisp
(with-temp-buffer
  (insert-file-contents path)
  ;; ...scan...
  )
```

`file-readable-p` before reading: belt-and-suspenders for the empty/
missing case. Returning nil on any read failure (not signalling) lets
callers do `(or title fallback)` cleanly.

## Verification

- Tangle: `./bin/tangle-org.sh config/workspaces/home-org.org`
- Spec passes: `./bin/run-tests.sh -d config/workspaces -p home-org-spec`
- Module loads cleanly on Emacs startup (no errors in `*Messages*`
  after `./bin/emacs-isolated.sh -nw` and `M-x message-buffer`).

## Context

design.md § Decisions / D4 — home.org reader: regex scan, no `org-mode` activation
specs/workspaces/spec.md § ADDED "home.org is user-authored after creation"
specs/workspaces/spec.md § ADDED "Required home directory and identity coupling" (scenarios on #+TITLE: override)
