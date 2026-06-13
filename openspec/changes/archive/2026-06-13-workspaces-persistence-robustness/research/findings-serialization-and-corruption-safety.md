# Findings: robust window-state serialization + corruption-safe persistence

Research grounded in Emacs source + peer packages (verified unless marked inferred).

## Root cause
`window-state-get … 'writable` emits a `writable`-marked window parameter's value **verbatim** (`window--state-get-1`, window.el ~6422-6426): no `bufferp`/`readablep` guard, no recursion into the value. `window-preserved-size` is set by `window-preserve-size` to `(list (window-buffer window) width height)` — a raw buffer object. We mark `(window-preserved-size . writable)` in `workspace-window-persistent-parameters` (layouts.el:15) and only rewrite the leaf `buffer` slot in `workspace--window-state-serialize`, never the parameter value. `prin1` writes `#<buffer …>` / `#<killed buffer>`; `read` throws `invalid-read-syntax`. Bookmark records are a secondary risk: `bookmark-make-record` has no unreadable-object guard (bug#56643: help-mode embedded native-compiled subrs); we have a restore-side workaround (layouts.el:119-145) but no write-side scrub.

## A. Safe serialization — recommended mechanism
**frameset is the wrong altitude** — `frameset-filter-alist` filters *frame* params only; window params flow through a separate `window-state-get`. The right mechanism is a **per-window-parameter serialize/deserialize translator pass** over the window-state leaf tree, as `burly.el` (`burly-window-parameters-translators` / `burly--window-serialized`) and `activities.el` (`activities-window-parameters-translators`) both do: `window-preserved-size` buffer → `buffer-name` on serialize, `get-buffer` on deserialize. Plus burly's belt-and-suspenders `bufferp`→nil scrub on bookmark props.

## B. Peer-package survey
| Package | Capture | Unreadable-object handling | Corrupt-file handling | Source |
|---|---|---|---|---|
| activities.el | `window-state-get nil 'writable` + struct buffer reincarnation | **Has** a `window-preserved-size` translator (buffer→name); struct for buffer slot | `persist.el` + `with-demoted-errors` → **clobbers** (same bug as us) | github.com/alphapapa/activities.el |
| burly.el | `window-state-get nil 'writable` + translator pass → bookmark URL | Per-param translators + `bufferp`→nil scrub (bug#56643) — gold standard | delegates to bookmark.el | github.com/alphapapa/burly.el |
| desktop.el | `frameset-save` (frame params) + per-buffer records | frameset filters frame params; per-buffer restore in `condition-case` | **No clobber**: modtime/checksum guard + `yes-or-no-p`; PID lock | emacs-mirror/emacs lisp/desktop.el |
| frameset.el | filtering layer | filter ACTION vocab `:never`/`:save`/`:restore`/fn→nil/t/(NEW.VAL); **frame params only** | n/a | emacs-mirror/emacs lisp/frameset.el |
| bookmark.el | record store | no write-side guards (only `print-circle`) | **Preserves**: validates `(listp blist)` before assigning, signals rather than overwrite | emacs-mirror/emacs lisp/bookmark.el |

## C. Corruption-safe persistence patterns
- **C1 Atomic write**: temp file in same dir + `rename-file` (atomic on one FS). We currently `with-temp-file` direct to target → mid-write crash truncates.
- **C2 Backup-on-read-failure**: on parse failure of an existing file, rename to `*.corrupt-<ts>` and refuse to overwrite. bookmark.el validates-before-assign; desktop.el prompts before overwrite. We swallow→nil→clobber.
- **C3 Absent vs present-but-unreadable**: must be distinguished; unreadable must gate autosave OFF so kill-emacs/idle can't write empty over the (backed-up) original.
- **C4 Write-time readable assert**: `(read (prin1-to-string form))` or `readablep` (Emacs 28+) before commit; abort write (keep prior good file) on failure. Doubles serialize cost (fine for debounced saves); doesn't cover mid-write crash (that's C1).

## D. Recommended fix architecture (this repo)
- **Layer A (layouts.el):** `workspace-window-parameter-translators` (window-preserved-size buffer↔name) run in `workspace--window-state-serialize`/`-deserialize`; bookmark-record scrub in `workspace--serialize-buffer`.
- **Layer B (persistence.el):** atomic temp+rename in `workspace--write-state` + write-time readable assert; `workspace--read-state` backup-on-corrupt + `workspace--persistence-blocked` flag + absent-vs-unreadable sentinel; gate `workspace--write-state`/`-flush-state`/`workspace-save-state`/`workspace--kill-emacs-flush` on the flag.

## E. Test plan (buttercup) — see tasks. Killed-buffer recipe:
```elisp
(let* ((buf (get-buffer-create "ws-killed-test")) (win (selected-window)))
  (set-window-buffer win buf)
  (with-selected-window win (window-preserve-size win t t))
  (kill-buffer buf)                                  ; param now holds #<killed buffer>
  (let* ((window-persistent-parameters (append workspace-window-persistent-parameters window-persistent-parameters))
         (state (workspace--window-state-serialize (window-state-get (frame-root-window) 'writable))))
    (expect (prin1-to-string state) :not :to-match "#<")))   ; clean after fix
```
Corruption-injection: write a file containing `#<…>`; assert loader preserves it as `*.corrupt-*`, sets the blocked flag, hydrates empty, and a subsequent flush does NOT write `(:version 3 :workspaces nil)`.

## Sources
window.el (`window--state-get-1`, `window-preserve-size`); ELisp manual "Window Configurations"/"Preserving Window Sizes"; activities.el; burly.el; frameset.el; desktop.el; bookmark.el; emacs-devel "Atomic file replacement" (2020-10); bug#56643; bug#12503/#12507.
