# activities.el persistence — deep dive

Verified against source unless marked inferred.

## Correction to the first survey
activities.el does **not** ship the bare `window-preserved-size` bug: it marks the param `writable` **and** registers a translator that converts the buffer object to a buffer-name on serialize (and back via `get-buffer`). That translator is exactly the piece our code lacks.

## persist.el (github.com/emacs-straight/persist)
- One file per symbol under `persist--directory-location` (default `~/.emacs.d/persist`), name = symbol name, no extension.
- Serializes with `print` + `print-circle t` (handles circularity, **not** unreadable `#<…>`).
- `persist-defvar` runs `persist-load` at defvar-eval time (inferred from macro + docs).
- `persist-load`: `read` with **no error handling** — a corrupt file signals out of the function; variable stays at initvalue if caught upstream.
- `persist-save`: if value `equal` the default, **deletes** the file; else `write-region` **direct to final path** — no atomic rename, no backup.
→ persist.el is **not** safer than a hardened hand-rolled writer.

## activities.el usage
- Persists one var `activities-activities` (hash table) via `persist-defvar`, wrapped in `with-demoted-errors` — that wrapper is the *entire* load-corruption defense (swallow → empty).
- Saves on idle timer + `kill-emacs-hook` (`activities-save-all` → `activities--persist`), gated by `activities-always-persist` (= *whether* to write, not *whether state is trustworthy*).

## activities' window-state serialization
- Buffer slot → `activities-buffer` struct (bookmark/filename/name/local-vars) — never the live object (we mirror this).
- **Translates window-parameter values**: `activities-window-parameters-translators` has `window-preserved-size` (buffer→name serialize, name→get-buffer deserialize). Restore wraps each translator in `condition-case-unless-debug` and drops the param on failure.
- **No** general `#<`/`readablep` scan — safety is structural (whitelist of known buffer-bearing params). A *new* unreadable param would slip through. No write-side guard if the buffer is already dead at save time.

## activities' corruption handling — same clobber bug as us
Corrupt file → `persist-load` `read` signals → `with-demoted-errors` swallows → `activities-activities` stays empty → idle/kill-emacs `activities-save-all` → `persist-save` → because in-memory == default, the file is **deleted** (else overwritten). No backup, no versioning, no atomic write, no "loaded-clean" guard.

## Borrow vs avoid
| Aspect | activities | Safe? | Our decision |
|---|---|---|---|
| Struct buffer reincarnation (bookmark→file→name, error-buffer fallback) | yes | yes | Borrow (already mirrored) |
| `window-preserved-size` translator (buffer↔name) | yes | yes | **Borrow directly** — the fix |
| `writable` param whitelist | yes | only *with* translator | Borrow the pairing, never `writable` alone |
| Load corruption tolerance (`with-demoted-errors`→empty) | yes | **no** | Do NOT copy — preserve+backup instead |
| Save after failed load | yes | **no** (clobber) | Do NOT copy — gate on loaded-clean |
| Atomic write / backups | no | **no** | Do NOT copy — temp+rename, backup |
| Readable-before-write assert | no | partial | **Improve on activities** — add the assert |

## Net recommendation
Keep our own `.eld` writer and harden it (burly-style); **do not adopt persist.el** (no atomic write, no backup, `read` w/o handling, default-collapse deletes the file → imports the exact clobber bug). Borrow activities' *serialization* design (window-param translators + struct reincarnation), not its persistence layer.

## Sources
persist.el (emacs-straight/persist); activities.el (alphapapa) — `activities-window-parameters-translators`, `activities--window-serialized`, `activities--persist`, `activities-save-all`, `activities-mode--killing-emacs`, `cl-defstruct activities-buffer`.
