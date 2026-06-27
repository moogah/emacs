---
name: fix-read-metadata-trigger-gap
description: Bash-parser doesn't classify `test`/`[ ` as read-metadata operations and `stat` emits `:read` instead of `:read-metadata`; out-of-scope metadata calls are silently allowed and the `:GPTEL_SCOPE_READ_METADATA:` bucket is dead in practice.
status: ready
source: openspec/changes/gptel-scope-in-org-properties
relations:
  - discovered-from:final-verify-and-archive-prep
  - discovered-from:cycle-2-disposition-10A
---

> Surfaced during cycle-4 manual smoke testing of `gptel-scope-in-org-properties` (Step 4: `:read-metadata` bucket). Sibling to `fix-match-pattern-parser-validator-boundary.md` — both are parser/validator boundary defects where the trigger layer fails to emit the operation type the writer/drawer-bucket layer is built to handle. Recording here so the next maintainer of bash-parser + scope-validation can pick it up; out of scope for the drawer-migration change.

## The bug in one sentence

For `test -e /Users/foo` (and `[ -e ... ]`, and arguably `stat`), the bash-parser does not emit a `:read-metadata` operation against an out-of-scope path, so the validator extracts zero file operations, the no-op allowance lets the call through, the expansion UI never fires, and the `:GPTEL_SCOPE_READ_METADATA:` drawer key — built specifically for cycle-2 disposition 10A — never gets populated through the live add-to-scope flow.

## Reproduction

In a session whose `:GPTEL_SCOPE_READ:` covers `/Users/jefffarr/**` but NOT `/Users/foo`:

```
run_bash_command :command "test -e /Users/foo"
```

Observed (from cycle-4 smoke transcript at `/Users/jefffarr/.gptel/sessions/smoke-drawer-20260430085658/branches/main/session.org` line 115–117):

```
{"success":{},"output":"","exit_code":1,"truncated":{},
 "warnings":["Warning: Command contains absolute path arguments. Directory scope may not protect these paths."]}
```

No expansion UI. No drawer mutation. Silent allow.

Expected per cycle-2 disposition 10A: expansion UI fires with a `:read-metadata` violation; on Add to Scope, `/Users/foo` lands in `:GPTEL_SCOPE_READ_METADATA:` (not `:GPTEL_SCOPE_READ:`).

## Evidence

`config/bash-parser/commands/` enumeration (cycle-4):

- **No `test.el` handler.** `test` and `[ ` builtins fall through to whatever the default extraction is — which appears to extract no file operations.
- **No `[.el` handler.** Same gap.
- **`stat.el` emits `:read`, not `:read-metadata`** (`config/bash-parser/commands/stat.el:14`):
  ```elisp
  (push (list :file arg :operation :read :confidence :high :command "stat") operations)
  ```
  `stat` returns metadata, not file content; the operation type is wrong.
- **Only `dirname.el` and `which.el` emit `:read-metadata`** (`dirname.el:14`, `which.el:14`). The operation type is wired end-to-end (drawer bucket exists, writer routes it, loader reads it back per cycle-3 commit `b5f056e`); the producers are missing.

`config/gptel/scope/scope-validation.el` — the validator depends on the parser to emit operations. With zero operations from a fall-through `test -e ...` parse, no scoping check runs, and the no-op allowance auto-permits.

## Producers that should emit `:read-metadata` but don't

| Command | Current behaviour | Expected emission |
|---|---|---|
| `test -e PATH`, `test -f PATH`, `test -d PATH`, `test -r/-w/-x PATH` | No handler → no operations extracted | `:read-metadata PATH` |
| `[ -e PATH ]`, `[ -f PATH ]`, etc. (the bracket builtin) | No handler → no operations extracted | `:read-metadata PATH` |
| `stat PATH` | Emits `:read` | `:read-metadata` |
| `file PATH` | Emits `:read` (in `file.el`) | `:read-metadata` (debatable — `file` does read magic bytes) |
| `ls -la PATH` (no glob) | Emits `:read-directory` | `:read-directory` ok; metadata-only `ls` is rare |

## Two viable fixes (pick one and design)

**Option D1 (parser-side, principled):** Add `test.el` and a `[ ` builtin handler to `bash-parser/commands/`, both emitting `:read-metadata` on the path argument(s). Reclassify `stat.el` from `:read` to `:read-metadata`. This makes the parser's operation taxonomy match its semantics and unblocks the existing writer/loader pipeline end-to-end.

**Option D2 (validator-side workaround):** Have the validator treat unhandled-but-recognisable commands (`test`, `[`, `stat`) as `:read-metadata` against their absolute-path arguments. Smaller surface, but pollutes the validator with command-specific knowledge that belongs in the parser, and doesn't generalise.

D1 is the recommended path. The handlers are tiny — see `dirname.el` (15 lines including header) as the template.

## Why this matters

The `:GPTEL_SCOPE_READ_METADATA:` drawer key was added in cycle-2 (disposition 10A) specifically so that users could grant narrower-than-`:READ:` access for metadata-only operations. Cycle-3 (commit `b5f056e`) fixed the loader path so the bucket round-trips through `--load-from-buffer` correctly. The writer routes correctly when invoked. Three layers are correct end-to-end, but the trigger layer never fires for the most common metadata commands (`test -e`, `[ -e ]`, `stat`), so the bucket is effectively dead in user-facing flows. Users can populate it via custom-add or edit-manually, but the design intent — Add-to-Scope on a metadata violation routes to the metadata bucket — is unrealised.

This also creates a silent enforcement gap: **out-of-scope `test -e PATH` calls succeed without prompting**. Whether that is "acceptable because metadata is low-risk" or "a security gap because path existence is information disclosure" is a policy question the next maintainer should answer in the proposal that lands D1 or D2.

## Recommended next step

Open a small openspec change under `gptel-scope-bash-parser-metadata-handlers` (or similar). Decide D1 vs D2 in the proposal. If D1: add the three handlers (`test.el`, `[.el`, reclassify `stat.el`), copy the `dirname.el` template, write per-handler unit tests under `config/bash-parser/test/commands/` and an end-to-end smoke spec that asserts add-to-scope on `test -e /etc/hosts` lands `/etc/hosts` in `:GPTEL_SCOPE_READ_METADATA:` of the chat buffer's drawer.

When this lands, cycle-2 disposition 10A is fully realised, and `final-verify-and-archive-prep` Step 4 can be re-run to passing.

## Context

- Cycle-2 disposition 10A: `openspec/changes/gptel-scope-in-org-properties/tasks/closed/disposition-read-metadata-bucket.md`
- Cycle-3 loader fix: commit `b5f056e` (loader `:GPTEL_SCOPE_READ_METADATA:` round-trip)
- Sibling task: `.tasks/fix-match-pattern-parser-validator-boundary.md`
- Cycle-4 smoke transcript: `/Users/jefffarr/.gptel/sessions/smoke-drawer-20260430085658/branches/main/session.org` line 111–121
- Source change archive (after archival): `openspec/archive/gptel-scope-in-org-properties/`
