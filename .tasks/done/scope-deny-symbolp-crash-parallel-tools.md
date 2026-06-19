---
name: scope-deny-symbolp-crash-parallel-tools
description: "Live PersistentAgent test: denying a scope expansion throws `Wrong type argument: symbolp, \"<glob pattern>\"` and leaves the parent session stuck on the agent overlay. Root-caused to symbol-name on a non-symbol operation at scope-validation.org:246, reached via the parallel-tool / mixed expansion-queue path."
source: live work-root Phase-3 test session 2026-06-18 (session ~/.gptel/sessions/wr-live-20260618124704)
status: done
relations:
  - discovered-from:gptel-work-root-default-directory-verify-change
  - related-to:gptel-preexisting-async-scope-test-failures
---

## RESOLUTION (2026-06-18)

Fixed. The denial payload's array fields (`:allowed-patterns`,
`:deny-patterns`) are now JSON arrays (vectors), and ALL model-facing tool
results route through one hardened serializer.

Code changes:
- `scope-validation.org`: `format-tool-error` now `vconcat`s the pattern lists
  (`[]` when empty); the canonical `jf/gptel-scope--serialize-tool-result` +
  `jf/gptel-scope--sanitize-for-json` MOVED here (common ancestor) and extended
  to (a) coerce any non-keyword/non-alist list → vector, and (b) be TOTAL —
  a second failure degrades to a `serialization_error` payload instead of
  throwing, so a callback always fires and the FSM can never wedge.
- `scope-tool-wrapper.org`: serializer defuns removed (now call into validation).
- `scope-expansion.org` (deny / safe-callback / allow-once) and
  `scope-shell-tools.org` (request_scope_expansion): the 4 raw `json-serialize`
  call sites now route through `jf/gptel-scope--serialize-tool-result`. Five
  serializers → one chokepoint.

Tests (RED first, then fix):
- NEW `scope/test/integration/deny-serialization-spec.el`: serializer unit +
  real-deny single + parallel + mixed-with-request_scope_expansion. All drive
  the REAL `jf/gptel-scope--deny-expansion` under NON-EMPTY scope.
- CORRECTED the specs that papered over it: `work-root-worked-example-spec.el`
  STEP 2/4 (was a no-op expansion stub → now drives the real deny continuation
  and asserts the delivered denial), `parallel-tool-callback-spec.el` "both
  callbacks fire via queue" (was empty-scope + direct callback → now non-empty
  scope + real deny suffix, filesystem-based for batch stability), and the
  `tools/test/filesystem-tools-spec.el` contract test (asserted the buggy
  list shape → now asserts a vector).

Verification: `./bin/run-tests.sh -d config/gptel -f buttercup` →
1357 specs, **20 failed** (was 21 at baseline: zero new failures, one
pre-existing async-cluster flaky test fixed as a bonus). ERT 22/22, 0 unexpected.
Confirmed RED-before / GREEN-after by stashing the source and re-running.

The `gptel-work-root-default-directory` ARCHIVE HOLD (see that change's
`tasks/open/verify-change.md`) was blocked ONLY on this crash and can now be
lifted.

## Symptom (observed live, 2026-06-18)

During Phase-3 live verification of `gptel-work-root-default-directory`, an
fs-scope test agent (`test-agent-fs-scope`) was asked to run four file ops,
three of which it issued **in parallel**, with a prompt that also nudged it
toward `request_scope_expansion`. When the user **denied** the scope-expansion
prompt:

```
Error invoking callback: Wrong type argument: symbolp,
  "/Users/jefffarr/emacs-gptel-default-directory/config/gptel/presets/**"
```

The parent session buffer was then **stuck** displaying the agent overlay; the
only recovery was killing the buffer and deleting the agent directory.

The string in the error is the agent's **allowed read pattern** (an
expansion-offered pattern), not the denied resource.

## Root cause (PROVEN — supersedes the earlier :246 hypothesis)

**The earlier hypothesis (symbol-name on a corrupted `operation` at
`scope-validation.org:246`) is WRONG.** A deterministic batch repro (drive the
real filesystem tools through the real wrapper, then drive the real
`jf/gptel-scope--deny-expansion`) shows the violation's `:operation` is the
correct symbol/keyword (`:read`); `:246` never gets a pattern. The closures are
lexically sound; nothing crosses the `operation` slot.

**Actual throw site: `json-serialize` on a plain Lisp list.**
`jf/gptel-scope--validate-path-operation` (scope-validation.org:211-218) sets

```elisp
:allowed-patterns (pcase operation
                    ((or :read ...) (append read-patterns write-patterns))
                    ...)
```

i.e. `:allowed-patterns` (and `:deny-patterns`) are **plain Lisp lists** of
pattern strings. `format-tool-error` (scope-validation.org:1002-1024) copies
them verbatim into the deny-response plist. The on-deny leg serializes that
plist with `jf/gptel-scope--serialize-tool-result` →
`(json-serialize result)`. Emacs `json-serialize` has **no list→array
coercion**: a non-keyword-headed list is treated as a plist/alist, so it calls
`symbol-name` on the first element — a pattern **string** — raising the exact
observed error:

```
(wrong-type-argument symbolp "/…/config/gptel/presets/**")
```

Verified directly: `(json-serialize (list "/a/**" "/b"))` ⇒
`(wrong-type-argument symbolp "/a/**")`. A **vector** serializes fine.

**Why it looked parallel-only / single-tool-fine — it is NOT about parallelism.**
The real trigger is simply *a denied filesystem op whose scope has at least one
allowed (or deny) pattern*:

- **Empty / deny-all scope** (`paths.read = ()`): `:allowed-patterns` is `nil`,
  which serializes cleanly (`{}`-ish), so no crash. Every existing
  `parallel-tool-callback-spec` and most scope specs use empty scope, which is
  exactly why the suite is green and the bug hid.
- **Real scope** (live session had `:GPTEL_SCOPE_READ: …/presets/**`): the
  init.el read denial carries `:allowed-patterns ("…/presets/**")` → crash. The
  parallel framing was incidental — that live run was just the first real-scope
  deny exercised end-to-end.

**Why `--serialize-tool-result`'s `wrong-type-argument` guard does not save it:**
the guard retries via `jf/gptel-scope--sanitize-for-json`, but that helper only
re-encodes offending *string bytes* (Latin-1) — it does **not** convert Lisp
lists to vectors. So the retry `json-serialize` re-throws the same `symbolp`
error, now *outside* the guard, and it escapes.

**Why the session hangs (the parallelism-relevant part):** the symptom differs
by *when the deny resolves*, which is what parallel/queued calls change:

- *Synchronous deny* (expansion resolves within the tool's dynamic extent — e.g.
  a mocked direct-callback): the escaped throw is caught by the scoped-tool
  macro's **outer `condition-case`** (scope-tool-wrapper.org:167), which returns
  a `{"success":{}, "error":"tool_exception", "message":"Tool error: Wrong type
  argument: consp/symbolp …"}` to the model. The callback DOES fire — no hang —
  but the model gets garbage instead of a real denial, and allowed-patterns are
  lost.
- *Queued / async deny* (expansion resolves on a later turn via the transient
  suffix → `jf/gptel-scope--deny-expansion`, scope-expansion.org:481): the throw
  fires from the **callback funcall** there, far outside the macro's
  `condition-case`. It is only `message`-logged ("Error invoking callback: …"),
  the gptel `process-tool-result` callback **never fires**, the FSM counter
  never reaches `ntools`, and the session is stuck on the agent overlay forever.

So the documented "Parallel tool callback: transient collision / session stuck"
cluster and this crash share the async-deny resolution path, but the *crash
itself* is the list-vs-vector `json-serialize` defect, not a transient/queue
arg-crossing.

## NOT implicated: the work-root change

The work-root outputs are all correct on disk for this very session — the agent
drawer carries `:GPTEL_WORK_ROOT: …/presets`, `:GPTEL_SCOPE_READ: …/presets/**`
(D6 auto-include fired even with `read_paths nil`), `:GPTEL_SCOPE_WRITE: /tmp/**`,
and a later run shows add-to-scope **persisting** `init.el` / `pa-fs-test.txt`
into the drawer. Denial *does* occur; the bug is purely in the parallel-deny
callback path.

## Fix (proven, small, local)

**Primary — emit JSON arrays as vectors, not lists.** The deny-response must
carry `:allowed-patterns` / `:deny-patterns` as **vectors** so `json-serialize`
sees JSON arrays. Cleanest single chokepoint is `format-tool-error`
(scope-validation.org:1002-1024): wrap the pattern lists with `vconcat` (and
guard `nil` → `[]` or leave `nil`, both serialize). This is where every denial
funnels, so one edit covers filesystem and bash denials. (Could also fix at the
source in `validate-path-operation`, but that plist is consumed internally as a
list elsewhere — fix at the serialization boundary instead.)

**Defense-in-depth — make `--serialize-tool-result` total.** `sanitize-for-json`
only fixes string bytes; extend it (or the serialize guard) so a bare
non-keyword list is coerced to a vector, so a future stray list can never again
turn a denial into a `tool_exception` / hang. Optional but cheap.

**Also fix the hang independently of the payload.**
`jf/gptel-scope--deny-expansion` (and `--allow-once-action`, `--add-to-scope`,
the `safe-callback` sites) `message`-swallow a throwing callback, leaving the
FSM wedged. Even with the payload fixed, a throwing callback should still
deliver *some* terminal result to gptel (e.g. a serialized `tool_exception`)
before `process-expansion-queue`, so a callback error degrades to a denied tool
rather than a stuck session.

This is NOT an async/queue architecture change and should NOT be promoted to an
openspec change — it is a serialization-shape bug. Fixing the payload should
also green the real-scope deny scenarios and likely several quarantined
parallel-tool-callback specs once they are rewritten to use non-empty scope +
the real deny action (see Follow-up).

## Secondary observation (separate, lower priority)

In the expansion-efficacy retry run (step 8), the agent surfaced a **"create
directory" permission prompt twice** instead of a scope-expansion transient.
Likely the `write_file_in_scope` body's `(make-directory dir t)` or a gptel
`:confirm`, not a scope path. Investigate separately; capture which tool/agent
turn issued it from `~/.gptel/sessions/wr-live-20260618124704`.

## Repro

From a parent persistent session buffer, spawn `test-agent-fs-scope` with a
narrow `work_root` (so out-of-scope reads are genuinely denied) and a prompt
that asks for several file ops "in parallel" plus a `request_scope_expansion`
fallback, then **deny** the first expansion prompt. See the design doc
`.tasks/design-working-directory-scoping-for-sessions-and-agents.md` and the
captured session at `~/.gptel/sessions/wr-live-20260618124704`.
