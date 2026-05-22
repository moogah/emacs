---
name: emit-system-prompt-and-chat-headings-at-creation
description: A freshly created session.org is a flat file-level drawer plus a bare `#+begin_user` block. Restructure the creation output so the file is the file-level config drawer (unchanged location), then a folded `* System Prompt` heading whose body is seeded from the preset's system prompt, then a `* Chat` heading holding the empty user block. Establishes the canonical document shape; making the heading authoritative is a follow-up task.
change: gptel-drawer-as-source-of-truth
status: ready
relations: []
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — `jf/gptel--initial-session-body` (and `jf/gptel--initial-session-content`) gain the heading structure; `jf/gptel--create-session-core` threads the preset's `:system` text through
- `config/gptel/scope-profiles.org` (modify, if the heading emission belongs with the renderer) — confirm whether `--create-for-session` or the body helper owns the new headings; keep the file-level drawer text exactly where it is
- `openspec/changes/gptel-drawer-as-source-of-truth/specs/gptel/sessions-persistence.md` (modify) — revise the session-creation shape requirement to the new layout
- `config/gptel/sessions/test/commands/preset-application-spec.el` and/or `session-org-creation-spec.el` (modify) — assert the new shape

## Why

design.md §Addendum Finding B (Decision B). The system prompt is moving out of preset-only invisibility into the document as a visible `* System Prompt` heading body — an org heading body carries multi-line, special-character text with no escaping, so Decision 2's "unwieldy as a property value" objection does not apply to a heading body. This task lays down the canonical shape at creation time; `make-system-prompt-heading-authoritative` then makes the heading load-bearing on save/restore.

Layout (Option B — file-level drawer, then headings; chosen for lowest code risk: `org-entry-put` / `gptel-org--entry-properties` at `point-min` are unchanged):

```
:PROPERTIES:
...config + scope keys...
:END:

* System Prompt
:PROPERTIES:
:VISIBILITY: folded
:END:
<preset :system text — seeds the body>

* Chat
#+begin_user

#+end_user
```

## Implementation steps

1. The file-level config drawer text is unchanged — keep `jf/gptel-scope-profile--render-drawer-text` / `--create-for-session` output exactly as is, emitted first, at `point-min`.
2. Extend the body template so it emits, after the drawer:
   - `* System Prompt` with a `:PROPERTIES:`/`:END:` drawer containing `:VISIBILITY: folded`, followed by the system-prompt body text.
   - `* Chat`, followed by the empty `#+begin_user` / `#+end_user` block.
   `jf/gptel--initial-session-body` currently takes no arguments — give it a `system-prompt` parameter (string or nil), and have `jf/gptel--create-session-core` pass `(plist-get preset-spec :system)` (the spec it already resolves at line ~388). When the preset has no `:system`, emit the `* System Prompt` heading with an empty body — the heading shape stays canonical and consistent.
3. Decide where the heading emission lives — recommend a single small helper (e.g. `jf/gptel--session-headings-block`) so the exact heading/`:VISIBILITY:` shape has one source of truth, reused by `make-system-prompt-heading-authoritative`'s save path. Avoid duplicating the literal heading strings across modules.
4. Keep `gptel-chat-new` scratch buffers unchanged — they emit a bare `#+begin_user` block with no headings (design.md §Decision 9). The parser's heading-indifference (Decision 12) is what lets both shapes coexist.
5. Re-tangle the touched `.org` files and update `specs/gptel/sessions-persistence.md`: the session-creation requirement now describes the file-level drawer + `* System Prompt` (folded, preset-seeded) + `* Chat` layout.
6. Update creation specs to assert: file-level drawer still at `point-min`; a `* System Prompt` heading with `:VISIBILITY: folded` and the preset's `:system` as body; a `* Chat` heading; the empty user block under `* Chat`. Verify `gptel-chat-parse-buffer` still returns the expected (empty) turn list for the created file — the `* System Prompt` body must not be parsed as a turn.

## Verification

```bash
./bin/tangle-org.sh config/gptel/sessions/commands.org
./bin/tangle-org.sh config/gptel/scope-profiles.org
./bin/run-tests.sh -d config/gptel/sessions
./bin/run-tests.sh -d config/gptel/chat/test/parser
grep -n 'System Prompt\|VISIBILITY\|\* Chat\|initial-session-body' config/gptel/sessions/commands.el
```

Expect: a created `session.org` matches the layout above; parser tests still pass (heading body is commentary, not a turn).

## Context

design.md §Addendum Finding B. Parser heading-indifference: `config/gptel/chat/parser.el` (`gptel-chat-parse-buffer`, design.md §Decision 12). Preset spec is already resolved in `jf/gptel--create-session-core` (`config/gptel/sessions/commands.el` ~line 388). Known limitation recorded in design.md: a system-prompt body with a literal `^#+begin_user` would be mis-parsed — accept or add a narrow guard.

Cited register entries (cycle-7 plan, both `status: speculated`):
- `interfaces.org#register-shape-session-document-layout` — this task is the **producer**: the creation renderer emits the file-level drawer + `* System Prompt` (folded, preset-seeded) + `* Chat` layout the entry describes. The entry carries an embedded `validator` defun; assert against it.
- `interfaces.org#register-invariant-system-prompt-heading-authoritative` — this task lays the `* System Prompt` heading shape the invariant depends on; making the body *authoritative* is the sibling task. No scaffold work here.

## Observations

- **`scope-profiles.org` was not modified.** The task's files-to-modify
  listed `config/gptel/scope-profiles.org` "if the heading emission belongs
  with the renderer". It does not. `jf/gptel-scope-profile--render-drawer-text`
  / `--create-for-session` produce only the file-level `:PROPERTIES:` config
  drawer string; the task explicitly requires that drawer text stay byte-for-byte
  identical and at `point-min`. The `* System Prompt` / `* Chat` headings are
  document *body*, owned by `jf/gptel--initial-session-body` in `commands.org`.
  Emitting headings from the renderer would have entangled drawer rendering with
  body layout. The new single-source-of-truth helper `jf/gptel--session-headings-block`
  lives in `commands.org` instead — it is also where the sibling task
  `make-system-prompt-heading-authoritative`'s save path can reuse it.
- **Pre-existing ERT failure: `test-directory-creation-org-session-structure`**
  (`config/gptel/sessions/filesystem-test.el:43`). This test asserts at line 65
  that `metadata.yml` exists after `jf/gptel--create-session-core` runs. Decision 6
  of this change (`No metadata.yml is written`) removed metadata.yml emission in
  the earlier task `rewire-session-creation` (commit `be6b80c`), so this test has
  been failing since well before this task. It is the "sessions-filesystem"
  pre-existing failure named in the orchestrator's baseline note. Not touched —
  out of scope for this task; the test is stale and should be updated (or deleted)
  by whoever owns the metadata.yml removal. Note also that its `cl-letf` mock of
  `jf/gptel-scope-profile--create-for-session` uses a 5-arg lambda while the real
  function takes 6 args; the `&optional` tail makes the extra arg harmless, but
  the mock is drifting from production.
- **Three existing creation/activity specs had to be updated for the new shape.**
  `session-org-creation-spec.el`, `preset-application-spec.el`, and
  `activity-session-chat-spec.el` all asserted the old flat-drawer-then-bare-user-block
  layout. `activity-session-chat-spec.el` is not in this task's files-to-modify list,
  but its "no markdown heading" assertion (`content :not :to-match "^# "`) was
  broken by the change: the `* System Prompt` body is now verbatim preset `:system`
  text, and the `executor` preset's prompt legitimately contains markdown `# `
  lines. The fix scopes the markdown-markup check to the renderer-owned regions
  (config drawer + heading shape + `* Chat` block), excluding the verbatim
  system-prompt body. The same fix was applied to the equivalent test in
  `session-org-creation-spec.el`.
- **The §Addendum known limitation does not bite in practice.** A `* System Prompt`
  body containing a column-0 `#+begin_user` would be mis-parsed as a turn. A grep
  of `config/gptel/presets/*.md` confirms no preset system prompt contains a
  column-0 `#+begin_*` line. The decision recorded in design.md ("accept or add a
  narrow guard") is resolved as **accept**: no guard added, and a regression test
  (`parses to a single user turn even when the preset has a markdown system prompt`)
  pins that a markdown-heavy `:system` still parses to exactly one user turn.
- **Empty `#+begin_user` block parses to content `"\n"`, not `""`.** The parser's
  `gptel-chat--scan-user-body` returns the verbatim `buffer-substring` between the
  delimiters; for `#+begin_user\n\n#+end_user` that is the single newline. The
  new parser-roundtrip test asserts `"\n"` accordingly. This matches existing
  parser-test conventions (e.g. `buffer-format-spec.el` asserts content
  `"deep-nested prompt\n"` with the trailing newline kept).

## Discoveries

- discovery_id: disc-emit-system-prompt-and-chat-headings-at-creation-1
  class: spec-signal
  description: |
    register/shape/session-document-layout (status: speculated, load_bearing)
    is now fully realised by the creation renderer. The embedded `validator`
    defun `shape/validate-session-document-layout` was used as the assertion
    contract: a created `session.org` satisfies all four of its checks —
    config drawer at point-min, exactly one `* System Prompt` heading,
    exactly one `* Chat` heading, no turn block before `* Chat`. The
    speculated entry matched implementation with no deviation: the document
    regions, structural_invariants, and known_limitation all hold as written.
    The producers list is accurate (commands.org owns the creation-time
    emission). One refinement: the entry's `producers` says the initial-body
    helper emits "the full document layout (config drawer + ... + `* Chat`)",
    but in implementation the config drawer is rendered by scope-profiles.org's
    `--render-drawer-text` and only *prepended* at the call site; the body
    helper (`jf/gptel--initial-session-body`) emits only the two headings.
    The new single-source-of-truth helper for the heading shape is
    `jf/gptel--session-headings-block`.
  affected_register_entry: register/shape/session-document-layout
  recommendation: |
    Reconcile entry from `speculated` to `confirmed`. Adjust the
    `producers` description: the creation renderer composes
    `(concat <drawer-text> <headings>)` where drawer-text comes from
    `jf/gptel-scope-profile--render-drawer-text` and the heading block
    comes from the new `jf/gptel--session-headings-block` helper in
    config/gptel/sessions/commands.org (the single source of truth for
    the `* System Prompt` / `* Chat` shape, reused by the
    make-system-prompt-heading-authoritative save path). No change to
    document_regions, structural_invariants, validator, or
    known_limitation — all confirmed accurate.

- discovery_id: disc-emit-system-prompt-and-chat-headings-at-creation-2
  class: invariant-gap
  description: |
    register/invariant/system-prompt-heading-authoritative (status:
    speculated, load_bearing) depends on the `* System Prompt` heading
    shape. This task lays that shape: the heading carries
    `:VISIBILITY: folded` and a body seeded from the preset's `:system`.
    The invariant's "create" leg of the round trip
    (create -> restore -> save -> re-restore) is now satisfiable — a fresh
    session.org has an authoritative-capable heading. The restore/save legs
    remain the sibling task's work (no scaffold work done here, as briefed).
    One observation feeding that sibling task: the heading body emitted at
    creation is the preset `:system` text *verbatim* with no escaping, and
    a blank/nil `:system` yields an empty body (heading still present).
    The invariant's "blank body falls through to preset" rule therefore
    composes cleanly with this producer — a no-`:system` preset creates an
    empty heading body, and the restore precedence will fall through to the
    preset as the invariant specifies.
  affected_register_entry: register/invariant/system-prompt-heading-authoritative
  recommendation: |
    Keep `speculated` until make-system-prompt-heading-authoritative lands
    the restore + save legs. No reconciliation needed from this task — the
    shape it depends on is now in place and consistent with the invariant's
    statement (verbatim body, empty-when-no-:system, fold via :VISIBILITY).
