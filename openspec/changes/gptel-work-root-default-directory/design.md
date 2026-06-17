# Design — gptel-work-root-default-directory

## Context

A gptel session opens its `session.org` via `find-file` / `find-file-noselect`,
which leaves `default-directory` at the file's own directory — the
`branches/<branch>/` *bookkeeping* dir. Nothing resets it. So the model's
relative paths resolve against the bookkeeping dir, and the scope validator
judges a path the model never meant (proposal: "wrong path universe").

Three facts (established during exploration, grounded to this branch) shape the design:

1. **The binder is already the single setup seam.** Content-addressed
   activation (Threads A/F, *implemented*) routes `gptel-chat-mode` via
   `magic-mode-alist` on the drawer signature; the `gptel-chat-mode-hook`
   binder (`config/gptel/sessions/commands.org`) already scans the drawer
   alist and `setq-local`s the four identity vars. Adding `default-directory`
   is one more `setq-local` beside them — no new hook, no new drawer read.
2. **Tools already run in the session buffer's context.** `gptel--handle-tool-use`
   (`gptel-request.el:1692`) wraps every tool funcall in
   `(with-current-buffer (plist-get info :buffer) …)`, and `info :buffer`
   defaults to the request's originating buffer (`:1773`,`:1982`). Verified:
   both interactive chat (`gptel-request` from the chat buffer) and agents
   (`--task` sends inside `with-current-buffer agent-buffer`) see the session
   buffer's `default-directory` for *both* validation and execution. A single
   buffer-local set therefore reaches all tools.
3. **The validator is already sound on absolute paths.** Re-verified on this
   branch: deny-all defaults on missing config; deny-precedence; denials carry
   the `allowed-patterns` the expansion UI offers (195/195 suite green + a
   focused absolute-path batch). The only gap is *which* directory relative
   paths resolve against.

Constraint that bounds every decision: gptel's work-root setup MUST work with
the workspaces package absent (one-way dependency). Workspaces is one
*producer* of the value (integration-registry `:home` payload at `:on-create`);
gptel never names a workspaces symbol.

## Goals / Non-Goals

**Goals:**
- Give each session an explicit work root (`GPTEL_WORK_ROOT`) so relative-path
  scope decisions are meaningful for chat and agents alike.
- Keep cwd and scope in lock-step by deriving both from one `project-root` input.
- Let the parent *pass* an agent's work root and read/write paths explicitly,
  upholding the existing ZERO-inheritance principle.
- Remove the accidental "agent write is always only `/tmp`" behavior; retain
  `/tmp` as scratch.
- No on-disk migration; keyless sessions behave exactly as today.

**Non-Goals:**
- The worktree *manifest / discovery* source the parent model uses to choose a
  `work_root` (exploration-log Thread D) — out of scope; the parent passes an
  explicit value or accepts the parent-default.
- Any change to the validator's allow/deny logic, deny-precedence, or
  fail-closed absolute-path / missing-config behavior.
- Session identity changes (Thread F, already resolved) and a workspaces-side
  `.dir-locals.el` layer (RESOLVED C: redundant vs the registry payload).
- Migrating existing sessions to add the key.

## Decisions

### D1 — Work root is a second persisted *output* of the `project-root` input

`jf/gptel--create-session-core` canonicalizes `project-root` ONCE at entry
(`expand-file-name`), then fans that single canonical-absolute string out to its
two consumers: it expands `${project_root}` into `GPTEL_SCOPE_*`, and it
additionally persists the same string verbatim as `:GPTEL_WORK_ROOT:`. One
canonical input → two byte-identical absolute outputs ⇒ cwd and scope cannot
disagree.
- *Alternatives:* (a) an independent `work_root` param decoupled from scope —
  rejected, reopens the agreement invariant as a manual obligation; (b) renaming
  `project-root` to `work-root` — rejected, conflates the *input parameter* with
  one of its *persisted outputs*. Resolves cross-cutting Q3.

### D2 — Read in the binder; single `setq-local` before the registry block

The binder reads `GPTEL_WORK_ROOT` from the `drawer-alist` it already has and
sets buffer-local `default-directory`. Placed alongside the four identity
`setq-local`s and *before* the `condition-case` registry block, so it survives a
registry failure (matching the existing var-setting discipline).
- *Form:* `(setq-local default-directory (file-name-as-directory (expand-file-name (or (cdr (assoc "GPTEL_WORK_ROOT" drawer-alist)) jf/gptel--branch-dir))))`.
- *Alternatives:* a separate `gptel-chat-mode-hook` function — rejected, the
  binder is the single seam and already holds the alist; `hack-local-variables-hook`
  — rejected, the drawer is not a local-variables mechanism (that ordering
  concern was a dir-locals artifact, RESOLVED C).

### D3 — Keyless fallback = `branch-dir`; absolute-at-source storage

Absent the key, fall back to `jf/gptel--branch-dir` — identical to `find-file`'s
value today, so legacy sessions are unchanged (a true no-op, since
`default-directory` already equals `branch-dir` at activation).

`:GPTEL_WORK_ROOT:` is stored absolute, but the absolute form is established by
the source-side `expand-file-name` in D1 — NOT by the renderer.
`jf/gptel-scope-profile--expand-string` substitutes `${project_root}` VERBATIM (a
raw `replace-regexp-in-string`, no `expand-file-name`; scope-profiles.org
§expand-string), so the scope keys are absolute only because the *input* was
already canonicalized before the renderer saw it. Because the same canonical
string feeds both the renderer and the `:GPTEL_WORK_ROOT:` write, the two outputs
are byte-identical and absolute for ANY input form (relative, trailing-slash,
`~`) — strengthening the agreement invariant from verbatim-agreement to
absolute-and-agreeing without reopening it.
- *Alternatives:* derive a project root for keyless sessions (rejected —
  reintroduces path archaeology); relative-to-session storage for move-safety
  (rejected — archaeology again, and work-root+scope go stale *together* so they
  stay mutually consistent; beta/no-migration accepts the staleness).

### D4 — Tools inherit the work root through the existing buffer context

No new binding point. Per Context fact #2, the binder's buffer-local
`default-directory` is already in effect when `gptel--handle-tool-use` funcalls
each tool. Filesystem tools (`expand-file-name` with no dir arg) and bash
(`directory = default-directory`) both pick it up.
- *Alternative:* binding `default-directory` inside the scope-tool-wrapper from
  session context — rejected as redundant; the spike proved it unnecessary.

### D5 — Agent: parent passes `work_root`, defaulting to the parent's work root

PersistentAgent gains a `work_root` param written into the agent's own drawer as
`:GPTEL_WORK_ROOT:`. Omitted ⇒ default to the parent buffer's work root
(`default-directory` at spawn), *frozen* into the agent drawer. This is a
parent-supplied default captured at create time, not runtime inheritance — the
agent buffer still reads its value from its own drawer (D2 path).
- *Alternatives:* derive `work_root` from the common root of `write_paths`
  (rejected — ambiguous with multiple write globs, removes parent control);
  inherit the parent's drawer values live (rejected — violates zero-inheritance).

### D6 — Symmetric `read_paths` / `write_paths`; `/tmp` is appended scratch

The tool surface replaces `allowed_paths` with symmetric `read_paths` /
`write_paths` (**BREAKING** at the tool-schema level). `jf/gptel-persistent-agent--build-scope-plist`
becomes `(read-paths write-paths)` → `:read read-paths`,
`:write (append write-paths '("/tmp/**"))`, deny unchanged. `/tmp/**` is thus the
last element of the `:write` list and serializes through the *existing*
`+`-multivalue drawer convention (`:GPTEL_SCOPE_WRITE+: /tmp/**`) — no new
encoding.
- *Alternatives:* keep `allowed_paths` and add `write_paths` beside it (rejected
  — asymmetric, confusing); since the schema is re-read by the model each
  session and carries no persisted data, the rename is safe under
  beta/no-migration.

### D7 — Consistency guardrail = warn, not error

When the resolved `work_root` is not matched by any `read_paths` pattern, emit a
warning; do not abort creation. This catches the silent-DENY trap (relative
paths resolving outside scope) without forbidding a deliberate
absolute-path-only agent.
- *Alternatives:* hard error/abort (rejected — too strict; a parent may grant
  only absolute paths intentionally); silent (rejected — revives the very
  failure mode this change removes).

### Testing approach (scenario → spec mapping)

Buttercup, co-located. New/extended files:
- `config/gptel/sessions/test/commands/` — binder sets `default-directory` from
  `GPTEL_WORK_ROOT`; keyless fallback to `branch-dir`; value normalized to a
  directory. `create-session-core` canonicalizes its `project-root` input once
  (`expand-file-name`) and writes `GPTEL_WORK_ROOT` == that canonical-absolute
  string == the scope-expansion root (byte-identical), for non-canonical inputs
  too.
- `config/gptel/tools/test/persistent-agent/` — drawer carries passed
  `work_root`; omitted ⇒ parent's work root frozen in drawer; `build-scope-plist`
  read/write split with `/tmp/**` retained as scratch; tool `:args` schema lists
  the six params and excludes `allowed_paths`/`denied_paths`; guardrail warns when
  `work_root` escapes read scope.
- `config/gptel/scope/test/` — relative path resolves against `default-directory`
  (work root) and lands in scope; same relative path under a different work root
  resolves differently. (Absolute-path fail-closed behavior already covered by
  the green validation suite; not duplicated.)

## Risks / Trade-offs

- [Absolute-path move-fragility] → If the tree moves, `GPTEL_WORK_ROOT` and
  `GPTEL_SCOPE_*` both go stale, but *together* (both from one input), so cwd
  still agrees with scope — just points at the old location. Accepted under
  beta/no-migration.
- [BREAKING schema rename `allowed_paths` → `read_paths`/`write_paths`] →
  Mitigation: grep and update docs/preset guidance and the tool `:description`;
  no persisted data, model re-reads the schema, so no runtime migration.
- [Keyless parent spawning an agent] → parent's `default-directory` is its
  `branch-dir` (a metadata dir); a `work_root`-omitting agent would inherit that
  as its default. Mitigation: only affects sessions created *before* this change;
  the D7 guardrail warns when that root sits outside the agent's read scope;
  beta/no-migration accepts it. The parent can always pass an explicit `work_root`.
- [`default-directory` buffer-local timing] → set via `setq-local` in the binder,
  which runs after `kill-all-local-variables` on mode activation (the binder
  comment and existing four `setq-local`s already rely on this), so it lands.

## Migration Plan

None required (beta / no-migration default).
- **Existing sessions:** keyless → `branch-dir` fallback ⇒ byte-for-byte today's
  behavior. No rewrite of on-disk `session.org` files.
- **Forward:** sessions/agents created after this change carry `GPTEL_WORK_ROOT`.
- **Rollback:** removing the binder stanza and the `create-session-core` write
  reverts behavior; because the keyless path equals the legacy path, a rolled-back
  build silently ignores any `GPTEL_WORK_ROOT` keys already on disk.

## Open Questions

- **Worktree manifest source (deferred, Thread D):** how the parent model
  discovers valid worktrees to choose a `work_root`, kept workspace-neutral
  (`git worktree list` / `magit-list-worktrees`, surfaced via the preamble). Out
  of scope here; until it lands, the parent passes an explicit `work_root` or
  accepts the parent-default.
- **Interactive `work_root` distinct from `project-root`?** For chat sessions we
  set `work_root == project-root` (D1). Whether a future flow wants a chat
  session whose cwd differs from its scope-expansion root is left open; the
  drawer key supports it, but no creation path exposes it today.
