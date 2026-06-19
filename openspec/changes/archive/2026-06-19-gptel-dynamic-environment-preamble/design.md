## Context

A gptel session records its work root (`GPTEL_WORK_ROOT` → buffer
`default-directory`) and its file-access scope (`GPTEL_SCOPE_*`) in the
session-file `:PROPERTIES:` drawer, both shipped by the archived
`2026-06-18-gptel-work-root-default-directory` change. The model, however, is
told only its role (the system prompt); it must spend tool calls discovering its
cwd and scope. This change appends a short, always-current **environment block**
to the system message so the model knows its environment up front.

The injection point already exists. `config/gptel/chat/menu.org` installs a
`:before` advice on `gptel-request`
(`gptel-chat--refresh-system-prompt-from-file`) that, for `gptel-chat-mode`
buffers only, re-reads the sibling `system-prompt.<ext>` into
`gptel--system-message` before dispatch. Persistent agents send through
`gptel-request` from buffers that are themselves `gptel-chat-mode`, so this one
advice covers chat and agents alike.

Hard constraint (from the exploration log): gptel's environment surfacing MUST
work with the workspaces package absent — the builder reads only the buffer's
drawer and `default-directory`, naming no workspace symbol.

## Goals / Non-Goals

**Goals:**
- Append a workspace-neutral environment block (work root + scope, with prose
  framing and a "this is live" note) to the tail of the composed system message
  on every send from a `gptel-chat-mode` buffer.
- Cover interactive chat and persistent agents with one seam and no
  agent-specific code.
- Keep the composition idempotent: rebuilt wholesale from sources each send, so
  scope grown mid-session stays accurate and blocks never accumulate.
- Degrade gracefully on a chat-mode buffer with no scope drawer.

**Non-Goals:**
- A worktree manifest / discovery list (deferred; was dropped from this block's
  scope during exploration).
- The agent write-scope fix (separate: `.tasks/agent-work-root-into-write-scope.md`;
  this change assumes agents carry correct write scope).
- Changing scope-validation, activation, or buffer-format behavior.
- Trimming/length management of the block (scope lists are short today; revisit
  if blocks ever get long).

## Decisions

### D1 — One widened pre-send composer, not a second advice

Fold the environment append into the existing pre-send function rather than
adding a second `:before` advice. The composer runs on every send from a
chat-mode buffer and does, in order: (1) resolve the role content (sibling-file
body when `:GPTEL_SYSTEM_PROMPT_FILE:` is set and readable — current behavior;
else the buffer's base system message), (2) build the environment block
(unconditional), (3) set `gptel--system-message` to `role + "\n\n" + env` in one
wholesale assignment.

- *Why:* a single function gives a deterministic order (env always after the
  sibling read) with no advice-ordering/`depth` fragility. *Alternative —* two
  ordered `:before` advices: rejected (ordering is implicit and brittle under
  re-tangle/reload).

### D2 — Idempotency via a stable role base, never reading back the composed value

The composed `gptel--system-message` (role + env) MUST NOT become an input to
the next send's composition, or the env block accumulates. The sibling-file case
is already safe (role is re-read from the *file* each send). The no-sibling case
is the trap: today `gptel--system-message` is set once at activation and would be
read back.

Decision: track the role content in a dedicated buffer-local
(`gptel-chat--system-prompt-base`) written wherever role content is installed
(activation-time `apply-system-prompt-file` / preset application, and each
sibling refresh). The composer reads the base, never the composed output;
`gptel--system-message` becomes purely the per-send composed result.

- *Alternative — strip the known env block off `gptel--system-message` before
  re-appending:* rejected (string surgery on model-visible text; fragile if the
  block format changes). *Alternative — re-resolve the preset `:system` from the
  drawer each send:* rejected (duplicates activation logic).

### D3 — Builder reads raw drawer scope keys, renders verbatim globs

The builder reads `GPTEL_SCOPE_READ` / `GPTEL_SCOPE_WRITE` / `GPTEL_SCOPE_DENY`
straight from the drawer via `jf/gptel--scan-session-drawer-keys` (the same
scanner the binder uses), and the work root from `default-directory`. Scope is
rendered as **verbatim glob patterns** (not summarized), because those are
exactly what the validator enforces — the model should see the real boundary.

- *Why drawer keys, not `jf/gptel-scope--load-config`:* load-config expands/
  applies deny-all defaults for validation; the block wants the human-facing
  patterns as written. *Alternative — summarized prose:* rejected (loses the
  exact patterns; the model would guess at the boundary).

Block shape (markdown, appended at tail):

```
# Environment
You are working in the directory below. The file-access scope lists what you may
read and write; this information is current as of this message.

- Working directory: <default-directory>
- Readable: <GPTEL_SCOPE_READ globs>
- Writable: <GPTEL_SCOPE_WRITE globs>
- Denied:   <GPTEL_SCOPE_DENY globs>
```

### D4 — Tail placement, appended after the role

The block is appended after the role content (`role + "\n\n" + env`), making it
the last section of the system message. *Why:* it composes by pure `concat` (no
splitting of the agent's baked harness+role sibling), leaves the agent write path
untouched, and the recency of the volatile data (scope) sitting last suits it.
Order the model reads: identity → rules → role → current environment.

### D5 — Graceful degradation, scoped to chat-mode

A chat-mode buffer with no `GPTEL_SCOPE_*` keys yields a block reporting
`default-directory` and "no scope restrictions apply" — never an error, never an
empty block. The whole mechanism stays behind the existing
`(derived-mode-p 'gptel-chat-mode)` guard, so non-chat-mode `gptel-request`
callers pay only the predicate check.

## Risks / Trade-offs

- [Env block accumulates across sends if composed value is read back] →
  Mitigated by D2 (stable role base; `gptel--system-message` is write-only
  output of the composer). Covered by an explicit two-send idempotency test.
- [Block reports scope that disagrees with what agents can actually write,
  because the write-scope fix is separate] → Accepted: the block honestly
  reports the drawer's `GPTEL_SCOPE_WRITE`; once
  `.tasks/agent-work-root-into-write-scope.md` lands, the drawer (and thus the
  block) is correct. The block never lies about the current drawer.
- [Reading the drawer on every send] → Cheap (a buffer scan, no shell-out);
  negligible against a model round-trip. No caching needed.
- [Base-tracking touches the activation installer] → Small, localized: set
  `gptel-chat--system-prompt-base` at the same points that already set
  `gptel--system-message`. Existing pre-send-refresh tests guard the role path.

## Migration Plan

None required (beta / no-migration). No on-disk format change. Sessions created
before this change get the block computed from whatever their drawer carries
(or the drawerless degradation path). Rollback = remove the env append + the
base-tracking `setq-local`s; the pre-send refresh reverts to role-only.

## Testing Approach

- **Framework / location:** Buttercup, co-located at
  `config/gptel/chat/test/menu/environment-preamble-spec.el` (beside the
  existing `pre-send-refresh-spec.el`), using `config/gptel/chat/test/test-helpers.el`.
- **Unit (builder):** given a drawer alist + `default-directory`, assert the
  block reports the work root, renders read/write/deny verbatim, includes the
  live-note, and (no `GPTEL_SCOPE_*`) reports cwd + "no restrictions" without
  error. Builder names no workspaces symbol (workspaces absent → still builds).
- **Behavioral (pre-send composition):** in a real `gptel-chat-mode` buffer,
  invoke the composer/advice and assert: env block is the tail of
  `gptel--system-message`; two consecutive sends yield exactly one block
  (idempotency, D2); extending the drawer scope between sends is reflected on the
  second; a non-chat-mode buffer is untouched (the mode guard).
- **Running:** `./bin/run-tests.sh -d config/gptel/chat` (or
  `-d config/gptel/chat/test/menu`).
- **Scenario mapping:** each spec scenario in
  `specs/gptel/environment-preamble.md` maps to one `it` above.

## Open Questions

- Length budget if scope lists ever grow long (soft; verbatim is fine today).
- Whether interactive (non-agent) chat ever wants the block suppressed — assume
  not; every chat-mode buffer benefits from knowing its cwd/scope.
