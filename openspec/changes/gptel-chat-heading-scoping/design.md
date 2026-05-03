# Design: Chat-mode heading scoping

## Context

`research.md` establishes the empirical findings; this document records the design decisions for the implementation.

## Decision 1: Escape mechanism — leading-space, not `,*`

**Choice:** column-0 `*` lines inside chat blocks are escaped by prepending whitespace (default: a single space, configurable via `gptel-chat-content-indentation`). The on-disk form is ` * Heading` rather than `,* Heading`.

**Rationale:**

- **Babel parity.** `org-edit-src-content-indentation` (default `2`) is the established mechanism org uses to keep `*` lines from collapsing src blocks. Using the same mechanism for chat blocks gives users a familiar mental model and keeps the door open for a future `gptel-chat-edit-turn` indirect-editing kernel that would mirror `org-edit-src-content-indentation`'s strip-on-entry, indent-on-exit behavior.
- **Lower visual noise at high frequency.** Heading escape is expected to happen on every assistant response that uses headings — much more frequent than the existing `,#+end_*` escape, which only triggers when assistants talk about chat-mode itself. A leading space is nearly invisible; `,*` is conspicuous on every line.
- **Composes with future visual font-lock.** A follow-up change can apply `org-level-N` faces to lines matching `^[ \t]+\*+ ` inside chat blocks. The leading-space form survives this overlay cleanly. The `,*` form would render the comma as a visible artifact under any face.

**Alternatives considered:**

- **`,*` (uniform comma escape).** Pros: single coherent escape mechanism with the existing `,#+end_*`. One round-trip mental model. Visually obvious. Cons: high visual noise at high frequency; loses babel parity; harder to overlay heading faces later.
- **Mixed (leading-space for headings, `,` for delimiters).** This is what this design proposes. The two collision rules are different in shape (one is "this line would close a block," the other is "this line would be a heading"), so different escapes are not incoherent — they encode different intents.

**Implications:**

- The streaming sanitizer becomes a multi-rule scanner: per line, check `#+end_*` (apply `,`) and check `^\*+ ` (apply leading whitespace). Order: `,` first if applicable (the `#+end_*` rule), then heading rule. A line cannot match both rules (a line starting with `#+end_` cannot also start with `*`).
- The parser un-escape becomes two steps: strip leading-space prefix from `*` lines, strip leading `,` from `#+end_*` lines. Two passes or one combined regex; either is fine.
- The `gptel-chat-content-indentation` defcustom value affects only *new* writes. Existing content with the old default is honored on read regardless. The migration step (Decision 5) normalizes to the current value.

## Decision 2: User-typed escape via `post-self-insert-hook`

**Choice:** when the user types `*` at column 0 with point inside a chat-block body and not on a delimiter line, a `post-self-insert-hook` function inserts the configured escape prefix immediately before the `*`.

**Rationale:**

- `post-self-insert-hook` runs after every self-inserting character. Cheapest place to catch the keypress path.
- The cursor naturally lands at column N+1 after the escape is inserted, which matches user expectation ("I typed `*`, the cursor moved past it"). The escape character before is visually subtle and does not require additional cursor motion logic.
- Errors here are recoverable: undo restores the pre-insert state, including the escape prefix. The user can disable `post-self-insert-hook` per buffer if they're doing exotic editing.

**Alternatives considered:**

- **`before-change-functions`.** Runs before any buffer mutation, including programmatic insertion. Catches more paths (paste, yank, kill-region replacement) in one place. Rejected as the *primary* hook because it runs too aggressively (every keystroke in the buffer), increasing the cost of the predicate "am I inside a chat block body?" — the predicate would need to be O(1) via cached region info. For paste/yank, we use `after-change-functions` (Decision 3) which is cheaper than maintaining `before-change-functions` per-keystroke.
- **`org-mode` indentation hooks (`indent-line-function`, `electric-indent-functions`).** Only fire on explicit indent commands (`TAB`, `RET` with electric indent). Don't catch typing on existing lines.

**Implications:**

- The "am I inside a chat-block body?" predicate runs once per typed character. Must be cheap. We provide a fast `gptel-chat--point-in-block-body-p` helper that scans backward for the nearest `#+begin_*` / `#+end_*` line (line-by-line `re-search-backward`), bounded by a small look-back window or by `point-min`. Typical chat blocks are short enough that backward-scan is sub-millisecond.
- Delimiter-line exclusion: the predicate returns nil if point is on a `#+begin_*` or `#+end_*` line itself (column-0 `*` on a `#+begin_*` line is impossible anyway, since the line starts with `#`).

## Decision 3: Paste / yank handling via `after-change-functions`

**Choice:** an `after-change-functions` filter detects when an insertion into a chat-block body contains `*` at column 0 of any line within the inserted range, and rewrites those lines to apply the escape.

**Rationale:**

- `after-change-functions` runs after the change is in the buffer, with `(beg end length)`. For an insertion, `length` is 0 and `[beg, end)` is the inserted text range. The filter inspects that range for column-0 `*` lines and applies the escape in a second pass.
- Catches paste, yank, programmatic `insert` calls from non-streaming code paths, kill-region paste, drag-and-drop, mouse paste, etc. — every insertion path.
- The streaming sanitizer is functionally redundant under this hook (the streamer also calls `insert`), but is kept as a separate path because the streamer can apply the escape per-line as chunks arrive without inspecting the whole inserted range — slightly faster for the high-frequency streaming path and more explicit about intent.

**Alternatives considered:**

- **Advise `insert` and friends.** Heavier-handed; couples to a general Emacs primitive.
- **Per-yank-handler.** Would only catch yank, not paste from external sources.

**Implications:**

- The hook runs on every buffer change, including changes made by the hook itself. The implementation must be idempotent (re-running on already-escaped content is a no-op) AND must guard against infinite recursion. Standard pattern: bind `inhibit-modification-hooks` while the filter applies its rewrites.
- The predicate "is this insertion inside a chat-block body?" runs once per change. Same fast helper as Decision 2.
- For `(beg end)` ranges that span a chat-block boundary (e.g., a paste that crosses `#+end_user`), only the portion inside the body is escaped. The portion on or past the delimiter is left alone. Implementation: clip the range to the body extent before applying the escape.

## Decision 4: Streaming sanitizer rule order

**Choice:** the existing `gptel-chat--sanitize-chunk` function is extended with the heading rule. Order of checks per line: `#+end_*` first (existing), `^\*+ ` second (new). The two rules are mutually exclusive at the line level, so order is not load-bearing for correctness — it's chosen for code clarity.

**Rationale:**

- Mutually exclusive: a line cannot start with both `#+end_` and `*`. Either matches or neither, never both.
- Keeping the existing rule first preserves the read of the function for anyone familiar with the v1 sanitizer. The new rule appends.

**Alternatives considered:**

- **Two separate sanitizers.** Cleaner separation but doubles the per-line function-call overhead in the hot streaming path. Not worth the cleanliness.

**Implications:**

- Existing tests for the delimiter-collision escape still pass unchanged.
- New tests for the heading escape live alongside in `escape-round-trip-spec.el`, mirroring the existing structure.

## Decision 5: Migration on read, not on save

**Choice:** when `gptel-chat-mode` activates on a buffer with unescaped column-0 `*` lines inside chat blocks, the mode applies the escape on-read as part of activation. The buffer becomes modified (only if migration changed anything); the next `save-buffer` persists the normalized form. No separate migration command, no on-disk rewrite ahead of opening the file.

**Rationale:**

- **Reversibility.** The user can always discard the buffer modification (`M-x revert-buffer`) and the on-disk file is unchanged. A destructive on-disk migration cannot be undone by a worried user without restoring from backup.
- **No discovery.** Existing sessions already on disk get fixed the next time they're opened, with no scan-the-filesystem ceremony.
- **Local correctness wins.** The buffer's `org-element` view is correct from the moment the mode activates. Without migration, `org-element-parse-buffer` on an existing session produces a corrupted AST, and any code path that consults `org-element` (font-lock, fold, future indirect-edit) sees the broken state.

**Alternatives considered:**

- **One-time migration command.** Sweeps the sessions directory, rewrites files in place. Rejected because (a) it requires the user to know it exists, (b) it's destructive without backup, (c) every newly-discovered session needs the same treatment anyway.
- **Lazy migration only on edit.** Would leave existing sessions in their broken state until a per-buffer trigger fires. Bad: the parser still sees the broken AST when the buffer first opens.

**Implications:**

- Mode activation does a single pass: walk turn blocks (the existing `gptel-chat-parse-buffer` already enumerates them), and for each block body, scan for `^\*+ ` lines and apply the escape if any are found.
- `set-buffer-modified-p t` is called only if the migration actually changed the buffer. A clean session opens clean.
- This adds a small cost to mode activation. For typical chat session sizes (tens of turns, KB-scale body content) it's well under 100ms.

## Decision 6: Defer indirect editing (`gptel-chat-edit-turn`)

**Choice:** the indirect-editing kernel mirroring `org-edit-comment-block` is identified by `research.md` as the natural "real heading work inside a turn" affordance. It is **not** part of this change. A follow-up change introduces it, reusing the leading-space normalization as the on-disk format.

**Rationale:**

- The corruption bug is fixed by the escape mechanism alone. Indirect editing is an ergonomic upgrade for the rare "I want real heading affordances inside this turn" workflow, not a correctness requirement.
- Shipping the escape first lets users write and read sessions correctly today. The indirect-edit kernel can ship separately when the UX warrants it.
- Reusing `org-src--edit-element` (which already strips/applies indentation) means deferring the kernel does not strand any architectural choice — the escape mechanism is already the right shape.

**Alternatives considered:**

- **Ship both together.** Larger change surface, longer review, unnecessarily blocks the corruption fix on UX work that has open questions (Path γ comparison, etc.).

**Implications:**

- The chat-mode spec carveout reads: "headings inside chat blocks render as escaped paragraph text; real heading affordances require [follow-up change name TBD]." The follow-up change can revise the spec wording when it lands.

## Decision 7: Defer visual font-lock for escaped headings

**Choice:** rendering escaped `*` lines with `org-level-N` faces is **not** part of this change. The escaped lines render as their literal text (paragraph face).

**Rationale:**

- Correctness ships first; visual polish ships second.
- Font-lock rules for "match `^[ \t]+\*+ ` inside chat-block bodies, apply `org-level-N` per `*` count" are isolated and small (~30-50 lines), but require careful interaction with org's existing font-lock machinery and live-update behavior. Not worth coupling to the correctness fix.

**Alternatives considered:**

- **Ship visual rendering with the escape.** Convenient, but adds review surface and complicates rollback if the font-lock interaction has edge cases.

**Implications:**

- Users will see literal ` * Heading` in their chat sessions until the visual font-lock change lands. This is a minor cosmetic regression compared to the current (broken) state where `* Heading` renders as a real heading but destroys the chat block.

## Decision 8: Escape character is configurable via defcustom

**Choice:** `gptel-chat-content-indentation` (defcustom, `:type 'natnum`, default `1`) controls how many spaces of escape are applied. Default `1` is the minimum that breaks the heading regex; `2` matches `org-edit-src-content-indentation` exactly. The type is `natnum` (non-negative integer) rather than `integer` because the consumer `(make-string gptel-chat-content-indentation ?\s)` signals `wrong-type-argument wholenump` on negatives — pushing the type check to `customize-variable` time keeps the failure local to the misconfiguration rather than crashing at the first column-0 `*` write.

**Rationale:**

- A single space is sufficient (the org heading regex is `^\*+ ` anchored at column 0; any leading whitespace breaks it).
- Defaulting to `1` minimizes visual noise. Users who want symmetry with their src-block indentation can set it to `2`.
- A defcustom (rather than hardcoded) is cheap to add and matches the existing chat-mode style of exposing tunable knobs.

**Alternatives considered:**

- **Default `2` to match babel exactly.** Defensible. Either is acceptable; default `1` is the principled minimum.
- **No defcustom; hardcode to 1.** Slightly simpler. Loses the ability to switch to `2` later without a code change.

**Implications:**

- The parser un-escape strips *any* amount of leading whitespace before a `*` line, not exactly `gptel-chat-content-indentation` spaces. This makes round-trip robust against config changes and against legacy content with different indent widths.
- The migration-on-read step (Decision 5) applies the *current* value of `gptel-chat-content-indentation`. A buffer that was written with the value `1` and is re-opened under `2` gets re-normalized.

## Open coherence check

Two design decisions are reversible if real-world use turns up surprises:

1. Decision 1 (leading-space vs `,*`). The escape character is a single uniform choice; switching it later is a migration script. Easier to do early.
2. Decision 8 (defcustom default). Trivial to change.

Decisions 2 (`post-self-insert-hook`), 3 (`after-change-functions`), 4 (sanitizer extension), 5 (migration on read), 6 (defer indirect-edit), 7 (defer visual font-lock) are independently revisable and don't lock in architectural choices that prevent later evolution.
