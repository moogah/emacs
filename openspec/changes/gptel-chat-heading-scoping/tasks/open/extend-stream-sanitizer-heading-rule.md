---
name: extend-stream-sanitizer-heading-rule
description: Add column-0 heading escape to gptel-chat--sanitize-chunk
change: gptel-chat-heading-scoping
status: ready
relations:
  - blocked-by:add-content-indentation-defcustom
  - enables:add-parser-heading-unescape
---

## Files to modify

- `config/gptel/chat/stream.org` (and tangled `stream.el`)
- `config/gptel/chat/test/stream/streaming-spec.el` (extend existing specs)
- `config/gptel/chat/test/parser/escape-round-trip-spec.el` (extend round-trip specs)

## Implementation steps

1. In `stream.org`, modify `gptel-chat--sanitize-chunk` to apply two rules per line, in this order:
   a. If the line matches `gptel-chat--end-delimiter-regexp` (existing rule), prepend `,`.
   b. Else if the line matches `^\*+ `, prepend `(make-string gptel-chat-content-indentation ?\s)`.
   c. Else return the line unchanged.
2. The two rules are mutually exclusive (a line cannot start with both `#+end_` and `*`), so the `else` chain is correct.
3. Update the function's docstring to describe both rules and their inverses.
4. Add Buttercup specs in `streaming-spec.el` covering:
   - Single-line `* Heading` chunk → escaped output.
   - Multi-level `*** Deep` → escaped with one prefix (the prefix doesn't change with star count).
   - Mixed chunk: `* Heading\n#+end_assistant\nplain text` → first two lines escaped per their respective rules, third unchanged.
   - Heading split across chunks: `\n* He` + `ading\n` → completed line escaped before insertion.
   - Negative: `not* a heading` (not column 0) → unchanged.
   - Negative: `*no-space` (no trailing space) → unchanged (the heading regex requires a space).
5. Add round-trip specs in `escape-round-trip-spec.el` mirroring the existing pattern: a body containing escaped headings round-trips back to the original through sanitize → parse → message.

## Design rationale

The streaming sanitizer is the high-frequency write path; per-line scan with two predicates is the cheapest design. See design.md Decision 4.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes (existing + new specs).
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes (existing + new round-trip specs).
- Manual check: `grep -n 'gptel-chat--sanitize-chunk' config/gptel/chat/stream.el` shows the function with both rules.

## Context

- `openspec/changes/gptel-chat-heading-scoping/design.md` Decision 4.
- Existing sanitizer: `config/gptel/chat/stream.el:46`.
- Existing round-trip tests: `config/gptel/chat/test/parser/escape-round-trip-spec.el`.

## Observations

- **Round-trip specs are sanitize-side-only for headings.** The task body asks
  for round-trip specs in `escape-round-trip-spec.el` "mirroring the existing
  pattern: a body containing escaped headings round-trips back to the original
  through sanitize → parse → message."  The parse-side inverse (strip leading
  whitespace before `*` lines) is owned by sibling task
  `add-parser-heading-unescape` and does not exist in the parser yet.  The
  added specs therefore pin the SANITIZE-SIDE shape, idempotence, and width
  honouring of the heading rule alongside the existing delimiter inverse-pair
  section, plus a body-level mixed-rule integration check.  The full
  parse → message → resanitize → reparse cycle for heading lines is left as a
  follow-on commit when the parser un-escape lands; the file's commentary
  explicitly notes that this section is half of the inverse pair until then.
  This is a deviation from the literal task body wording, but the alternative
  (write tests that fail until the next task lands) breaks the verification
  command in this task and would block the wave's merge.

- **Tests now require `gptel-chat-mode`.**  The sanitizer reads
  `gptel-chat-content-indentation` (defcustom in `mode.el`).  Both
  `streaming-spec.el` and `escape-round-trip-spec.el` now `(require
  'gptel-chat-mode)` so the variable is bound at its defcustom default during
  the run.  Production load order already loads `mode.el` before `stream.el`
  (per `chat.org`), so this only affects the test environment.  As an
  additional safety net, `stream.el` carries a `(defvar gptel-chat-content-indentation)`
  forward declaration (silences byte-compiler) and the function falls back to
  `1` via `bound-and-true-p` if the defcustom is somehow not loaded — the `1`
  literal duplicates the defcustom default and is documented as such; if the
  default is ever changed, both must be updated.  This duplication is a small
  invariant-gap (recorded as a discovery below) but is the cheapest way to
  preserve the chat.org "no cross-module require" rule while keeping the
  sanitizer safe to call from any context.

- **`gptel-chat--heading-collision-regexp` is named for parity with
  `gptel-chat--end-delimiter-regexp`.**  The sibling task
  `add-parser-heading-unescape` will add an inverse `^[ \t]+\*+ ` regexp to
  the parser; naming the write-side regexp explicitly (rather than inlining
  `^\\*+ ` in the function) makes the inverse pair greppable.

- **Negative case `*` (bare star, no trailing space) is covered defensively.**
  The heading regex requires a trailing space, so a bare `*` is preserved.
  Less obvious is that org list bullets often DO use `*` with text on the
  same line (`* item`), which IS the same as a heading at column 0.  The
  collision is unavoidable: org cannot distinguish a column-0 unordered-list
  bullet from a heading without context.  Inside chat blocks we treat all
  column-0 `*+ ` lines as headings-to-escape, since list bullets that DO
  appear inside chat content end up indented (`-` or `+`) per common
  conventions.  The negative spec for `  * already-indented` documents that
  list bullets indented past column 0 pass through unchanged.

## Discoveries

- discovery_id: disc-extend-stream-sanitizer-heading-rule-1
  class: invariant-gap
  description: |
    `stream.el` carries a fallback default of `1` for
    `gptel-chat-content-indentation` (via `(or (bound-and-true-p ...) 1)` in
    the sanitizer) that duplicates the defcustom default in `mode.el`.  The
    duplication exists to keep the sanitizer safe to call from a test
    context that loads `stream.el` standalone, while honouring the
    chat.org load-time rule "no chat-mode module issues a cross-module
    `require`".  If the defcustom default is ever changed (e.g., to 2 for
    `org-edit-src-content-indentation` parity), `stream.el` must be
    updated in lockstep — there is no automated check.
  affected_register_entry: register/boundary/chat-heading-collision-escape
  recommendation: |
    Two viable cleanups, in increasing order of disruption:
    (a) Add a regression spec in `streaming-spec.el` that asserts
        `gptel-chat-content-indentation`'s defcustom default equals the
        literal in `stream.el` (one assertion, three lines).
    (b) Promote `gptel-chat-content-indentation` to a `defvar` in a
        small new shared module loaded before `mode.el` and
        `stream.el`, then have `mode.el` add the customize metadata
        via `defcustom` on the same symbol (defcustom is idempotent
        when the variable is already bound, only the doc/customize
        info is updated).
    (b) is cleaner but adds a new module purely to host a single
    constant; (a) is the cheap safeguard.

- discovery_id: disc-extend-stream-sanitizer-heading-rule-2
  class: spec-signal
  description: |
    The task body asks for full sanitize → parse → message round-trip
    specs for heading-collision lines, but the parser un-escape that
    closes the loop is owned by the sibling task
    `add-parser-heading-unescape`.  This task's `enables:`
    relationship (`enables: add-parser-heading-unescape`) confirms the
    ordering: this task lands first, the parser un-escape lands
    after.  The two together would let the round-trip succeed; either
    alone cannot.  The current task body therefore overshoots its
    own scope by one sibling task.
  affected_register_entry: register/boundary/chat-heading-collision-escape
  recommendation: |
    When `add-parser-heading-unescape` lands, extend the "Full
    pipeline" section of `escape-round-trip-spec.el` with three
    heading-line round-trip specs mirroring the existing delimiter
    coverage:
      1. A `* Heading` line inside an assistant body round-trips to
         the bare `* Heading` after parse + un-escape.
      2. All three escape variants (` * H`, `   * H`, ` ** sub`)
         round-trip to their bare forms regardless of indent width.
      3. A mixed body containing both a heading and an escaped
         delimiter round-trips both rules in a single pass.
    The current `## Observations` and the file commentary are
    explicit pointers for that follow-up.

- discovery_id: disc-extend-stream-sanitizer-heading-rule-3
  class: scope-question
  description: |
    The heading regex `^\*+ ` matches both org headings AND
    column-0 `*` list bullets.  Inside chat blocks, the sanitizer
    cannot distinguish them and treats both identically (escape
    with leading whitespace).  This is correct behaviour for the
    corruption-fix goal: any column-0 `*+ ` line breaks
    `org-element-parse-buffer`'s view of the special block.  But
    it means a model that emits an unindented Markdown bullet
    list will see those bullets escaped to ` * item` on disk.
    The parser un-escape (sibling task) will strip that leading
    space on send, so the model never sees the artefact.
  affected_register_entry: register/invariant/chat-block-body-no-column-zero-stars
  recommendation: |
    No change recommended.  Document the behaviour in the parent
    change's spec ("Heading-collision escape" scenario) so future
    readers understand the bullet-vs-heading collision is intentional.
    The alternative (try to distinguish bullets from headings) is
    fundamentally undecidable without parsing context the sanitizer
    deliberately does not have (it is a per-line helper).
