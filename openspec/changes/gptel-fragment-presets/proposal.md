## Why

The gptel presets (markdown files with YAML frontmatter, body sent verbatim as
the system prompt) predate the current understanding of system-prompt best
practices and have drifted into ad-hoc, freeform blobs. Separately, the runtime
framing that wraps every system message — the chat "Emacs prelude", the agent
"sub-agent preamble", and the dynamic environment block — is hard-coded in
`defconst`/`defun` form, so it cannot be edited or composed without touching
elisp. Both problems are the same problem viewed from two angles: everything
that flows into the system message should be an **editable, composable,
file-sourced fragment**, authored once and rendered to a backend-appropriate
form. This change establishes that model and proves it by migrating the existing
hard-coded pieces and authoring two fresh, best-practice presets on top of it.

## What Changes

- **New fragment/composition/section model.** A *composition* (what a preset is)
  is config plus an ordered list of *fragments*. A *fragment* is one editable
  `.org` source file containing one or more *sections* (org headings). A
  *renderer* maps each section to a backend-appropriate delimited block
  (Claude → XML tags; the renderer takes `backend` as a parameter, with Claude
  the only implementation initially). A *composer* assembles the ordered
  fragment list into the effective system message.
- **Static fragments pre-render at tangle time** into committed text artifacts;
  **dynamic fragments evaluate at compose time** (the existing environment-block
  cost profile). Dynamic placement is *supported anywhere* but *guided to the
  tail* for prompt-cache friendliness, and "do not use a dynamic fragment
  without good reason" is documented guidance.
- **Migrate the three hard-coded pieces to fragment files**: the chat
  `gptel-chat--emacs-prelude` (static), the persistent-agent
  `jf/gptel-persistent-agent--system-preamble` (static), and the
  `gptel-chat--build-environment-block` (dynamic) become fragment sources rather
  than `defconst`/`defun` literals. Existing composition behavior (prelude
  leads, env tails) is preserved.
- **Presets become `.org` sources that tangle to `.el`.** Config lives in an
  elisp config block (no YAML); the role fragment's static content pre-renders
  to text; the tangled `.el` registers via `gptel-make-preset`. **BREAKING:**
  the YAML-frontmatter `.md` preset pipeline in `preset-registration` is removed
  (parse / snake→kebab normalize / YAML-type coercion / plist scope+mode
  extraction all go away — net deletion).
- **Fresh-start the preset set.** Remove the existing `.md` presets (after
  verifying no snapshot/count test depends on the presets directory). Author two
  new presets on the new model:
  - `workspace-assistant` — general-purpose workspace helper; replaces
    `executor` as the workspace package's initial preset (flip
    `jf/gptel-workspace-initial-preset`). Tool wiring for the workspace command
    palette is a near-future follow-up, out of scope here.
  - `system-explorer` — read-only environment analyst (installed packages,
    available commands, configuration); no write/modify operations.
- **Pre-rendered sibling preserved.** The per-send `system-prompt.<ext>` sibling
  remains pre-rendered text read verbatim (today's behavior); dynamic content
  lives only in dedicated dynamic fragments.

## Capabilities

### New Capabilities
- `prompt-fragments`: The fragment / composition / section model — fragment
  source format (`.org`, sections, static vs dynamic), the backend-parametrized
  renderer (section → delimited block), the composer that assembles an ordered
  fragment list into the effective system message, context-derived default
  compositions (chat vs agent) with an explicit-override seam, and the
  tangle-time pre-render / compose-time evaluate contract.

### Modified Capabilities
- `gptel/preset-registration`: Replace the YAML `.md` frontmatter pipeline with
  the fragment pipeline — presets authored as `.org`, config in an elisp block,
  tangled to `.el`, registered via `gptel-make-preset`; static role content
  pre-rendered; scope/mode carried through the new format.
- `gptel/chat-mode`: The static Emacs prelude is sourced from a fragment file
  rather than the `gptel-chat--emacs-prelude` defconst; composition is expressed
  as the context-default fragment list.
- `gptel/persistent-agent`: The sub-agent preamble is sourced from a fragment
  file rather than the `jf/gptel-persistent-agent--system-preamble` defconst.
- `environment-preamble`: The environment block becomes a dynamic fragment
  (evaluated at the existing pre-send seam) rather than the
  `gptel-chat--build-environment-block` defun; tail placement and drawer-sourced
  content are preserved.

## Impact

- **Code:** `config/gptel/preset-registration.org/el` (pipeline rewrite),
  `config/gptel/chat/menu.org/el` (prelude + composer), `config/gptel/tools/
  persistent-agent.org/el` (preamble), `config/gptel/presets/` (delete `.md`,
  add `.org` sources + tangled `.el`), new fragment files + renderer + composer
  module(s), `config/gptel/sessions/workspace-integration.org/el`
  (`jf/gptel-workspace-initial-preset` default flip).
- **Dependencies:** Removes the `yaml.el` dependency from the preset pipeline
  (verify no other consumer).
- **Tests:** `preset-registration` tests rewritten for the fragment pipeline;
  new renderer/composer/fragment specs; chat-mode, persistent-agent, and
  environment-preamble specs updated for fragment sourcing; verify preset
  snapshot/count tests before deleting old presets.
- **Out of scope:** Multi-backend rendering beyond Claude (renderer takes
  `backend` as a seam only), workspace command-palette tool wiring, dynamic
  fragments at non-tail positions (allowed by the model, not exercised here).
