# gptel/chat-mode (delta)

## MODIFIED Requirements

### Requirement: Mode definition and activation

The system SHALL define `gptel-chat-mode` as a major mode derived from `org-mode`. Activation SHALL be possible via `M-x gptel-chat-mode`, file-local mode cookies (`-*- gptel-chat -*-`), and **content-addressed recognition through `magic-mode-alist`**.

The system SHALL register one `magic-mode-alist` entry whose predicate (the *session signature*) recognizes a gptel session by its content, not by its filename or path:

- The predicate SHALL match a buffer whose first non-blank content at `point-min` is an Org `:PROPERTIES:` drawer that contains at least one property key whose name begins with `GPTEL_` (e.g. `:GPTEL_PRESET:`, `:GPTEL_SESSION_ID:`).
- The predicate SHALL be anchored to a *real* drawer at `point-min` (a `:PROPERTIES:` line followed, before its `:END:`, by a `:GPTEL_…:` property line). Prose that merely quotes the string `:GPTEL_PRESET:` inside body text SHALL NOT match.
- The predicate SHALL inspect only the buffer head (no whole-buffer scan) and SHALL be safe on non-org buffers (no parse error).

Because `set-auto-mode` consults `magic-mode-alist` before `auto-mode-alist`, a signature-bearing `.org` file SHALL open in `gptel-chat-mode` rather than the default `org-mode`. As `gptel-chat-mode` derives from `org-mode`, all org features remain available.

The system SHALL NOT register any `auto-mode-alist` entry of its own (activation is content-addressed, not filename-addressed). Users MAY still add their own `auto-mode-alist` pattern.

#### Scenario: Interactive activation
- **WHEN** running `M-x gptel-chat-mode` in a buffer
- **THEN** the major mode becomes `gptel-chat-mode`
- **AND** the buffer inherits `org-mode` features
- **AND** chat-mode keybindings are active

#### Scenario: File-local cookie activation
- **WHEN** opening a file with first line `# -*- gptel-chat -*-`
- **THEN** the buffer is in `gptel-chat-mode` after load

#### Scenario: Content-addressed activation by session signature
- **WHEN** any `.org` file is opened (via `find-file`, `find-file-noselect`, dired, recentf, or a bookmark) whose `point-min` `:PROPERTIES:` drawer carries a `:GPTEL_`-prefixed key
- **THEN** `magic-mode-alist` selects `gptel-chat-mode` as the major mode
- **AND** the selection wins over the default `.org → org-mode` mapping

#### Scenario: Signature does not false-match a quoting org file
- **WHEN** opening an ordinary org file that mentions the text `:GPTEL_PRESET:` only inside a paragraph or source block (not in a `point-min` `:PROPERTIES:` drawer)
- **THEN** the session signature predicate returns nil
- **AND** the file opens in ordinary `org-mode`

#### Scenario: Old session without identity keys still activates
- **WHEN** opening a pre-existing `session.org` whose drawer carries `:GPTEL_PRESET:` but no `:GPTEL_SESSION_ID:`
- **THEN** the signature still matches (any `:GPTEL_`-prefixed key qualifies)
- **AND** the buffer activates in `gptel-chat-mode`
