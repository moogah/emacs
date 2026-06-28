## ADDED Requirements

### Requirement: Display-Free Frame State as Structured Data

The system SHALL provide a `jf/frame-state` function, loadable into a target, that
returns the target's observable state as machine-parseable structured data (JSON), so
that a client with no display can inspect and assert on the target's state. The
observation MUST function against a headless target (one with no graphical frame). The
returned state SHALL include at least: the current buffer's name and contents, the
window configuration (which buffers are displayed), the position of point, the
minibuffer state (active or not, and its contents when active), and recent echo-area
messages. The function SHALL be invocable over the control channel via
`emacsclient -s <name> --eval '(jf/frame-state)'`.

#### Scenario: Frame state returned for a headless target

- **WHEN** a client calls `jf/frame-state` against a running headless target
- **THEN** the system returns structured data describing the current buffer, window
  configuration, point, minibuffer state, and recent echo-area messages

#### Scenario: State is machine-parseable for assertions

- **WHEN** a client receives a `jf/frame-state` result
- **THEN** the result is JSON the client can parse and assert on directly, without
  requiring a screenshot or a display

### Requirement: Bounded Observation Output

The `jf/frame-state` function SHALL bound the size of returned buffer contents so that
an arbitrarily large buffer does not produce an unbounded response. When content is
truncated, the result SHALL indicate that truncation occurred.

#### Scenario: Large buffer content is truncated with an indicator

- **WHEN** a target's current buffer is larger than the observation size limit and a
  client calls `jf/frame-state`
- **THEN** the returned contents are truncated to the limit and the result indicates
  that truncation occurred

### Requirement: Screenshot as an Optional Secondary Observation

The system SHALL treat screenshots as an optional, secondary observation path: it MAY
provide a screenshot capability, and when a target has a graphical frame and that
capability is invoked, the system SHALL return an image of that frame. A screenshot
SHALL NOT be required for headless targets, for which `jf/frame-state` is the
observation contract.

#### Scenario: Screenshot available only with a graphical frame

- **WHEN** the screenshot capability is invoked against a target that has a graphical
  frame
- **THEN** the system returns an image of the frame

#### Scenario: Headless target relies on frame-state

- **WHEN** a target is headless and a client needs to observe it
- **THEN** `jf/frame-state` provides the observation, and the absence of a screenshot
  does not prevent observation
