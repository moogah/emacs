## ADDED Requirements

### Requirement: Fragment source format

A *fragment* SHALL be a single `.org` source file that contributes content to a
composed system message. A fragment SHALL contain one or more *sections*, where
a section is a top-level Org heading whose heading text names the section and
whose body is the section content. A fragment SHALL declare whether it is
**static** (its rendered text is fixed at tangle time) or **dynamic** (its text
is produced by evaluating a function at compose time). A fragment that declares
no kind SHALL default to static.

#### Scenario: Fragment sections come from top-level Org headings
- **WHEN** a static fragment source contains the top-level headings `Role`, `Background`, and `Constraints`
- **THEN** the fragment is parsed into three sections named `Role`, `Background`, and `Constraints`
- **AND** each section's content is the body beneath its heading

#### Scenario: Fragment kind defaults to static
- **WHEN** a fragment source declares no kind
- **THEN** the fragment is treated as static

#### Scenario: A fragment may carry a single section
- **WHEN** a fragment source has exactly one section
- **THEN** it is a valid fragment contributing that one section

### Requirement: Section rendering is backend-parametrized

The renderer SHALL map each section to a backend-appropriate delimited block,
taking the target `backend` as a parameter. For the `claude` backend a section
named `N` with body `B` SHALL render as an XML block: an opening tag derived
from `N`, the body `B`, and the matching closing tag. The renderer SHALL be
structured so that additional backends can be added without changing fragment
sources; only the `claude` rendering is implemented by this change. The renderer
SHALL NOT emit Markdown by default — the output format is whatever the target
backend's rendering specifies.

#### Scenario: Claude rendering wraps each section in an XML tag
- **WHEN** a section named `Role` with body `You are X.` is rendered for the `claude` backend
- **THEN** the output is the body `You are X.` enclosed in an opening and matching closing XML tag derived from `Role`

#### Scenario: Backend is an explicit parameter
- **WHEN** the renderer is invoked
- **THEN** it requires a `backend` argument and selects the rendering for that backend

#### Scenario: Unimplemented backend is reported, not silently mis-rendered
- **WHEN** the renderer is invoked for a backend with no implemented rendering
- **THEN** it signals or logs that the backend rendering is unavailable rather than emitting Claude/Markdown output as if it were correct

### Requirement: Composition assembles an ordered fragment list

A *composition* SHALL be an ordered list of fragments. The composer SHALL
produce the effective system message by rendering each fragment in list order
and joining the rendered fragments into a single string. The relative order of
fragments in the list SHALL be preserved in the output.

#### Scenario: Fragments appear in list order
- **WHEN** a composition lists fragments `[A, B, C]`
- **THEN** the composed system message contains A's content, then B's, then C's, in that order

#### Scenario: Composition of a single fragment
- **WHEN** a composition lists exactly one fragment
- **THEN** the composed system message is that fragment's rendered text

### Requirement: Context-derived default composition with an override seam

The default composition SHALL be derived from the send context. An interactive
chat-mode context SHALL default to `[emacs-prelude (static), role, environment
(dynamic)]`. A persistent-agent context SHALL default to `[agent-preamble
(static), role, environment (dynamic)]`. A preset author SHALL normally provide
only config plus a role fragment and rely on the context default; the system
SHALL provide a seam by which a composition MAY override the default list (add,
remove, or reorder fragments) without requiring that of the common case.

#### Scenario: Chat context uses the prelude-led default
- **WHEN** a system message is composed for an interactive chat-mode buffer
- **THEN** the composition is `emacs-prelude`, then the role, then the environment fragment

#### Scenario: Agent context uses the preamble-led default
- **WHEN** a system message is composed for a persistent-agent buffer
- **THEN** the composition is `agent-preamble`, then the role, then the environment fragment

#### Scenario: Author supplies only a role fragment
- **WHEN** a preset declares config and a role fragment and no explicit composition override
- **THEN** the effective composition is the context default with that preset's role fragment in the role position

### Requirement: Static fragments pre-render at tangle time

A static fragment SHALL be rendered to committed text at tangle time, so that
its rendered artifact is a reviewable, version-controlled file and no rendering
work occurs at compose time for static fragments. The composer SHALL consume the
pre-rendered text for static fragments rather than re-rendering them per send.

#### Scenario: Static fragment yields a committed text artifact
- **WHEN** a static fragment `.org` source is tangled
- **THEN** a rendered text artifact is produced and tracked in version control

#### Scenario: Composing a static fragment does no rendering
- **WHEN** a static fragment is included in a composition at send time
- **THEN** the composer uses its pre-rendered text and performs no section rendering for it

### Requirement: Dynamic fragments evaluate at compose time

A dynamic fragment SHALL produce its text by evaluating a function when the
system message is composed. The default position for a dynamic fragment SHALL be
the **tail** of the composition, preserving prompt-cache friendliness (the
static prefix stays stable across sends). The model SHALL permit a dynamic
fragment at any position, but the documented guidance SHALL be to place dynamic
fragments at the tail and to introduce a dynamic fragment only when a static one
will not do.

#### Scenario: Dynamic fragment text is produced at compose time
- **WHEN** a composition that includes a dynamic fragment is composed twice with differing live inputs
- **THEN** each composition reflects the live input at the time it was composed

#### Scenario: Dynamic fragment defaults to the tail
- **WHEN** a dynamic fragment is added to a context default composition without an explicit position
- **THEN** it is placed at the tail, after the static prefix and the role

#### Scenario: Non-tail placement is permitted by the mechanism
- **WHEN** a composition explicitly places a dynamic fragment ahead of the tail
- **THEN** the composer honors that placement
- **AND** this remains discouraged by documented guidance
