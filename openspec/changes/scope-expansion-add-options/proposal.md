## Why

The scope expansion UI currently offers one way to permanently add a resource to the plan: add the exact denied path. Users often need more control over what gets added — either broadening a specific path to a wildcard (`/**`) or crafting a custom pattern — without having to open scope.yml manually afterward.

## What Changes

- Add "Add parent `/**` to scope" option to the expansion transient menu, which appends `/**` to the directory containing the denied resource (or the path itself if it's a directory) before writing to scope.yml
- Add "Add custom pattern to scope" option that pre-populates an editable field with the denied resource string, lets the user modify it inline, then writes the edited value to scope.yml
- Both new options follow the existing "Add to scope" callback and routing contract (success response with `:patterns_added`)

## Capabilities

### New Capabilities
- None

### Modified Capabilities
- `gptel/scope-expansion`: Expansion UI gains two additional "add to scope" variants — wildcard and custom-edit — expanding the three-choice menu to a five-choice menu (Deny, Allow once, Add exact, Add `/**`, Add custom).

## Impact

- `config/gptel/scope/scope-expansion.org` — new transient suffix commands and menu layout changes
- `config/gptel/scope/scope-expansion.el` — generated output
- `config/gptel/scope/test/expansion/` — new or updated Buttercup specs for the two new options
- No changes to scope.yml update logic beyond reusing existing path-updater with a different input pattern
