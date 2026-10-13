# Activities State (retired)

This directory used to hold the state file of the `activities.el` package
(`activities-activities`, written by `persist.el`, one subdirectory per
machine role). Only this README is in git.

## Nothing writes here any more

- The `activities` package and the `config/activities/` module were removed
  from this configuration (commit `9950063c`, "Cutover: remove activities,
  perspective, and activities-integration"). Workspaces
  (`config/workspaces/`) replaced them; their state lives under
  `state/workspaces/{machine-role}/`.
- Nothing in the configuration sets `persist-location` for
  `activities-activities` or reads this directory.

## Not in git

`.gitignore` ignores everything under `state/` except directories and
`README.md` files, so any `activities-activities` file left here by an older
checkout is local to that checkout, with no git history or backup. The
activity files that were once committed were removed from git in commit
`fba22a99` and are only in the history before it.
