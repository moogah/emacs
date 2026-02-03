;;; constants.el --- GPTEL Session Constants -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Centralized constants for gptel session management.
;; Defines directory paths, file names, and configuration variables.

;;; Code:

(require 'cl-lib)

(defcustom jf/gptel-sessions-directory "~/.gptel/sessions/"
  "Directory where gptel sessions are stored.
Each session is a subdirectory containing conversation history,
metadata, tool logs, and branching information."
  :type 'directory
  :group 'gptel)

(defcustom jf/gptel-presets-directory
  (expand-file-name "config/gptel/presets/" jf/emacs-dir)
  "Directory containing preset template files.
Users can create custom presets by copying and editing template files."
  :type 'directory
  :group 'gptel)

(defconst jf/gptel-session--context-file "session.md"
  "File name for main conversation context.
Uses markdown format (gptel's native format).")

(defconst jf/gptel-session--tools-log-file "tools.org"
  "File name for tool call log.
Uses org-mode format for better structure and linking.")

(defconst jf/gptel-session--system-prompts-file "system-prompts.org"
  "File name for system prompt change log.
Uses org-mode format for better structure.")

(defconst jf/gptel-session--current-link "current"
  "Name of symlink pointing to current conversation position.")

(defconst jf/gptel-session--agents-dir "agents"
  "Directory name for agent sessions within parent session.")

(defcustom jf/gptel-autosave-idle-time 0.5
  "Idle time in seconds before auto-saving session buffer.
Set to 0 to disable auto-save."
  :type 'number
  :group 'gptel)

(defvar-local jf/gptel-autosave-enabled nil
  "Buffer-local flag controlling auto-save for this session.")

(defvar-local jf/gptel--session-id nil
  "Unique identifier for the current gptel session.
Format: <slug>-<timestamp>, e.g., 'react-refactoring-20260120153042'.")

(defvar-local jf/gptel--session-dir nil
  "Absolute path to the directory for the current session.
All session files (context, metadata, tools log, etc.) are stored here.")

(defvar-local jf/gptel--branch-name nil
  "Name of the current branch within the session.
Typically 'main' for the primary branch, or timestamped names for alternate branches.")

(defvar-local jf/gptel--branch-dir nil
  "Absolute path to the current branch directory.
This is where preset.md, scope-plan.yml, and session.md files are located.
For regular sessions: <session-dir>/branches/<branch-name>/
For agents: <session-dir>/ (agents don't use branch subdirectories).")

(defvar-local jf/gptel--parent-session-id nil
  "Session ID of parent session (for agents only).
Nil for top-level sessions.")

(provide 'gptel-session-constants)
;;; constants.el ends here
