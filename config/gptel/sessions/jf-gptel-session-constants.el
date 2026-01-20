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

(defconst jf/gptel-session--metadata-file "metadata.json"
  "File name for session metadata.")

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

(defconst jf/gptel-session--subagents-dir "subagents"
  "Directory name for subagent sessions within parent session.")

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

(defvar-local jf/gptel--parent-session-id nil
  "Session ID of parent session (for subagents only).
Nil for top-level sessions.")

(provide 'jf-gptel-session-constants)
;;; constants.el ends here
