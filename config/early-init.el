;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jeff Farr

;; Author: Jeff Farr <jefffarr@molokai>
;; Keywords:

;;; Worktree Support
;; Allow user-emacs-directory to be set via environment or --eval
;; This enables worktrees to specify their own runtime directories
(unless (bound-and-true-p user-emacs-directory-override-done)
  ;; Check if already set by launch script
  (when (getenv "EMACS_USER_DIRECTORY")
    (setq user-emacs-directory (file-name-as-directory (getenv "EMACS_USER_DIRECTORY"))))
  (setq user-emacs-directory-override-done t))

;;; Variables
(defvar enable-debug-p nil
  "Non-nil to enable debug.")

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024))))

;;; Performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((lexical-binding)))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Without this, Emacs will try to resize itself to a specific column size
(setq frame-inhibit-implied-resize t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
;; No second pass of case-insensitive search over auto-mode-alist.
(setq auto-mode-case-fold nil)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the
;; dashboard) is more than enough, and faster to display.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)
(setq initial-buffer-choice nil
      inhibit-startup-buffer-menu t)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Give up some bidirectional functionality for slightly faster re-display.
(setq bidi-inhibit-bpa t)

;; Remove "For information about GNU Emacs..." message at startup
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Suppress the vanilla startup screen completely. We've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

 ;; Activate `native-compile'
(setq native-comp-jit-compilation t
      package-native-compile t)
;;
;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-warning-on-missing-source enable-debug-p)

(setq debug-on-error enable-debug-p
      jka-compr-verbose enable-debug-p)

(setq byte-compile-warnings enable-debug-p)
(setq byte-compile-verbose enable-debug-p)

;;; Disable unneeded UI elements

;; Disable startup screens and messages
(setq inhibit-splash-screen t)


(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(tooltip-mode -1)

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)  ; Never show the hello file
