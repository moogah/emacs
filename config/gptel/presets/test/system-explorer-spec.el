;;; system-explorer-spec.el --- Specs for the system-explorer preset -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral specs for the read-only `system-explorer' preset
;; (`config/gptel/presets/system-explorer/preset.el', tangled from preset.org).
;;
;; Covered contract:
;;   - The preset registers under the symbol `system-explorer' (name = source
;;     subdir basename) via the fragment-era registration pipeline.
;;   - Its :system is the role fragment rendered for the `claude' backend; it
;;     equals the committed golden snapshot byte-for-byte (static pre-render,
;;     register/invariant/static-prerender-dynamic-compose).
;;   - The native config carries the confirmed shape
;;     (register/shape/preset-config-plist): :backend the gptel backend name
;;     string "Claude", :model a symbol, a :description string.
;;   - It is READ-ONLY: its :scope-profile is the read-only "system-explorer"
;;     profile (extracted into `jf/gptel-preset--scope-defaults', stripped from
;;     the registration plist), and no write/modify-capable tool, :paths,
;;     :shell-commands, or :bash-tools is granted.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Resolve sub-module paths from this spec's location.
(defconst system-explorer-spec--presets-dir
  (expand-file-name
   "../" (file-name-directory (or load-file-name buffer-file-name)))
  "The `config/gptel/presets/' directory, relative to this spec.")

;; Load the renderer, the registration pipeline, and the golden reader.
(require 'jf-gptel-fragments
         (expand-file-name "fragments.el" system-explorer-spec--presets-dir))
(require 'gptel-preset-registration
         (expand-file-name "registration.el" system-explorer-spec--presets-dir))
(require 'presets-helpers-spec
         (expand-file-name "test/helpers-spec.el" system-explorer-spec--presets-dir))

;; Minimal stubs for the upstream gptel preset store so the suite is
;; self-contained when run directory-scoped (no full gptel load).
(unless (boundp 'gptel--known-presets)
  (defvar gptel--known-presets nil))
(unless (fboundp 'gptel-make-preset)
  (defun gptel-make-preset (name &rest keys)
    "Test stub mirroring upstream `gptel-make-preset' (idempotent by NAME)."
    (if-let* ((p (assoc name gptel--known-presets)))
        (setcdr p keys)
      (setq gptel--known-presets
            (nconc gptel--known-presets (list (cons name keys)))))))
(unless (fboundp 'gptel-get-preset)
  (defun gptel-get-preset (name)
    "Test stub mirroring upstream `gptel-get-preset'."
    (alist-get name gptel--known-presets nil nil #'equal)))

(defconst system-explorer-spec--preset-el
  (expand-file-name "system-explorer/preset.el" system-explorer-spec--presets-dir)
  "The tangled preset.el under test.")

;; Custom matcher: ACTUAL must be `member' of EXPECTED (a list).
(buttercup-define-matcher :to-be-weakly-equal-to-one-of (actual expected)
  (let ((a (funcall actual))
        (set (funcall expected)))
    (if (member a set)
        (cons t (format "Expected %S not to be one of %S" a set))
      (cons nil (format "Expected %S to be one of %S" a set)))))

(describe "system-explorer preset"

  ;; Load the tangled preset.el against a clean preset store + scope tables so
  ;; the registration side effects are deterministic.  preset.el renders its
  ;; role fragment at load time and self-registers.
  (before-each
    (setq gptel--known-presets nil)
    (setq jf/gptel-preset--scope-defaults nil)
    (setq jf/gptel-preset--mode-defaults nil)
    (load system-explorer-spec--preset-el nil t))

  (after-each
    (setq gptel--known-presets nil)
    (setq jf/gptel-preset--scope-defaults nil)
    (setq jf/gptel-preset--mode-defaults nil))

  (describe "registration"

    (it "registers under the `system-explorer' name symbol"
      (expect (gptel-get-preset 'system-explorer) :not :to-be nil))

    (it "carries a :description string for the menu / @mention surface"
      (let ((desc (plist-get (gptel-get-preset 'system-explorer) :description)))
        (expect (stringp desc) :to-be t)
        (expect (length desc) :to-be-greater-than 0)))

    (it "uses the Claude backend name string and a model symbol"
      (let ((spec (gptel-get-preset 'system-explorer)))
        ;; register/shape/preset-config-plist: :backend is the gptel backend
        ;; NAME STRING ("Claude") forwarded to gptel-make-preset (whose apply
        ;; path resolves a backend by name); :model is a symbol.
        (expect (plist-get spec :backend) :to-equal "Claude")
        (expect (symbolp (plist-get spec :model)) :to-be t)
        (expect (plist-get spec :model) :to-equal 'claude-sonnet-4-6))))

  (describe "pre-rendered :system (static pre-render)"

    (it "equals the golden Claude render byte-for-byte"
      ;; The role fragment renders at load time; registration forwards it as
      ;; :system verbatim (no per-send rendering).
      (expect (plist-get (gptel-get-preset 'system-explorer) :system)
              :to-equal (presets-test-read-golden "system-explorer.claude.txt")))

    (it "renders the role as Claude XML section blocks"
      (let ((sys (plist-get (gptel-get-preset 'system-explorer) :system)))
        (expect sys :to-match "\\`<role>\n")
        (expect sys :to-match "</role>")
        (expect sys :to-match "<constraints>")
        (expect sys :to-match "</constraints>\\'")))

    (it "describes how to reason, not live machine data"
      ;; Guardrail for the static/dynamic separation: the static role must not
      ;; bake in a captured machine snapshot (OS name, kernel string, paths).
      (let ((sys (downcase (plist-get (gptel-get-preset 'system-explorer) :system))))
        (expect sys :not :to-match "darwin")
        (expect sys :not :to-match "/usr/local/bin")
        (expect sys :not :to-match "homebrew"))))

  (describe "read-only contract"

    (it "extracts the read-only system-explorer scope profile"
      ;; Scope profile lands in the side table and is stripped from the
      ;; registration plist (register/boundary/preset-org-to-registration).
      (let ((scope (cdr (assq 'system-explorer jf/gptel-preset--scope-defaults))))
        (expect (plist-get scope :scope-profile) :to-equal "system-explorer"))
      (expect (plist-member (gptel-get-preset 'system-explorer) :scope-profile)
              :to-be nil))

    (it "grants no ad-hoc write/exec scope keys"
      ;; No :paths / :shell-commands / :bash-tools are authored, so no scope
      ;; defaults entry carries them.
      (let ((scope (cdr (assq 'system-explorer jf/gptel-preset--scope-defaults))))
        (expect (plist-member scope :paths) :to-be nil)
        (expect (plist-member scope :shell-commands) :to-be nil)
        (expect (plist-member scope :bash-tools) :to-be nil)))

    (it "names only read-only tools (no write/modify/delete/exec)"
      (let ((tools (plist-get (gptel-get-preset 'system-explorer) :tools)))
        (expect (listp tools) :to-be t)
        (expect tools :not :to-be nil)
        ;; Every tool is on the read-only allowlist.
        (dolist (tool tools)
          (expect tool
                  :to-be-weakly-equal-to-one-of
                  '("read_file" "list_directory"
                    "search_project_content" "list_project_files")))
        ;; And explicitly none of the write/exec-capable tools.
        (dolist (forbidden '("create_file" "delete_files" "execute_bash"
                             "execute_command" "run_bash_command"
                             "execute_sql_insert" "execute_sql_update"
                             "execute_sql_delete" "execute_sql_drop"))
          (expect (member forbidden tools) :to-be nil))))))

(provide 'gptel-preset-system-explorer-spec)

;;; system-explorer-spec.el ends here
