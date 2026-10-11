;;; bootstrap-seed-spec.el --- The static seed: the emacs-agent skill and its pointer -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D9 (L3) and
;; § Seams: seam/bootstrap-seed.
;;
;; What an agent reads from its loaded context cannot be driven from a
;; test; these contract specs read the two files that make up the seed —
;; the Claude Code skill and the one-line CLAUDE.md pointer, both in the
;; code root — and check what they must and must not say.  Pending until
;; the skill exists.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defconst bootstrap-seed-spec--skill
  (expand-file-name ".claude/skills/emacs-agent/SKILL.md" emacs-agent-test-root)
  "The emacs-agent skill.")

(defconst bootstrap-seed-spec--initial-tools
  '("org_graph_query" "org_graph_typed_edges" "org_graph_write_node"
    "search_roam_nodes" "list_roam_nodes" "read_roam_node"
    "get_roam_node_metadata" "query_roam_backlinks"
    "workspace_add_layout" "workspace_remove_layout"
    "emacs_describe_symbol" "emacs_find_definition"
    "emacs_variable_value" "emacs_apropos")
  "Every tool of the initial presets (control-surface-discoverability
§ Presets as Allowlists, D26).")

(defun bootstrap-seed-spec--read (file)
  "FILE's text."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))


(describe "The static seed (seam/bootstrap-seed)"

  (xit "names the wrapper, the target choice, the tiers and the tools command (Scenario: control-surface-discoverability § Agent learns the entry point from static context)"
    (let ((skill (bootstrap-seed-spec--read bootstrap-seed-spec--skill)))
      (expect skill :to-match "^name: emacs-agent$")
      (expect skill :to-match "^description: ")
      (dolist (needle '("bin/emacs-agent" "emacs-agent live" "emacs-agent target start"
                        "--worktree" "--data" "scratch:" "\\bcall\\b[^-]" "call-write"
                        "\\beval\\b" "\\btools\\b"))
        (expect (cons needle (and (string-match-p needle skill) t))
                :to-equal (cons needle t))))
    (expect (bootstrap-seed-spec--read (expand-file-name "CLAUDE.md" emacs-agent-test-root))
            :to-match "\\.claude/skills/emacs-agent"))

  (xit "names no tool of either preset (Scenario: control-surface-discoverability § Seed does not change when tools change)"
    (let ((skill (bootstrap-seed-spec--read bootstrap-seed-spec--skill))
          (names (append bootstrap-seed-spec--initial-tools
                         (and (fboundp 'jf/agent-preset-tool-names)
                              (append (jf/agent-preset-tool-names 'emacs-agent-read)
                                      (jf/agent-preset-tool-names 'emacs-agent-write))))))
      (dolist (name (delete-dups names))
        (expect (cons name (string-search name skill)) :to-equal (list name))))))

;;; bootstrap-seed-spec.el ends here
