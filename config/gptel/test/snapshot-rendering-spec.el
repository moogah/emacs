;;; snapshot-rendering-spec.el --- Buttercup specs for chat-mode snapshot rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the chat-mode snapshot extension delivered by
;; task `extend-render-drawer-text-with-preset-snapshot' (see
;; openspec/changes/gptel-drawer-as-source-of-truth/tasks/open/
;; extend-render-drawer-text-with-preset-snapshot.md, design.md
;; §Decision 4 and §Decision 2).
;;
;; Coverage:
;;   - `jf/gptel-scope-profile--snapshot-lines' emits each snapshot
;;     key when the preset spec carries a non-nil value, and omits
;;     each key when absent or wrong-typed.
;;   - `:GPTEL_SYSTEM:' is *never* emitted by the helper or the
;;     wrapping renderer/applicator (write-side of register/invariant/
;;     drawer-system-key-write-exclusion).
;;   - `--render-drawer-text' renders the snapshot keys above the
;;     scope keys, in the documented order, when PRESET-SPEC is
;;     supplied; legacy callers that omit PRESET-SPEC see the prior
;;     shape unchanged.
;;   - `--apply-to-drawer' writes the snapshot keys via `org-entry-put'
;;     and survives the round-trip via `org-entry-get-multivalued-property'
;;     for `:GPTEL_TOOLS:'.
;;   - Modify-list tool specs (`(:append (...))') are resolved against
;;     the current `gptel-tools' default at render time.
;;
;; Anchors:
;;   register/shape/drawer-text-block (divergent → reconciled)
;;   register/boundary/scope-profile-applicator (divergent → reconciled)
;;   register/invariant/drawer-system-key-write-exclusion (speculated →
;;     reconciled at integrate)

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'org)

(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (gptel-dir (expand-file-name "../" spec-dir)))
  (add-to-list 'load-path gptel-dir))

(require 'gptel)
(require 'gptel-scope-profiles)

(describe "jf/gptel-scope-profile--snapshot-lines"

  (it "returns nil when PRESET-SPEC is nil"
    (expect (jf/gptel-scope-profile--snapshot-lines nil) :to-be nil))

  (it "returns nil when PRESET-SPEC carries none of the snapshot keys"
    (expect (jf/gptel-scope-profile--snapshot-lines '(:system "ignored"))
            :to-be nil))

  (it "emits :GPTEL_MODEL: from a symbol model name"
    (expect (jf/gptel-scope-profile--snapshot-lines '(:model claude-sonnet-4-6))
            :to-equal '(":GPTEL_MODEL: claude-sonnet-4-6")))

  (it "emits :GPTEL_BACKEND: from a string backend"
    (expect (jf/gptel-scope-profile--snapshot-lines '(:backend "Claude"))
            :to-equal '(":GPTEL_BACKEND: Claude")))

  (it "emits :GPTEL_TEMPERATURE: as a string number"
    (expect (jf/gptel-scope-profile--snapshot-lines '(:temperature 0.7))
            :to-equal '(":GPTEL_TEMPERATURE: 0.7")))

  (it "emits :GPTEL_MAX_TOKENS: as a string number"
    (expect (jf/gptel-scope-profile--snapshot-lines '(:max-tokens 4096))
            :to-equal '(":GPTEL_MAX_TOKENS: 4096")))

  (it "emits :GPTEL_NUM_MESSAGES_TO_SEND: as a string number"
    (expect (jf/gptel-scope-profile--snapshot-lines
             '(:num-messages-to-send 8))
            :to-equal '(":GPTEL_NUM_MESSAGES_TO_SEND: 8")))

  (it "skips a temperature whose value is non-numeric"
    (expect (jf/gptel-scope-profile--snapshot-lines '(:temperature "warm"))
            :to-be nil))

  (it "emits multiple keys in the documented order: model, backend, tools, temp, max, num"
    (expect (jf/gptel-scope-profile--snapshot-lines
             '(:model claude-sonnet-4-6
               :backend "Claude"
               :tools (PersistentAgent run_bash_command)
               :temperature 0.7
               :max-tokens 4096
               :num-messages-to-send 8))
            :to-equal
            '(":GPTEL_MODEL: claude-sonnet-4-6"
              ":GPTEL_BACKEND: Claude"
              ":GPTEL_TOOLS: PersistentAgent run_bash_command"
              ":GPTEL_TEMPERATURE: 0.7"
              ":GPTEL_MAX_TOKENS: 4096"
              ":GPTEL_NUM_MESSAGES_TO_SEND: 8")))

  (it "NEVER emits :GPTEL_SYSTEM: even when :system is non-nil (write-side enforcement)"
    (let ((lines (jf/gptel-scope-profile--snapshot-lines
                  '(:model claude-sonnet-4-6
                    :system "You are a careful collaborator."))))
      (expect lines :to-equal '(":GPTEL_MODEL: claude-sonnet-4-6"))
      (expect (cl-some (lambda (line)
                         (string-match-p "^:GPTEL_SYSTEM:" line))
                       lines)
              :to-be nil)))

  (describe "tools resolution"
    (it "passes through a list of name symbols as space-joined string names"
      (expect (jf/gptel-scope-profile--snapshot-lines
               '(:tools (PersistentAgent run_bash_command)))
              :to-equal '(":GPTEL_TOOLS: PersistentAgent run_bash_command")))

    (it "passes through a list of name strings unchanged"
      (expect (jf/gptel-scope-profile--snapshot-lines
               '(:tools ("a" "b" "c")))
              :to-equal '(":GPTEL_TOOLS: a b c")))

    (it "resolves a (:append ...) modify-list spec against the current gptel-tools default"
      ;; `--resolve-tool-names' reads `(default-value 'gptel-tools)', which
      ;; ignores `let'-style dynamic shadowing on a `defcustom'.  We must
      ;; mutate the global default and restore it via `unwind-protect' so
      ;; the merge is exercised against the documented base list rather
      ;; than the batch-process default of nil (which would let the test
      ;; pass for the wrong reason — `(append nil '(extra-C))' equals
      ;; `(extra-C)').
      (let ((original (default-value 'gptel-tools)))
        (unwind-protect
            (progn
              (setq-default gptel-tools '(base-A base-B))
              (expect (jf/gptel-scope-profile--snapshot-lines
                       '(:tools (:append (extra-C))))
                      :to-equal
                      '(":GPTEL_TOOLS: base-A base-B extra-C")))
          (setq-default gptel-tools original))))

    (it "omits :GPTEL_TOOLS: when modifier resolution yields the empty list"
      (let ((original (default-value 'gptel-tools)))
        (unwind-protect
            (progn
              (setq-default gptel-tools nil)
              (expect (jf/gptel-scope-profile--snapshot-lines
                       '(:tools (:append nil)))
                      :to-be nil))
          (setq-default gptel-tools original))))))

(describe "jf/gptel-scope-profile--render-drawer-text with PRESET-SPEC"

  (it "embeds snapshot lines between :GPTEL_PRESET: and the scope keys"
    (let ((text (jf/gptel-scope-profile--render-drawer-text
                 'system-explorer nil
                 '(:paths (:read ("/tmp/**")))
                 '(:model claude-sonnet-4-6
                   :tools (PersistentAgent)
                   :temperature 0.5))))
      (expect text :to-match "^:PROPERTIES:\n")
      (expect text :to-match "\n:END:\n\\'")
      (expect text :to-match ":GPTEL_PRESET: system-explorer\n")
      (expect text :to-match ":GPTEL_MODEL: claude-sonnet-4-6\n")
      (expect text :to-match ":GPTEL_TOOLS: PersistentAgent\n")
      (expect text :to-match ":GPTEL_TEMPERATURE: 0.5\n")
      (expect text :to-match ":GPTEL_SCOPE_READ: /tmp/\\*\\*\n")
      ;; Order: snapshot block precedes scope block.
      (let ((model-pos (string-match ":GPTEL_MODEL:" text))
            (scope-pos (string-match ":GPTEL_SCOPE_READ:" text)))
        (expect model-pos :to-be-truthy)
        (expect scope-pos :to-be-truthy)
        (expect (< model-pos scope-pos) :to-be t))))

  (it "preserves the legacy shape when PRESET-SPEC is omitted"
    (let ((text (jf/gptel-scope-profile--render-drawer-text
                 'system-explorer nil
                 '(:paths (:read ("/tmp/**"))))))
      (expect (string-match-p ":GPTEL_MODEL:" text) :to-be nil)
      (expect (string-match-p ":GPTEL_BACKEND:" text) :to-be nil)
      (expect (string-match-p ":GPTEL_TOOLS:" text) :to-be nil)
      (expect text :to-match ":GPTEL_PRESET: system-explorer\n")
      (expect text :to-match ":GPTEL_SCOPE_READ:")))

  (it "NEVER renders :GPTEL_SYSTEM: even when PRESET-SPEC has a non-nil :system"
    (let ((text (jf/gptel-scope-profile--render-drawer-text
                 'system-explorer nil
                 '(:paths (:read ("/tmp/**")))
                 '(:model claude-sonnet-4-6
                   :system "Long multi-line system prompt with backticks `like this`."))))
      (expect (string-match-p ":GPTEL_SYSTEM:" text) :to-be nil)
      (expect text :to-match ":GPTEL_MODEL: claude-sonnet-4-6\n")))

  (it "omits the snapshot block when PRESET-SPEC carries only a :system"
    (let ((text (jf/gptel-scope-profile--render-drawer-text
                 'minimal nil nil
                 '(:system "Only system; no snapshot."))))
      (expect (string-match-p ":GPTEL_MODEL:" text) :to-be nil)
      (expect (string-match-p ":GPTEL_SYSTEM:" text) :to-be nil)
      (expect text :to-match ":GPTEL_PRESET: minimal\n"))))

(describe "jf/gptel-scope-profile--apply-to-drawer with PRESET-SPEC"

  (it "writes :GPTEL_MODEL: via org-entry-put"
    (with-temp-buffer
      (insert ":PROPERTIES:\n:GPTEL_PRESET: existing\n:END:\n\n")
      (org-mode)
      (jf/gptel-scope-profile--apply-to-drawer
       (current-buffer)
       '(:paths nil)
       '(:model claude-sonnet-4-6))
      (goto-char (point-min))
      (expect (org-entry-get (point-min) "GPTEL_MODEL")
              :to-equal "claude-sonnet-4-6")))

  (it "writes :GPTEL_TOOLS: as a multi-valued property that round-trips"
    (with-temp-buffer
      (insert ":PROPERTIES:\n:GPTEL_PRESET: existing\n:END:\n\n")
      (org-mode)
      (jf/gptel-scope-profile--apply-to-drawer
       (current-buffer)
       '(:paths nil)
       '(:tools (PersistentAgent run_bash_command)))
      (goto-char (point-min))
      (expect (org-entry-get-multivalued-property (point-min) "GPTEL_TOOLS")
              :to-equal '("PersistentAgent" "run_bash_command"))))

  (it "writes :GPTEL_TEMPERATURE: / :GPTEL_MAX_TOKENS: / :GPTEL_NUM_MESSAGES_TO_SEND: as scalars"
    (with-temp-buffer
      (insert ":PROPERTIES:\n:GPTEL_PRESET: existing\n:END:\n\n")
      (org-mode)
      (jf/gptel-scope-profile--apply-to-drawer
       (current-buffer)
       '(:paths nil)
       '(:temperature 0.7 :max-tokens 4096 :num-messages-to-send 8))
      (goto-char (point-min))
      (expect (org-entry-get (point-min) "GPTEL_TEMPERATURE") :to-equal "0.7")
      (expect (org-entry-get (point-min) "GPTEL_MAX_TOKENS") :to-equal "4096")
      (expect (org-entry-get (point-min) "GPTEL_NUM_MESSAGES_TO_SEND")
              :to-equal "8")))

  (it "NEVER writes :GPTEL_SYSTEM: even when PRESET-SPEC has :system"
    (with-temp-buffer
      (insert ":PROPERTIES:\n:GPTEL_PRESET: existing\n:END:\n\n")
      (org-mode)
      (jf/gptel-scope-profile--apply-to-drawer
       (current-buffer)
       '(:paths nil)
       '(:model claude-sonnet-4-6 :system "Should never round-trip."))
      (goto-char (point-min))
      (expect (org-entry-get (point-min) "GPTEL_SYSTEM") :to-be nil)
      (expect (org-entry-get (point-min) "GPTEL_MODEL")
              :to-equal "claude-sonnet-4-6")))

  (it "preserves existing non-snapshot keys (GPTEL_PRESET, GPTEL_PARENT_SESSION_ID)"
    (with-temp-buffer
      (insert ":PROPERTIES:\n:GPTEL_PRESET: existing\n:GPTEL_PARENT_SESSION_ID: parent-1\n:END:\n\n")
      (org-mode)
      (jf/gptel-scope-profile--apply-to-drawer
       (current-buffer)
       '(:paths nil)
       '(:model claude-sonnet-4-6))
      (expect (org-entry-get (point-min) "GPTEL_PRESET")
              :to-equal "existing")
      (expect (org-entry-get (point-min) "GPTEL_PARENT_SESSION_ID")
              :to-equal "parent-1")
      (expect (org-entry-get (point-min) "GPTEL_MODEL")
              :to-equal "claude-sonnet-4-6"))))

(describe "round-trip idempotency: render → write → re-apply same spec"
  ;; Anchors register/boundary/scope-profile-applicator's cross-stage
  ;; invariant: writing the rendered text verbatim to a fresh buffer
  ;; and re-applying the same spec via --apply-to-drawer must leave
  ;; the buffer unchanged (modulo trailing whitespace).

  (it "rendered text equals applied buffer for the same scope-plist + preset-spec"
    (let* ((scope-plist '(:paths (:read ("/tmp/**" "/home/**"))))
           (preset-spec '(:model claude-sonnet-4-6
                          :tools (PersistentAgent run_bash_command)
                          :temperature 0.7))
           (rendered (jf/gptel-scope-profile--render-drawer-text
                      'system-explorer nil scope-plist preset-spec)))
      (with-temp-buffer
        (insert rendered)
        (org-mode)
        ;; Re-applying the same spec should leave the drawer with the
        ;; same key-value pairs (idempotent).
        (jf/gptel-scope-profile--apply-to-drawer
         (current-buffer) scope-plist preset-spec)
        (expect (org-entry-get (point-min) "GPTEL_MODEL")
                :to-equal "claude-sonnet-4-6")
        (expect (org-entry-get-multivalued-property (point-min) "GPTEL_TOOLS")
                :to-equal '("PersistentAgent" "run_bash_command"))
        (expect (org-entry-get (point-min) "GPTEL_TEMPERATURE")
                :to-equal "0.7")
        (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/tmp/**" "/home/**"))
        ;; Drawer singleton invariant carries over (only one :PROPERTIES: line).
        (let ((content (buffer-substring-no-properties
                        (point-min) (point-max))))
          (expect (cl-count-if
                   (lambda (line)
                     (string-match-p "^:PROPERTIES:[ \t]*$" line))
                   (split-string content "\n"))
                  :to-equal 1))))))

(provide 'snapshot-rendering-spec)
;;; snapshot-rendering-spec.el ends here
