;;; two-layer-contracts-spec.el --- Architecture contracts for Option A two-layer separation -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract tests for the Option A architecture: clean separation of
;; grammar decomposition (Layer 0) from semantic extraction (Layer 1).
;;
;; Architecture overview:
;;   Layer 0 (Grammar): Pure decomposition of compounds into simple commands
;;     with accumulated var-context + grammar-level redirection extraction.
;;     Does NOT call command handlers.
;;   Layer 1 (Semantics): Command handlers called exactly once per simple
;;     command, receiving the simple command with full accumulated context.
;;     Produces all domain operations (:filesystem, :authentication, :network, ...).
;;   Merge: Combines Layer 0 redirections + Layer 1 handler ops, calculates coverage.
;;
;; Test organization:
;;   Section 1: Layer 0 decomposition contracts (tests jf/bash--decompose-to-simple-commands)
;;   Section 2: Layer 0 purity contracts (decomposition must not call handlers)
;;   Section 3: Layer 1 dispatch contracts (handlers called once per simple command)
;;   Section 4: End-to-end contracts (observable behavior of jf/bash-extract-semantics)
;;
;; These tests define the TARGET architecture. They should:
;;   - FAIL or ERROR now (proving current bugs)
;;   - PASS when the Option A refactor is complete

;;; Code:

(require 'cl-lib)
(require 'bash-parser-orchestrator)
(require 'bash-parser-semantics)
(require 'bash-parser)

(defvar two-layer--saved-handlers nil
  "Saved handler registry for test isolation.")

;; ═══════════════════════════════════════════════════════════════════════
;; Section 1: Layer 0 — Decomposition Contracts
;;
;; These test jf/bash--decompose-to-simple-commands, the pure grammar
;; function that walks compound structures and yields simple commands
;; with accumulated context.
;;
;; Expected interface:
;;   (jf/bash--decompose-to-simple-commands parsed-command &optional var-context)
;;   => list of plists, each:
;;      (:command <simple-cmd-plist>
;;       :var-context <alist>
;;       :redirection-ops <list of operation plists>
;;       :metadata <plist with context flags>)
;; ═══════════════════════════════════════════════════════════════════════

(describe "Layer 0: Grammar decomposition"

  (describe "simple commands"

    (it "yields one entry for a simple command"
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed)))
        (expect (length entries) :to-equal 1)
        (expect (plist-get (plist-get (car entries) :command) :command-name)
                :to-equal "cat")))

    (it "includes redirection operations in the entry"
      (let* ((parsed (jf/bash-parse "cat file.txt > output.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             (redir-ops (plist-get (car entries) :redirection-ops)))
        (expect redir-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :write)
                                (eq (plist-get op :source) :redirection)))
                         redir-ops)
                :to-be-truthy)))

    (it "extracts all redirection types from a single command"
      (let* ((parsed (jf/bash-parse "cat input.txt > output.txt 2> error.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             (redir-ops (plist-get (car entries) :redirection-ops)))
        ;; stdout redirect
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :write)))
                         redir-ops)
                :to-be-truthy)
        ;; stderr redirect
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "error.txt")
                                (eq (plist-get op :operation) :write)))
                         redir-ops)
                :to-be-truthy)))

    (it "extracts append redirections"
      (let* ((parsed (jf/bash-parse "echo hello >> log.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             (redir-ops (plist-get (car entries) :redirection-ops)))
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "log.txt")
                                (eq (plist-get op :operation) :append)))
                         redir-ops)
                :to-be-truthy)))

    (it "extracts input redirections"
      (let* ((parsed (jf/bash-parse "sort < unsorted.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             (redir-ops (plist-get (car entries) :redirection-ops)))
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "unsorted.txt")
                                (eq (plist-get op :operation) :read)))
                         redir-ops)
                :to-be-truthy))))

  (describe "pipelines"

    (it "yields one entry per pipeline segment"
      (let* ((parsed (jf/bash-parse "cat file.txt | grep pattern | wc -l"))
             (entries (jf/bash--decompose-to-simple-commands parsed)))
        (expect (length entries) :to-equal 3)
        (let ((names (mapcar (lambda (e)
                               (plist-get (plist-get e :command) :command-name))
                             entries)))
          (expect names :to-equal '("cat" "grep" "wc")))))

    (it "preserves per-segment redirections"
      (let* ((parsed (jf/bash-parse "cat file.txt | grep pattern > matches.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             ;; grep segment should have the redirection
             (grep-entry (cl-find-if
                          (lambda (e)
                            (equal "grep"
                                   (plist-get (plist-get e :command) :command-name)))
                          entries))
             (redir-ops (plist-get grep-entry :redirection-ops)))
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "matches.txt")
                                (eq (plist-get op :operation) :write)))
                         redir-ops)
                :to-be-truthy))))

  (describe "chains"

    (it "yields one entry per chain command"
      (let* ((parsed (jf/bash-parse "rm temp.txt && touch new.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed)))
        (expect (length entries) :to-equal 2)
        (let ((names (mapcar (lambda (e)
                               (plist-get (plist-get e :command) :command-name))
                             entries)))
          (expect names :to-equal '("rm" "touch")))))

    (it "accumulates var-context across chain commands"
      (let* ((parsed (jf/bash-parse "DIR=/tmp && ls $DIR"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             ;; Second command (ls) should have DIR in its var-context
             (ls-entry (nth 1 entries))
             (ctx (plist-get ls-entry :var-context)))
        (expect (alist-get 'DIR ctx) :to-equal "/tmp")))

    (it "tracks directory changes across chain commands"
      (let* ((parsed (jf/bash-parse "cd /tmp && cat file.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             ;; cat should have PWD=/tmp in its var-context
             (cat-entry (cl-find-if
                         (lambda (e)
                           (equal "cat"
                                  (plist-get (plist-get e :command) :command-name)))
                         entries))
             (ctx (plist-get cat-entry :var-context)))
        (expect (alist-get 'PWD ctx) :to-equal "/tmp")))

    (it "preserves redirections within chain commands"
      (let* ((parsed (jf/bash-parse "cat f1.txt > combined.txt; cat f2.txt >> combined.txt"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             (all-redir-ops (mapcan (lambda (e)
                                     (copy-sequence (plist-get e :redirection-ops)))
                                   entries)))
        ;; First command: > write
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "combined.txt")
                                (eq (plist-get op :operation) :write)))
                         all-redir-ops)
                :to-be-truthy)
        ;; Second command: >> append
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "combined.txt")
                                (eq (plist-get op :operation) :append)))
                         all-redir-ops)
                :to-be-truthy))))

  (describe "nested structures"

    (it "yields entries for subshell contents"
      (let* ((parsed (jf/bash-parse "(cat file.txt; rm temp.txt)"))
             (entries (jf/bash--decompose-to-simple-commands parsed)))
        (expect (length entries) :to-be-greater-than 1)
        (let ((names (mapcar (lambda (e)
                               (plist-get (plist-get e :command) :command-name))
                             entries)))
          (expect (member "cat" names) :to-be-truthy)
          (expect (member "rm" names) :to-be-truthy))))

    (it "marks subshell entries with metadata"
      (let* ((parsed (jf/bash-parse "(cat file.txt)"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             (metadata (plist-get (car entries) :metadata)))
        (expect (plist-get metadata :subshell-context) :to-be-truthy)))

    (it "yields entries for conditional branches"
      (let* ((parsed (jf/bash-parse "if [ -f foo.txt ]; then cat foo.txt; else touch foo.txt; fi"))
             (entries (jf/bash--decompose-to-simple-commands parsed)))
        ;; Should have entries from both then and else branches
        (let ((names (mapcar (lambda (e)
                               (plist-get (plist-get e :command) :command-name))
                             entries)))
          (expect (member "cat" names) :to-be-truthy)
          (expect (member "touch" names) :to-be-truthy))))

    (it "marks conditional entries with branch metadata"
      (let* ((parsed (jf/bash-parse "if [ -f foo.txt ]; then cat foo.txt; else touch foo.txt; fi"))
             (entries (jf/bash--decompose-to-simple-commands parsed))
             (cat-entry (cl-find-if
                         (lambda (e)
                           (equal "cat"
                                  (plist-get (plist-get e :command) :command-name)))
                         entries)))
        (when cat-entry
          (let ((metadata (plist-get cat-entry :metadata)))
            (expect (plist-get metadata :conditional) :to-be-truthy)
            (expect (plist-get metadata :branch) :to-equal :then)))))))


;; ═══════════════════════════════════════════════════════════════════════
;; Section 2: Layer 0 — Purity Contracts
;;
;; The decomposition function must NOT call command handlers.
;; This is the key architectural property of Option A.
;; ═══════════════════════════════════════════════════════════════════════

(describe "Layer 0: Purity"

  (before-each
    (setq two-layer--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers two-layer--saved-handlers))

  (it "decomposition does not call jf/bash-extract-command-semantics"
    (spy-on 'jf/bash-extract-command-semantics :and-call-through)
    (jf/bash--decompose-to-simple-commands (jf/bash-parse "cat file.txt"))
    (expect 'jf/bash-extract-command-semantics :not :to-have-been-called))

  (it "decomposition does not call jf/bash-extract-command-semantics for compounds"
    (spy-on 'jf/bash-extract-command-semantics :and-call-through)
    (jf/bash--decompose-to-simple-commands (jf/bash-parse "cat file.txt | grep pattern && rm temp.txt"))
    (expect 'jf/bash-extract-command-semantics :not :to-have-been-called))

  (it "decomposition does not call jf/bash-extract-operations-from-positional-args"
    (spy-on 'jf/bash-extract-operations-from-positional-args :and-call-through)
    (jf/bash--decompose-to-simple-commands (jf/bash-parse "cat file.txt && rm temp.txt"))
    (expect 'jf/bash-extract-operations-from-positional-args :not :to-have-been-called)))


;; ═══════════════════════════════════════════════════════════════════════
;; Section 3: Layer 1 — Handler Dispatch Contracts
;;
;; Command handlers must be called exactly once per simple command,
;; never for compound command types, and with accumulated var-context.
;; Verified via spies on jf/bash-extract-command-semantics.
;; ═══════════════════════════════════════════════════════════════════════

(describe "Layer 1: Handler dispatch"

  (before-each
    (setq two-layer--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers two-layer--saved-handlers))

  (it "calls handlers once for a simple command"
    (spy-on 'jf/bash-extract-command-semantics :and-call-through)
    (jf/bash-extract-semantics (jf/bash-parse "cat file.txt"))
    (expect 'jf/bash-extract-command-semantics :to-have-been-called-times 1))

  (it "calls handlers once per simple command in a pipeline"
    (spy-on 'jf/bash-extract-command-semantics :and-call-through)
    (jf/bash-extract-semantics (jf/bash-parse "cat file.txt | grep pattern"))
    (expect 'jf/bash-extract-command-semantics :to-have-been-called-times 2))

  (it "calls handlers once per simple command in a chain"
    (spy-on 'jf/bash-extract-command-semantics :and-call-through)
    (jf/bash-extract-semantics (jf/bash-parse "rm temp.txt && touch new.txt"))
    (expect 'jf/bash-extract-command-semantics :to-have-been-called-times 2))

  (it "calls handlers once per simple command in a semicolon list"
    (spy-on 'jf/bash-extract-command-semantics :and-call-through)
    (jf/bash-extract-semantics (jf/bash-parse "cat a.txt; rm b.txt; touch c.txt"))
    (expect 'jf/bash-extract-command-semantics :to-have-been-called-times 3))

  (it "calls handlers for each segment of a mixed compound"
    ;; pipeline within a chain: (cat | grep) && rm
    (spy-on 'jf/bash-extract-command-semantics :and-call-through)
    (jf/bash-extract-semantics (jf/bash-parse "cat file.txt | grep pattern && rm temp.txt"))
    (expect 'jf/bash-extract-command-semantics :to-have-been-called-times 3))

  (it "never passes a compound command-type to handlers"
    ;; Capture all calls and verify each received a simple command
    (let ((received-types '()))
      (spy-on 'jf/bash-extract-command-semantics
              :and-call-fake
              (lambda (parsed-command)
                (push (plist-get parsed-command :type) received-types)
                (list :domains nil :claimed-token-ids nil)))
      (jf/bash-extract-semantics (jf/bash-parse "cat file.txt | grep pattern && rm temp.txt"))
      ;; No call should receive :pipeline or :chain type
      (expect (cl-some (lambda (type) (memq type '(:pipeline :chain))) received-types)
              :to-be nil)))

  (it "passes command with non-nil :command-name to handlers"
    ;; Every handler call should have a concrete command-name
    (let ((received-names '()))
      (spy-on 'jf/bash-extract-command-semantics
              :and-call-fake
              (lambda (parsed-command)
                (push (plist-get parsed-command :command-name) received-names)
                (list :domains nil :claimed-token-ids nil)))
      (jf/bash-extract-semantics (jf/bash-parse "rm temp.txt && touch new.txt"))
      (expect (length received-names) :to-equal 2)
      ;; Every name should be non-nil
      (expect (cl-every #'identity received-names) :to-be-truthy))))


;; ═══════════════════════════════════════════════════════════════════════
;; Section 4: End-to-end Contracts
;;
;; Observable behavior of jf/bash-extract-semantics under Option A.
;; These test what the user sees — correct output regardless of internals.
;; ═══════════════════════════════════════════════════════════════════════

(describe "End-to-end: Redirection survival"
  :var (two-layer--saved-handlers)

  (before-each
    (setq two-layer--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers two-layer--saved-handlers))

  (it "handler command with redirect: cat input.txt > output.txt"
    ;; Redirections must survive even when the command has a registered handler.
    ;; This is the critical contract — redirections are grammar-level (Layer 0),
    ;; completely independent of handler dispatch (Layer 1).
    (let* ((result (jf/bash-extract-semantics (jf/bash-parse "cat input.txt > output.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      ;; Handler produces :read for input.txt
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "input.txt")
                              (eq (plist-get op :operation) :read)))
                       fs-ops)
              :to-be-truthy)
      ;; Layer 0 produces :write for output.txt
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "output.txt")
                              (eq (plist-get op :operation) :write)))
                       fs-ops)
              :to-be-truthy)))

  (it "handler command with append: cat input.txt >> output.txt"
    (let* ((result (jf/bash-extract-semantics (jf/bash-parse "cat input.txt >> output.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "output.txt")
                              (eq (plist-get op :operation) :append)))
                       fs-ops)
              :to-be-truthy)))

  (it "handler command with stderr: grep pattern file.txt 2> errors.txt"
    (let* ((result (jf/bash-extract-semantics (jf/bash-parse "grep pattern file.txt 2> errors.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "errors.txt")
                              (eq (plist-get op :operation) :write)))
                       fs-ops)
              :to-be-truthy)))

  (it "no-handler command with redirect: echo hello > file.txt"
    ;; Commands without registered handlers still get redirection extraction.
    (let* ((result (jf/bash-extract-semantics (jf/bash-parse "echo hello > file.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "file.txt")
                              (eq (plist-get op :operation) :write)))
                       fs-ops)
              :to-be-truthy)))

  (it "redirect survives in pipeline: cat file.txt | grep pattern > matches.txt"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "cat file.txt | grep pattern > matches.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "matches.txt")
                              (eq (plist-get op :operation) :write)))
                       fs-ops)
              :to-be-truthy)))

  (it "redirects survive in chain: cat f1 > out1; cat f2 >> out1"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "cat f1.txt > combined.txt; cat f2.txt >> combined.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      ;; :write from first command
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "combined.txt")
                              (eq (plist-get op :operation) :write)))
                       fs-ops)
              :to-be-truthy)
      ;; :append from second command
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "combined.txt")
                              (eq (plist-get op :operation) :append)))
                       fs-ops)
              :to-be-truthy))))


(describe "End-to-end: Multi-domain completeness"
  :var (two-layer--saved-handlers)

  (before-each
    (setq two-layer--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers two-layer--saved-handlers))

  (it "simple aws command produces :authentication and :network domains"
    (let* ((result (jf/bash-extract-semantics (jf/bash-parse "aws s3 ls")))
           (domains (plist-get result :domains)))
      (expect (assq :authentication domains) :to-be-truthy)
      (expect (assq :network domains) :to-be-truthy)))

  (it "aws in a chain produces :authentication domain"
    ;; This is the critical multi-domain compound test.
    ;; Current bug: orchestrator calls handlers on compound (no :command-name),
    ;; so non-filesystem domains from subcommands are lost.
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "aws s3 cp file.txt s3://bucket/ && rm file.txt")))
           (domains (plist-get result :domains)))
      (expect (assq :authentication domains) :to-be-truthy)
      (expect (assq :network domains) :to-be-truthy)
      ;; Also: filesystem ops from both commands
      (let ((fs-ops (alist-get :filesystem domains)))
        ;; aws's :read of file.txt
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "file.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)
        ;; rm's :delete of file.txt
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "file.txt")
                                (eq (plist-get op :operation) :delete)))
                         fs-ops)
                :to-be-truthy))))

  (it "aws in a pipeline produces :authentication domain"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "cat config.txt | aws s3 cp - s3://bucket/")))
           (domains (plist-get result :domains)))
      (expect (assq :authentication domains) :to-be-truthy)
      (expect (assq :network domains) :to-be-truthy)))

  (it "multiple cloud commands in chain produce domains from all"
    ;; Both aws and gcloud should contribute their domains
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "aws s3 ls && gcloud storage ls")))
           (auth-ops (alist-get :authentication (plist-get result :domains))))
      ;; Should have auth entries from both providers
      (expect (length auth-ops) :to-be-greater-than 1)
      (expect (cl-some (lambda (op) (eq (plist-get op :provider) :aws)) auth-ops)
              :to-be-truthy)
      (expect (cl-some (lambda (op) (eq (plist-get op :provider) :gcloud)) auth-ops)
              :to-be-truthy))))


(describe "End-to-end: Compound completeness"
  :var (two-layer--saved-handlers)

  (before-each
    (setq two-layer--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers two-layer--saved-handlers))

  (it "chain: rm temp.txt && touch new.txt"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "rm temp.txt && touch new.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "temp.txt")
                              (eq (plist-get op :operation) :delete)))
                       fs-ops)
              :to-be-truthy)
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "new.txt")
                              (memq (plist-get op :operation) '(:create :create-or-modify))))
                       fs-ops)
              :to-be-truthy)))

  (it "chain: cp source.txt backup.txt && rm source.txt"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "cp source.txt backup.txt && rm source.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      ;; cp: read + write
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "source.txt")
                              (eq (plist-get op :operation) :read)))
                       fs-ops)
              :to-be-truthy)
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "backup.txt")
                              (eq (plist-get op :operation) :write)))
                       fs-ops)
              :to-be-truthy)
      ;; rm: delete
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "source.txt")
                              (eq (plist-get op :operation) :delete)))
                       fs-ops)
              :to-be-truthy)))

  (it "pipeline: cat file.txt | grep pattern > output.txt"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "cat file.txt | grep pattern > output.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      ;; cat: read
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "file.txt")
                              (eq (plist-get op :operation) :read)))
                       fs-ops)
              :to-be-truthy)
      ;; redirect: write
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "output.txt")
                              (eq (plist-get op :operation) :write)))
                       fs-ops)
              :to-be-truthy)))

  (it "chain with redirections: cat f1 > combined; cat f2 >> combined"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "cat f1.txt > combined.txt; cat f2.txt >> combined.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      ;; Should have 4+ operations: 2 reads + 1 write + 1 append
      (expect (length fs-ops) :to-be-greater-than 3))))


(describe "End-to-end: Coverage accuracy"
  :var (two-layer--saved-handlers)

  (before-each
    (setq two-layer--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers two-layer--saved-handlers))

  (it "simple command claims tokens (non-zero coverage)"
    (let* ((result (jf/bash-extract-semantics (jf/bash-parse "cat file.txt")))
           (coverage (plist-get result :coverage)))
      (expect (plist-get coverage :claimed-tokens) :to-be-greater-than 0)))

  (it "redirection tokens are claimed"
    (let* ((result (jf/bash-extract-semantics (jf/bash-parse "cat input.txt > output.txt")))
           (coverage (plist-get result :coverage))
           (unclaimed (plist-get coverage :unclaimed-tokens)))
      ;; No redirection tokens should be unclaimed
      (expect (cl-remove-if-not
               (lambda (tok) (eq (plist-get tok :type) :redirection))
               unclaimed)
              :to-equal '())))

  (it "compound commands have non-zero coverage"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "rm temp.txt && touch new.txt")))
           (coverage (plist-get result :coverage)))
      (expect (plist-get coverage :claimed-tokens) :to-be-greater-than 0)
      (expect (plist-get coverage :coverage-ratio) :to-be-greater-than 0.0))))


(describe "End-to-end: Error isolation"
  :var (two-layer--saved-handlers)

  (before-each
    (setq two-layer--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers two-layer--saved-handlers))

  (it "handler error does not discard redirection results"
    ;; Register a handler that throws an error
    (jf/bash-register-command-handler
     :command "cat"
     :domain :filesystem
     :handler (lambda (_cmd) (error "deliberate test error")))
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "cat input.txt > output.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      ;; Redirection :write must survive even when handler throws
      (expect (cl-some (lambda (op)
                         (and (equal (plist-get op :file) "output.txt")
                              (eq (plist-get op :operation) :write)))
                       fs-ops)
              :to-be-truthy)))

  (it "no error sentinel in messages for valid commands"
    ;; The orchestrator should not log grammar-level extraction errors
    ;; for well-formed commands
    (jf/bash-extract-semantics (jf/bash-parse "cat file.txt"))
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (expect (search-backward "Error in grammar-level extraction" nil t)
              :to-be nil))))


(describe "End-to-end: Var-context threading"
  :var (two-layer--saved-handlers)

  (before-each
    (setq two-layer--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers two-layer--saved-handlers))

  (it "var-context from caller is threaded through"
    (let* ((var-ctx '((DIR . "/data")))
           (result (jf/bash-extract-semantics
                    (jf/bash-parse "cat $DIR/file.txt") var-ctx))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      (expect (cl-some (lambda (op)
                         (equal (plist-get op :file) "/data/file.txt"))
                       fs-ops)
              :to-be-truthy)))

  (it "chain accumulates context for later commands"
    (let* ((result (jf/bash-extract-semantics
                    (jf/bash-parse "cd /tmp && cat file.txt")))
           (fs-ops (alist-get :filesystem (plist-get result :domains))))
      ;; cat should resolve file.txt against /tmp
      (expect (cl-some (lambda (op)
                         (and (eq (plist-get op :operation) :read)
                              (or (equal (plist-get op :file) "/tmp/file.txt")
                                  (equal (plist-get op :file) "file.txt"))))
                       fs-ops)
              :to-be-truthy))))

(provide 'two-layer-contracts-spec)
;;; two-layer-contracts-spec.el ends here
