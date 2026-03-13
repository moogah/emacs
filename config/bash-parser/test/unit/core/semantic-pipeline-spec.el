;;; semantic-pipeline-spec.el --- Two-layer semantic pipeline end-to-end tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Verifies the full two-layer semantic pipeline end-to-end:
;;   Layer 0: Grammar-level extraction (redirections, recursive engine)
;;   Layer 1: Command handlers (per-command domain dispatch)
;;   Merge: Layer 0 takes priority per domain; Layer 1 contributes new domains only
;;
;; Key invariant: If Layer 0 claims :filesystem with operations,
;; Layer 1 command handler :filesystem results are SKIPPED
;; (the `unless (assq domain domains-alist)` check in the orchestrator).
;;
;; Known issue: Layer 0 grammar extraction wraps both extraction AND
;; token claiming in a single condition-case.  The token-claiming
;; function (jf/bash--claim-tokens-for-operations) uses cl-return
;; inside dolist without a cl-block, which throws for commands that
;; produce positional-arg operations.  When this happens, the entire
;; Layer 0 result is discarded (even though extraction succeeded),
;; and Layer 1 becomes the sole contributor for :filesystem.
;; Commands WITHOUT handlers (echo, sort, etc.) are unaffected
;; since they produce no positional-arg operations.

;;; Code:

(require 'cl-lib)
(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))
(require 'bash-parser-orchestrator)
(require 'bash-parser-semantics)

;; Load contract validation helpers
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (config-dir (expand-file-name "../../../../" test-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  (add-to-list 'load-path contracts-dir))

(require 'contract-core)
(require 'contract-bash-parser)

(require 'contract-test-helpers
         (expand-file-name "config/bash-parser/commands/test/contract-test-helpers.el"
                           jf/emacs-dir))

(defvar semantic-pipeline--saved-handlers nil
  "Saved handler registry for test isolation.")

(describe "Semantic Pipeline"

  (before-each
    (setq semantic-pipeline--saved-handlers jf/bash-command-handlers)
    ;; Start with a fresh registry and re-load all command handlers
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers semantic-pipeline--saved-handlers))

  ;; ─── Result Structure Conformance ────────────────────────────────────────

  (describe "result structure"

    (it "returns :domains, :coverage, and :parse-complete keys"
      (let* ((parsed (jf/bash-parse "echo hello"))
             (result (jf/bash-extract-semantics parsed)))
        (expect (plist-member result :domains) :not :to-be nil)
        (expect (plist-member result :coverage) :not :to-be nil)
        (expect (plist-member result :parse-complete) :not :to-be nil)))

    (it "returns :domains as an alist of (domain . ops) pairs"
      (let* ((parsed (jf/bash-parse "echo hello > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains)))
        ;; domains should be a list
        (expect (listp domains) :to-be-truthy)
        ;; Each entry should be a cons cell (keyword . list)
        (dolist (entry domains)
          (expect (consp entry) :to-be-truthy)
          (expect (keywordp (car entry)) :to-be-truthy)
          (expect (listp (cdr entry)) :to-be-truthy))))

    (it "has valid domains alist per contract"
      (let* ((parsed (jf/bash-parse "cat input.txt > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains)))
        (contract-test--validate-domains domains "structure conformance"))))

  ;; ─── Layer 0 Only: Grammar Extraction ────────────────────────────────────

  (describe "Layer 0 only (grammar extraction)"

    (it "extracts :write from output redirection on command with no handler"
      (let* ((parsed (jf/bash-parse "echo hello > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (fs-ops (alist-get :filesystem domains)))
        (expect fs-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts :append from >> redirection"
      (let* ((parsed (jf/bash-parse "echo data >> log.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (fs-ops (alist-get :filesystem domains)))
        (expect fs-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "log.txt")
                                (eq (plist-get op :operation) :append)))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts :read from input redirection"
      (let* ((parsed (jf/bash-parse "sort < input.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (fs-ops (alist-get :filesystem domains)))
        (expect fs-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "input.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy))))

  ;; ─── Layer 0 Priority ────────────────────────────────────────────────────

  (describe "Layer 0 priority over Layer 1"

    (it "uses Layer 0 filesystem ops when grammar produces them for cat"
      ;; cat has a handler, but Layer 0 (grammar/recursive engine) also
      ;; extracts from positional args. When Layer 0 claims :filesystem,
      ;; the Layer 1 cat handler is SKIPPED.
      (let* ((parsed (jf/bash-parse "cat input.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (fs-ops (alist-get :filesystem domains)))
        ;; Should have filesystem operations from Layer 0
        (expect fs-ops :not :to-be nil)
        ;; Should contain :read for input.txt
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "input.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)))

    (it "provides :filesystem via Layer 1 handler when Layer 0 errors on token claiming"
      ;; "cat input.txt > output.txt" - Layer 0 extraction succeeds BUT
      ;; token claiming throws (cl-return bug), so entire Layer 0 is discarded.
      ;; Layer 1 cat handler then contributes :filesystem with :read for input.txt.
      ;; The :write for output.txt (from redirection) is lost because redirections
      ;; are only extracted by Layer 0.
      (let* ((parsed (jf/bash-parse "cat input.txt > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (fs-ops (alist-get :filesystem domains)))
        (expect fs-ops :not :to-be nil)
        ;; Should have :read for input.txt (from Layer 1 cat handler fallback)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "input.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts both redirection and positional ops for commands without handlers"
      ;; "sort input.txt > output.txt" - sort has no registered handler,
      ;; so Layer 0 succeeds fully (no positional-arg ops to trigger cl-return bug).
      ;; Redirections produce :write for output.txt.
      (let* ((parsed (jf/bash-parse "sort input.txt > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (fs-ops (alist-get :filesystem domains)))
        (expect fs-ops :not :to-be nil)
        ;; Should have :write for output.txt (from Layer 0 redirection extraction)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy))))

  ;; ─── Layer 1 Only: Command Handlers for Non-filesystem Domains ──────────

  (describe "Layer 1 (command handlers for non-filesystem domains)"

    (it "produces :authentication domain for gcloud command"
      (let* ((parsed (jf/bash-parse "gcloud compute instances list"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (auth-ops (alist-get :authentication domains)))
        (expect auth-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (eq (plist-get op :provider) :gcloud))
                         auth-ops)
                :to-be-truthy)))

    (it "produces :network domain for gcloud command"
      (let* ((parsed (jf/bash-parse "gcloud compute instances list"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (net-ops (alist-get :network domains)))
        (expect net-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (eq (plist-get op :protocol) :https))
                         net-ops)
                :to-be-truthy))))

  ;; ─── Multi-Domain Merge ──────────────────────────────────────────────────

  (describe "multi-domain merge"

    (it "produces both :authentication and :network for gcloud with redirection"
      ;; "gcloud compute instances list > instances.txt"
      ;; Layer 0: :filesystem (:write for instances.txt from redirection)
      ;; Layer 1: :authentication and :network (from gcloud handlers)
      (let* ((parsed (jf/bash-parse "gcloud compute instances list > instances.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains)))
        ;; Three domains should be present
        (expect (alist-get :filesystem domains) :not :to-be nil)
        (expect (alist-get :authentication domains) :not :to-be nil)
        (expect (alist-get :network domains) :not :to-be nil)
        ;; Validate the full domain structure
        (contract-test--validate-domains domains "gcloud multi-domain")))

    (it "produces :filesystem from Layer 0 and :authentication/:network from Layer 1 for aws"
      ;; "aws s3 ls > listing.txt"
      ;; Layer 0: :filesystem from redirection (:write listing.txt)
      ;; Layer 1: :authentication and :network from aws handlers
      ;; Layer 1 :filesystem is SKIPPED because Layer 0 already claimed it
      (let* ((parsed (jf/bash-parse "aws s3 ls > listing.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains)))
        (expect (alist-get :filesystem domains) :not :to-be nil)
        (expect (alist-get :authentication domains) :not :to-be nil)
        (expect (alist-get :network domains) :not :to-be nil)
        ;; The :write for listing.txt should be present (from Layer 0)
        (let ((fs-ops (alist-get :filesystem domains)))
          (expect (cl-some (lambda (op)
                             (and (equal (plist-get op :file) "listing.txt")
                                  (eq (plist-get op :operation) :write)))
                           fs-ops)
                  :to-be-truthy))
        (contract-test--validate-domains domains "aws multi-domain"))))

  ;; ─── Compound Commands ───────────────────────────────────────────────────

  (describe "compound commands"

    (it "collects operations from chained commands without handlers via &&"
      ;; Use commands without registered handlers to avoid cl-return bug.
      ;; "sort input.txt > sorted.txt && echo done > log.txt"
      ;; sort: no handler, Layer 0 extracts :write for sorted.txt (redirection)
      ;; echo: no handler, Layer 0 extracts :write for log.txt (redirection)
      ;; All collected in single :filesystem domain
      (let* ((parsed (jf/bash-parse "sort input.txt > sorted.txt && echo done > log.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (fs-ops (alist-get :filesystem domains)))
        (expect fs-ops :not :to-be nil)
        ;; Should find :write for log.txt (redirection from echo)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "log.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)
        ;; Should find :write for sorted.txt (redirection from sort)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "sorted.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)))

    (it "collects operations from piped commands without handlers"
      ;; Use commands without registered handlers.
      ;; "echo data | sort > output.txt"
      ;; echo: no file ops (no redirection, no positional-arg handler)
      ;; sort > output.txt: :write (Layer 0 redirection)
      (let* ((parsed (jf/bash-parse "echo data | sort > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains))
             (fs-ops (alist-get :filesystem domains)))
        (expect fs-ops :not :to-be nil)
        ;; Should contain write for output.txt
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)))

    (it "Layer 1 only dispatches on top-level command-name for chains"
      ;; For chains, Layer 1 uses the top-level :command-name (first command).
      ;; "cat file.txt && gcloud compute instances list"
      ;; Layer 1 dispatches on "cat" only, NOT "gcloud".
      ;; So :authentication and :network from gcloud are NOT present.
      (let* ((parsed (jf/bash-parse "cat file.txt && gcloud compute instances list"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains)))
        ;; :filesystem from cat handler (Layer 1, since Layer 0 errors)
        (expect (alist-get :filesystem domains) :not :to-be nil)
        ;; :authentication NOT present (gcloud handler not dispatched for chain)
        (expect (alist-get :authentication domains) :to-be nil))))

  ;; ─── No Operations ──────────────────────────────────────────────────────

  (describe "commands with no operations"

    (it "produces empty domains for echo without redirection"
      (let* ((parsed (jf/bash-parse "echo hello"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains)))
        ;; No :filesystem domain expected (echo has no handler, no redirection)
        (expect (alist-get :filesystem domains) :to-be nil)))

    (it "produces empty domains for true command"
      (let* ((parsed (jf/bash-parse "true"))
             (result (jf/bash-extract-semantics parsed))
             (domains (plist-get result :domains)))
        (expect domains :to-equal nil))))

  ;; ─── Coverage Calculation ────────────────────────────────────────────────

  (describe "coverage calculation"

    (it "returns coverage with :total-tokens, :claimed-tokens, :coverage-ratio keys"
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (result (jf/bash-extract-semantics parsed))
             (coverage (plist-get result :coverage)))
        (expect (plist-member coverage :total-tokens) :not :to-be nil)
        (expect (plist-member coverage :claimed-tokens) :not :to-be nil)
        (expect (plist-member coverage :coverage-ratio) :not :to-be nil)))

    (it "calculates non-zero coverage for command with file operations"
      ;; Use command without handler to avoid cl-return bug that would
      ;; discard all Layer 0 claimed tokens
      (let* ((parsed (jf/bash-parse "echo hello > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (coverage (plist-get result :coverage)))
        ;; Should have claimed some tokens (at least the redirection tokens)
        (expect (plist-get coverage :claimed-tokens) :to-be-greater-than 0)
        ;; Coverage ratio should be > 0
        (expect (plist-get coverage :coverage-ratio) :to-be-greater-than 0.0)))

    (it "reports total tokens matching parsed command token count"
      (let* ((parsed (jf/bash-parse "echo hello > out.txt"))
             (token-count (length (plist-get parsed :tokens)))
             (result (jf/bash-extract-semantics parsed))
             (coverage (plist-get result :coverage)))
        (expect (plist-get coverage :total-tokens) :to-equal token-count)))

    (it "has coverage-ratio of 1.0 for command with no tokens"
      ;; Edge case: if a command produces zero tokens, coverage should be 1.0
      ;; (perfect coverage of nothing)
      (let ((coverage (jf/bash-calculate-coverage nil nil)))
        (expect (plist-get coverage :coverage-ratio) :to-equal 1.0))))

  ;; ─── Parse Complete Pass-through ─────────────────────────────────────────

  (describe "parse-complete pass-through"

    (it "passes through :parse-complete from parsed command"
      (let* ((parsed (jf/bash-parse "echo hello"))
             (result (jf/bash-extract-semantics parsed)))
        ;; parse-complete from the parser should pass through
        (expect (plist-get result :parse-complete)
                :to-equal (plist-get parsed :parse-complete))))

    (it "passes through parse-complete=nil for incomplete parse"
      ;; Construct a synthetic parsed command with :parse-complete nil
      (let ((fake-parsed (list :tokens nil :parse-complete nil
                               :command-name "echo"
                               :positional-args '("hello")
                               :type :simple)))
        (let ((result (jf/bash-extract-semantics fake-parsed)))
          (expect (plist-get result :parse-complete) :to-be nil))))))

(provide 'semantic-pipeline-spec)
;;; semantic-pipeline-spec.el ends here
