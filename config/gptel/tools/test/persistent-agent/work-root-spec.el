;;; work-root-spec.el --- PersistentAgent work_root + read/write path tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the work-root + symmetric read/write path surface
;; added by the `agent-workroot-and-paths' task
;; (openspec/changes/gptel-work-root-default-directory):
;;
;; - `jf/gptel-persistent-agent--task' accepts a `work_root' written to the
;;   agent's OWN drawer as `:GPTEL_WORK_ROOT:'; omitted ⇒ frozen parent's
;;   work root (design.md D5).
;; - On activation the binder (`jf/gptel--bind-session-buffer', the SINGLE
;;   work-root seam — register/boundary/work-root-activation-seam) sets the
;;   agent buffer's `default-directory' from that drawer key.
;; - `read_paths' / `write_paths' carry the agent scope; `/tmp/**' is
;;   appended to write as scratch (design.md D6).
;; - The work root is readable BY CONSTRUCTION: `<work_root>/**' is
;;   prepended to `:read' so relative reads (which resolve against the
;;   work-root `default-directory') always land in scope.  This SUPERSEDES
;;   the old D7 consistency guardrail (design.md D6/D7).
;;
;; Each `it' block carries a leading scenario-mapping comment so drift
;; between the spec and the implementation surfaces here first.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared persistent-agent fixtures from the co-located helpers file.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path spec-dir)
  (load (expand-file-name "helpers-spec.el" spec-dir) nil t))

;; Load the module under test.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tools-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path tools-dir)
  (require 'gptel-persistent-agent
           (expand-file-name "persistent-agent.el" tools-dir)))

;; Load the scope validator for the read-side behavioral assertion
;; (a relative read under the work root must validate as ALLOWED).
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name "../../../scope/" spec-dir)))
  (add-to-list 'load-path scope-dir)
  (require 'jf-gptel-scope-validation
           (expand-file-name "scope-validation.el" scope-dir)))

;; Helper: open AGENT-DIR's `session.org' in an `org-mode' temp buffer so
;; drawer queries route through the same parser the production loader uses.
(defmacro jf/persistent-agent-test--with-agent-session-org-wr (agent-dir &rest body)
  "Open AGENT-DIR's session.org in a temp org buffer and run BODY there."
  (declare (indent 1) (debug t))
  `(let ((session-org (expand-file-name "session.org" ,agent-dir)))
     (with-temp-buffer
       (insert-file-contents session-org)
       (org-mode)
       ,@body)))

;; Helper: the single agent directory created under the mock parent branch.
(defun jf/persistent-agent-test--sole-agent-dir (branch-dir)
  "Return the absolute path of the sole agent dir under BRANCH-DIR's agents/."
  (let* ((agents-dir (expand-file-name "agents" branch-dir))
         (agent-name (car (cl-remove-if
                           (lambda (n) (member n '("." "..")))
                           (directory-files agents-dir)))))
    (expand-file-name agent-name agents-dir)))

;;; Tests

(describe "PersistentAgent work_root drawer key"

  (it "writes the explicit work_root into the agent drawer as :GPTEL_WORK_ROOT:"
    ;; Scenario: spec.md § "Agent working directory (parent-supplied)" ->
    ;; "Explicit work_root written to the agent drawer".
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil)
             (wr (expand-file-name "proj/worktree-a" mock-session-dir)))
         (make-directory wr t)
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            wr
            (list (expand-file-name "**" wr))))
         (let ((agent-dir (jf/persistent-agent-test--sole-agent-dir mock-branch-dir)))
           (jf/persistent-agent-test--with-agent-session-org-wr agent-dir
             ;; `:GPTEL_WORK_ROOT:' carries the resolved absolute work root.
             (expect (org-entry-get (point-min) "GPTEL_WORK_ROOT")
                     :to-equal wr)))))))

  (it "activates the agent buffer with default-directory set from :GPTEL_WORK_ROOT:"
    ;; Scenario: spec.md § "Agent working directory (parent-supplied)" ->
    ;; "on activation the agent buffer's default-directory is <work_root>/".
    ;; The single work-root seam is the chat-mode-hook binder
    ;; (register/boundary/work-root-activation-seam) — `--task' writes only
    ;; the drawer key, never a second `default-directory' binding.
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil)
             (wr (expand-file-name "proj/worktree-a" mock-session-dir)))
         (make-directory wr t)
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            wr
            (list (expand-file-name "**" wr))))
         (let* ((agent-dir   (jf/persistent-agent-test--sole-agent-dir mock-branch-dir))
                (session-org (expand-file-name "session.org" agent-dir))
                (buf (find-file-noselect session-org)))
           (unwind-protect
               (with-current-buffer buf
                 ;; The content-addressed activation drove the buffer into
                 ;; gptel-chat-mode and ran the binder.
                 (expect (derived-mode-p 'gptel-chat-mode) :not :to-be nil)
                 (expect default-directory
                         :to-equal (file-name-as-directory wr)))
             (kill-buffer buf)))))))

  (it "freezes the parent's work root into the drawer when work_root is omitted"
    ;; Scenario: spec.md § "Omitted work_root defaults to the parent's work
    ;; root".  `--task' runs in the parent buffer, so the default is the
    ;; parent's `default-directory' captured (frozen) at spawn — NOT a live
    ;; link (design.md D5).
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let* ((captured nil)
              (parent-root (file-name-as-directory
                            (expand-file-name "parent-cwd" mock-session-dir))))
         (make-directory parent-root t)
         ;; Simulate the parent buffer's work root being bound at spawn.
         (let ((default-directory parent-root))
           (jf/persistent-agent-test--with-mock-gptel-request captured
             ;; work_root omitted ⇒ defaults to the parent's work root.
             (jf/gptel-persistent-agent--task
              #'ignore "test-preset" "analyze code" "do the thing"
              nil
              (list (concat parent-root "**")))))
         (let ((agent-dir (jf/persistent-agent-test--sole-agent-dir mock-branch-dir)))
           (jf/persistent-agent-test--with-agent-session-org-wr agent-dir
             ;; Drawer records the parent's work root, frozen at spawn.
             ;; `expand-file-name' preserves the trailing slash that
             ;; `file-name-as-directory' added to PARENT-ROOT.
             (expect (org-entry-get (point-min) "GPTEL_WORK_ROOT")
                     :to-equal parent-root))))))))

(describe "PersistentAgent read_paths / write_paths scope keys"

  (it "records read_paths verbatim and write_paths with /tmp scratch appended"
    ;; Scenario: spec.md § "Agent session creation" -> "Drawer scope and
    ;; work-root keys written from parent-supplied paths".  `:read' is
    ;; verbatim; `:write' carries the supplied write paths PLUS `/tmp/**'
    ;; appended as scratch (design.md D6; register/shape/scope-config-plist).
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil)
             (wr "/path/to/project"))
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            wr
            '("/path/to/project/**")
            '("/path/to/project/**")))
         (let ((agent-dir (jf/persistent-agent-test--sole-agent-dir mock-branch-dir)))
           (jf/persistent-agent-test--with-agent-session-org-wr agent-dir
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_READ")
                     :to-equal '("/path/to/project/**"))
             ;; write_paths verbatim + `/tmp/**' scratch appended last.
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_WRITE")
                     :to-equal '("/path/to/project/**" "/tmp/**"))
             (expect (org-entry-get (point-min) "GPTEL_WORK_ROOT")
                     :to-equal wr)))))))

  (it "yields scratch-only write when write_paths is omitted"
    ;; Scenario: spec.md § "Agent session creation" -> "/tmp scratch is a
    ;; guaranteed grant, NOT the write default".  With no write_paths the
    ;; agent gets `/tmp/**' alone.
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil))
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            "/path/to/project"
            '("/path/to/project/**")))
         (let ((agent-dir (jf/persistent-agent-test--sole-agent-dir mock-branch-dir)))
           (jf/persistent-agent-test--with-agent-session-org-wr agent-dir
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_WRITE")
                     :to-equal '("/tmp/**")))))))))

(describe "PersistentAgent work_root auto-included in read scope (design.md D6)"

  ;; The D7 consistency guardrail (warn when work_root escapes read scope)
  ;; was SUPERSEDED: the work root is now made readable BY CONSTRUCTION —
  ;; `<work_root>/**' is prepended to `:read'.  A work_root unreadable to
  ;; the agent serves no purpose; the read/work_root redundancy is accepted
  ;; (user decision, cycle-1781718724-d7-guardrail-prefix-match).

  (it "prepends the work-root read pattern <root>/** to :GPTEL_SCOPE_READ:"
    ;; Scenario: spec.md § "Agent working directory (parent-supplied)" ->
    ;; relative reads under the work root always land in scope.  Even with
    ;; NO read_paths supplied, the drawer's read scope carries `<root>/**'
    ;; (register/vocabulary/agent-path-params: work_root maps to the
    ;; :GPTEL_WORK_ROOT: drawer key AND prepends :GPTEL_SCOPE_READ:).
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil)
             (wr (expand-file-name "proj/worktree-a" mock-session-dir)))
         (make-directory wr t)
         (jf/persistent-agent-test--with-mock-gptel-request captured
           ;; NO read_paths argument — the work root alone seeds read scope.
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            wr))
         (let ((agent-dir (jf/persistent-agent-test--sole-agent-dir mock-branch-dir)))
           (jf/persistent-agent-test--with-agent-session-org-wr agent-dir
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_READ")
                     :to-equal
                     (list (concat (directory-file-name wr) "/**")))))))))

  (it "does not double the work-root pattern when the caller already supplied it"
    ;; Cosmetic dedup (`member'): an identical `<root>/**' read_path is not
    ;; repeated when prepended.
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let* ((captured nil)
              (wr (expand-file-name "proj/worktree-a" mock-session-dir))
              (pat (concat (directory-file-name wr) "/**")))
         (make-directory wr t)
         (jf/persistent-agent-test--with-mock-gptel-request captured
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            wr
            (list pat)))
         (let ((agent-dir (jf/persistent-agent-test--sole-agent-dir mock-branch-dir)))
           (jf/persistent-agent-test--with-agent-session-org-wr agent-dir
             (expect (org-entry-get-multivalued-property
                      (point-min) "GPTEL_SCOPE_READ")
                     :to-equal (list pat))))))))

  (it "validates a relative read under the work root as ALLOWED with empty read_paths"
    ;; Behavioral: a relative path in the agent resolves against
    ;; `default-directory' (= work root).  With NO read_paths granted, the
    ;; auto-prepended `<root>/**' must make that resolved path ALLOWED when
    ;; routed through the real scope validator (the read side of the
    ;; work-root activation seam, register/boundary/work-root-activation-seam).
    (jf/persistent-agent-test--with-mock-parent-session
     (jf/persistent-agent-test--with-mock-preset 'test-preset
       (let ((captured nil)
             (wr (expand-file-name "proj/worktree-a" mock-session-dir)))
         (make-directory wr t)
         (jf/persistent-agent-test--with-mock-gptel-request captured
           ;; No read_paths — only the work root seeds read scope.
           (jf/gptel-persistent-agent--task
            #'ignore "test-preset" "analyze code" "do the thing"
            wr))
         (let* ((agent-dir (jf/persistent-agent-test--sole-agent-dir mock-branch-dir))
                (read-scope
                 (jf/persistent-agent-test--with-agent-session-org-wr agent-dir
                   (org-entry-get-multivalued-property
                    (point-min) "GPTEL_SCOPE_READ")))
                ;; A relative read `notes.txt' resolves against the work
                ;; root (default-directory) — the absolute path the
                ;; validator sees at funcall time.
                (resolved (expand-file-name "notes.txt" wr))
                (config (list :paths (list :read read-scope)))
                (result (jf/gptel-scope--validate-path-operation
                         resolved :read config)))
           (expect (plist-get result :allowed) :to-be t)))))))

(provide 'work-root-spec)
;;; work-root-spec.el ends here
