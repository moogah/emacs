;;; tool-call-spec.el --- Buttercup tests for gptel-chat stream-callback tool-call rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Exercises `gptel-chat--stream-callback', the factory that returns
;; the closure passed to `gptel-request' as `:callback' (design.md
;; §Decision 10).  The callback dispatches on upstream's documented
;; response shapes via `pcase' and renders tool calls as nested
;; `#+begin_tool' blocks inside the active assistant block.
;;
;; The upstream contract for tool events (see `gptel-request.el:1812-1827'
;; and `gptel.el:1801, 1855') is:
;;
;;   (tool-call   . ((TOOL-STRUCT ARGS CB)     ...))
;;   (tool-result . ((TOOL-STRUCT ARGS RESULT) ...))
;;
;; where TOOL-STRUCT is a `gptel-tool' cl-defstruct (built here via
;; `gptel-make-tool'), ARGS is a plist of model-supplied arguments,
;; CB is the callback upstream invokes with the tool result (we
;; ignore it; we only render), and RESULT is whatever the tool
;; function returned.  These tests feed exactly that triple-list
;; shape into the callback — an earlier iteration built the cdr out
;; of synthetic plists, which masked a runtime drift bug where every
;; tool header rendered as `#+begin_tool ( :args nil)' and every
;; result rendered as an empty string.  See task
;; `stream-callback-tool-element-shape-and-tests' for the corrective
;; context.
;;
;; Scenarios covered (spec §"Tool-call rendering inside assistant
;; blocks" and §"Response streaming and sanitization"):
;; - Single tool call: one `#+begin_tool'/`#+end_tool' block with
;;   correct args and result.
;; - Multiple tool calls interleaved with prose: three sibling blocks
;;   with prose in correct positions.
;; - Tool-result containing a `#+end_tool' collision is sanitized.
;; - Reasoning events are ignored.
;; - Unknown response shape signals (defensive guard).
;; - Auto-approved tool (no preceding tool-call event): upstream only
;;   emits `(tool-call . ...)' for tools that go through the
;;   `:confirm' path; auto-approved tools surface ONLY via the
;;   terminal `(tool-result . ...)' event.  The callback must render
;;   the block on the fly from the 3-list's (TOOL ARGS RESULT).
;;
;; Sibling specs (`streaming-spec.el') cover the completion / abort /
;; error terminal paths and the bypass assertion on
;; `gptel-post-response-functions'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
;; `gptel-tool' struct and `gptel-make-tool' constructor.  The
;; callback-under-test extracts the tool name via `gptel-tool-name'
;; from the struct upstream passes as the first element of each
;; tool-call / tool-result 3-list.
(require 'gptel)

;; Load the module under test from the co-located source directory.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-stream)


;;; Fixtures -----------------------------------------------------------------

(defvar gptel-chat-tool-call-test--buffer nil
  "Scratch buffer for stream-callback tool-call tests.")

(defvar gptel-chat-tool-call-test--marker nil
  "Live advance insertion marker for the active assistant block.")

(defun gptel-chat-tool-call-test--fresh-buffer ()
  "Create a fresh scratch buffer containing a single open assistant block.
Returns an advance marker positioned at the end of the assistant
body (i.e., where `#+end_assistant' will eventually be inserted)."
  (setq gptel-chat-tool-call-test--buffer
        (generate-new-buffer " *gptel-chat-tool-call-test*"))
  (with-current-buffer gptel-chat-tool-call-test--buffer
    (insert "#+begin_user\nhello\n#+end_user\n#+begin_assistant\n")
    (setq gptel-chat-tool-call-test--marker
          (copy-marker (point-max) t)))
  gptel-chat-tool-call-test--marker)

(defun gptel-chat-tool-call-test--buffer-string ()
  "Return the current contents of the scratch test buffer."
  (with-current-buffer gptel-chat-tool-call-test--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gptel-chat-tool-call-test--cleanup ()
  (when (buffer-live-p gptel-chat-tool-call-test--buffer)
    (kill-buffer gptel-chat-tool-call-test--buffer))
  (setq gptel-chat-tool-call-test--buffer nil
        gptel-chat-tool-call-test--marker nil))

(defun gptel-chat-tool-call-test--tool (name)
  "Return a minimal `gptel-tool' struct with NAME for test fixtures.
Only the `name' slot is consulted by the stream callback; the
other required slots (`function', `description') are set to
benign stand-ins so `gptel-make-tool' accepts the spec."
  (gptel-make-tool
   :name name
   :function #'ignore
   :description (format "test tool %s" name)
   :args nil
   :category "test"))

(defun gptel-chat-tool-call-test--ignore-cb (&rest _)
  "Stand-in for upstream's tool callback (3rd element of a call 3-list).
The stream callback never invokes CB — upstream does — so any
ignorable function works here."
  nil)


;;; gptel-chat--stream-callback — argument validation ---------------------

(describe "gptel-chat--stream-callback"

  (before-each
    (gptel-chat-tool-call-test--fresh-buffer))

  (after-each
    (gptel-chat-tool-call-test--cleanup))

  (describe "argument validation"

    (it "rejects a non-marker argument"
      (expect (gptel-chat--stream-callback 42) :to-throw))

    (it "rejects a marker with no buffer"
      (let ((dead (make-marker)))
        (expect (gptel-chat--stream-callback dead) :to-throw)))

    (it "rejects a marker with insertion-type nil"
      (let ((default-type-marker
             (with-current-buffer gptel-chat-tool-call-test--buffer
               (copy-marker (point-max)))))
        (expect (marker-insertion-type default-type-marker) :to-equal nil)
        (expect (gptel-chat--stream-callback default-type-marker)
                :to-throw)))

    (it "returns a callable closure when given a valid marker"
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-tool-call-test--marker)))
        (expect (functionp cb) :to-be-truthy))))


  ;;; gptel-chat--stream-callback — tool-call / tool-result ---------------

  (describe "single tool call"

    (it "renders one #+begin_tool block with name, args, and result"
      ;; Scenario: assistant block contains one `#+begin_tool' /
      ;; `#+end_tool' block with the call arguments and result.
      ;;
      ;; Upstream shape per `gptel.el:1855':
      ;;   (tool-result . ((TOOL ARGS RESULT) ...))
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (tool (gptel-chat-tool-call-test--tool "read_file"))
             (args '(:path "/tmp/x")))
        (funcall cb `(tool-call . ((,tool ,args ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,tool ,args "file contents\n")))
                 nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "#+begin_tool (read_file :args (:path \"/tmp/x\"))\n"
                      "file contents\n"
                      "#+end_tool\n")))

    (it "renders an empty result when result is nil"
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (tool (gptel-chat-tool-call-test--tool "noop")))
        (funcall cb `(tool-call . ((,tool nil ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,tool nil nil)))
                 nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "#+begin_tool (noop :args nil)\n"
                      "\n"
                      "#+end_tool\n"))))


  (describe "prose surrounding a tool call"

    (it "prose before and after a single tool call lands in the right places"
      ;; Scenario: prose streams before and after the tool event.
      ;; After tool-result, `tool-marker' is cleared, so subsequent
      ;; streamed prose routes back to the assistant-level marker.
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (tool (gptel-chat-tool-call-test--tool "read_file"))
             (args '(:path "/a")))
        (funcall cb "Let me check.\n" nil)
        (funcall cb `(tool-call . ((,tool ,args ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,tool ,args "ok")))
                 nil)
        (funcall cb "Done.\n" nil)
        (funcall cb t nil))
      (let ((s (gptel-chat-tool-call-test--buffer-string)))
        (expect s
                :to-equal
                (concat "#+begin_user\nhello\n#+end_user\n"
                        "#+begin_assistant\n"
                        "Let me check.\n"
                        "#+begin_tool (read_file :args (:path \"/a\"))\n"
                        "ok\n"
                        "#+end_tool\n"
                        "Done.\n"
                        "#+end_assistant\n"
                        "\n#+begin_user\n\n#+end_user\n")))))


  (describe "multiple tool calls interleaved with prose"

    (it "renders three sibling #+begin_tool blocks in order"
      ;; Scenario (spec §"Multiple tool calls in a response"): three
      ;; sequential tool calls interleaved with prose produce three
      ;; sibling blocks in document order, with prose in its correct
      ;; positions.
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (t1 (gptel-chat-tool-call-test--tool "t1"))
             (t2 (gptel-chat-tool-call-test--tool "t2"))
             (t3 (gptel-chat-tool-call-test--tool "t3")))
        (funcall cb "A\n" nil)
        (funcall cb `(tool-call . ((,t1 (:x 1) ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,t1 (:x 1) "r1")))
                 nil)
        (funcall cb "B\n" nil)
        (funcall cb `(tool-call . ((,t2 (:x 2) ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,t2 (:x 2) "r2")))
                 nil)
        (funcall cb "C\n" nil)
        (funcall cb `(tool-call . ((,t3 (:x 3) ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,t3 (:x 3) "r3")))
                 nil)
        (funcall cb "D\n" nil)
        (funcall cb t nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "A\n"
                      "#+begin_tool (t1 :args (:x 1))\nr1\n#+end_tool\n"
                      "B\n"
                      "#+begin_tool (t2 :args (:x 2))\nr2\n#+end_tool\n"
                      "C\n"
                      "#+begin_tool (t3 :args (:x 3))\nr3\n#+end_tool\n"
                      "D\n"
                      "#+end_assistant\n"
                      "\n#+begin_user\n\n#+end_user\n")))

    (it "handles a single tool-call event carrying multiple parallel calls"
      ;; Two calls arrive in one tool-call event (parallel), then two
      ;; results in one tool-result event in matching order.  Both
      ;; blocks must be in-order siblings with the correct results.
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (p1 (gptel-chat-tool-call-test--tool "p1"))
             (p2 (gptel-chat-tool-call-test--tool "p2")))
        (funcall cb `(tool-call . ((,p1 (:k 1) ,#'gptel-chat-tool-call-test--ignore-cb)
                                   (,p2 (:k 2) ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,p1 (:k 1) "first")
                                     (,p2 (:k 2) "second")))
                 nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "#+begin_tool (p1 :args (:k 1))\nfirst\n#+end_tool\n"
                      "#+begin_tool (p2 :args (:k 2))\nsecond\n#+end_tool\n"))))


  (describe "tool-result sanitization"

    (it "escapes a #+end_tool line inside a tool result"
      ;; If the tool's result happens to contain a `#+end_tool' line
      ;; (e.g. the tool read a file that contains one), that line must
      ;; be rewritten to `,#+end_tool' before insertion — otherwise it
      ;; would prematurely close the containing block.
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (tool (gptel-chat-tool-call-test--tool "cat"))
             (args '(:f "p"))
             (result (concat "line1\n"
                             "#+end_tool\n"
                             "line3")))
        (funcall cb `(tool-call . ((,tool ,args ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,tool ,args ,result)))
                 nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "#+begin_tool (cat :args (:f \"p\"))\n"
                      "line1\n"
                      ",#+end_tool\n"
                      "line3\n"
                      "#+end_tool\n")))

    (it "escapes a #+end_assistant line inside a tool result"
      ;; Same protection for an outer-block collision.
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (tool (gptel-chat-tool-call-test--tool "cat"))
             (args '(:f "p")))
        (funcall cb `(tool-call . ((,tool ,args ,#'gptel-chat-tool-call-test--ignore-cb)))
                 nil)
        (funcall cb `(tool-result . ((,tool ,args "#+end_assistant\n")))
                 nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "#+begin_tool (cat :args (:f \"p\"))\n"
                      ",#+end_assistant\n"
                      "#+end_tool\n"))))


  (describe "reasoning events"

    (it "ignores a reasoning chunk"
      ;; Decision 10: v1 ignores `(reasoning . CHUNK)`.  The buffer
      ;; must be unchanged after a reasoning event.
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-tool-call-test--marker)))
        (funcall cb `(reasoning . "I should check the file first.") nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              "#+begin_user\nhello\n#+end_user\n#+begin_assistant\n")))


  (describe "defensive guard for unknown shapes"

    (it "signals on an unexpected response shape"
      ;; The default `pcase' arm surfaces drift from upstream's
      ;; callback protocol so it cannot silently corrupt the buffer.
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-tool-call-test--marker)))
        (expect (funcall cb 'unexpected-sentinel nil)
                :to-throw))))


  (describe "auto-approved tool (no preceding tool-call event)"
    ;; Upstream `gptel--handle-tool-use' only emits
    ;; `(tool-call . ...)' for tools whose `:confirm' path is active
    ;; (see `gptel-request.el:1684-1752' — the `pending-calls' branch).
    ;; Auto-approved tools execute inline and surface ONLY via the
    ;; terminal `(tool-result . result-alist)' event; no tool-call
    ;; ever reaches this callback.  Each 3-list in RESULTS carries
    ;; TOOL-STRUCT and ARGS, so the callback has everything it needs
    ;; to open and close a fresh `#+begin_tool' block on the fly.

    (it "renders one #+begin_tool block when tool-result arrives with no prior tool-call"
      ;; Mirrors the real-world crash: model asks for `which brew',
      ;; scope auto-approves, tool returns success, upstream fires
      ;; (tool-result . ((TOOL ARGS RESULT))) with no preceding
      ;; tool-call.  The callback MUST render the block rather than
      ;; signal "orphan tool-result".
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (tool (gptel-chat-tool-call-test--tool "run_bash_command"))
             (args '(:command "which brew"))
             (result "/opt/homebrew/bin/brew\n"))
        (funcall cb `(tool-result . ((,tool ,args ,result)))
                 nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "#+begin_tool (run_bash_command :args (:command \"which brew\"))\n"
                      "/opt/homebrew/bin/brew\n"
                      "#+end_tool\n")))

    (it "renders the block inline with surrounding prose and subsequent prose routes back to assistant"
      ;; After an auto-approved tool-result renders its block, the
      ;; tool-marker override MUST be cleared so further streamed
      ;; prose lands at the assistant-level marker (not inside the
      ;; closed tool block).
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (tool (gptel-chat-tool-call-test--tool "run_bash_command"))
             (args '(:command "which brew")))
        (funcall cb "Checking path.\n" nil)
        (funcall cb `(tool-result . ((,tool ,args "/opt/homebrew/bin/brew\n")))
                 nil)
        (funcall cb "Found it.\n" nil)
        (funcall cb t nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "Checking path.\n"
                      "#+begin_tool (run_bash_command :args (:command \"which brew\"))\n"
                      "/opt/homebrew/bin/brew\n"
                      "#+end_tool\n"
                      "Found it.\n"
                      "#+end_assistant\n"
                      "\n#+begin_user\n\n#+end_user\n")))

    (it "renders multiple auto-approved parallel calls as sibling blocks in result order"
      ;; `gptel--handle-tool-use' batches auto-approved results into a
      ;; single `(tool-result . ((TOOL1 ARGS1 R1) (TOOL2 ARGS2 R2) ...))'
      ;; event.  Each triple should render its own sibling block.
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (t1 (gptel-chat-tool-call-test--tool "list_files"))
             (t2 (gptel-chat-tool-call-test--tool "read_file")))
        (funcall cb `(tool-result . ((,t1 (:dir "/a") "file1\nfile2\n")
                                     (,t2 (:path "/b") "body")))
                 nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "#+begin_tool (list_files :args (:dir \"/a\"))\n"
                      "file1\nfile2\n"
                      "#+end_tool\n"
                      "#+begin_tool (read_file :args (:path \"/b\"))\n"
                      "body\n"
                      "#+end_tool\n")))

    (it "sanitizes a #+end_tool collision inside an auto-approved tool-result"
      ;; Collision-escape invariant must hold on the synthesized
      ;; block path too — a tool reading a file whose content
      ;; happens to contain `#+end_tool' must still produce a
      ;; well-formed containing block.
      (let* ((cb (gptel-chat--stream-callback
                  gptel-chat-tool-call-test--marker))
             (tool (gptel-chat-tool-call-test--tool "cat"))
             (args '(:f "p"))
             (result (concat "line1\n"
                             "#+end_tool\n"
                             "line3")))
        (funcall cb `(tool-result . ((,tool ,args ,result)))
                 nil))
      (expect (gptel-chat-tool-call-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "#+begin_tool (cat :args (:f \"p\"))\n"
                      "line1\n"
                      ",#+end_tool\n"
                      "line3\n"
                      "#+end_tool\n")))))


(provide 'tool-call-spec)

;;; tool-call-spec.el ends here
