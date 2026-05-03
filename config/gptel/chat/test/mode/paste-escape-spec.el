;;; paste-escape-spec.el --- after-change paste/yank heading escape -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--escape-inserted-headings', the
;; chat-mode `after-change-functions' entry that prefixes column-0
;; `*'-headings inside chat-block bodies with
;; `gptel-chat-content-indentation' leading spaces.  See
;; `openspec/changes/gptel-chat-heading-scoping/design.md' §Decision 3
;; and the task body
;; `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-paste-heading-escape.md'.
;;
;; Coverage (the seven scenarios from the task body):
;;
;;   1. Yank `* H1\n- list\n** H2' inside a `#+begin_user' body —
;;      heading lines get the indent prefix; non-heading lines do not.
;;   2. Yank text with no `*' headings — buffer is unchanged.
;;   3. Mid-line yank (point column > 0) where the inserted text
;;      begins with `* H1' — the first segment is concatenated onto
;;      the existing line and is NOT escaped; subsequent column-0
;;      lines that are headings are escaped.
;;   4. Yank that crosses `#+end_user' — only lines whose BOL falls
;;      strictly inside a chat-block body are escaped; the portion
;;      past the closer is left alone.
;;   5. Yank outside any chat block — no escape applied.
;;   6. Re-yank already-escaped content (lines with leading whitespace
;;      before `*') — idempotent: no double escape.
;;   7. Programmatic `(insert "* Test\n")' inside a chat-block body —
;;      escape is applied.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Add the source dir to `load-path' so `require' below resolves.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

;; The hook calls `gptel-chat--point-in-block-body-p' at runtime, so
;; the parser module must be loaded before the hook fires.  Loading
;; here ensures the predicate is available even when this spec file
;; is the first thing the test runner loads.
(require 'gptel-chat-parser)
(require 'gptel-chat-mode)

;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defmacro gptel-chat-paste-test--with-chat-buffer (content &rest body)
  "Activate `gptel-chat-mode' in a temp buffer, insert CONTENT, run BODY.

CONTENT is inserted *before* mode activation so the mode hook does
not see it as an after-change event — which keeps the fixture
deterministic regardless of any future on-read migration hook.
Point is left at `point-min' after activation; tests reposition it
explicitly."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (gptel-chat-mode)
     (goto-char (point-min))
     ,@body))

(defun gptel-chat-paste-test--goto-marker (marker)
  "Move point to the first occurrence of MARKER and delete the marker.
Returns the position where the marker was."
  (goto-char (point-min))
  (search-forward marker)
  (let ((pos (match-beginning 0)))
    (delete-region pos (match-end 0))
    (goto-char pos)
    pos))

;;; ---------------------------------------------------------------
;;; Specs
;;; ---------------------------------------------------------------

(describe "gptel-chat--escape-inserted-headings"

  ;; ---------------------------------------------------------------------
  ;; Scenario 1: Yank multi-line content with a mix of headings and
  ;; non-heading lines into a user block.  Column-0 `*' lines get the
  ;; one-space prefix; non-heading lines (e.g., `- list') do not.
  ;; ---------------------------------------------------------------------
  (it "escapes column-0 `*' lines and leaves non-heading lines alone"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      ;; Simulate yank of three lines, the first and third of which
      ;; are headings.
      (insert "* H1\n- list\n** H2\n")
      (let ((body (buffer-substring-no-properties (point-min)
                                                  (point-max))))
        (expect body :to-equal
                (concat "#+begin_user\n"
                        " * H1\n"
                        "- list\n"
                        " ** H2\n"
                        "#+end_user\n")))))

  ;; ---------------------------------------------------------------------
  ;; Scenario 2: Yank text containing only non-heading lines.  No
  ;; column-0 `*' line means no edit.
  ;; ---------------------------------------------------------------------
  (it "is a no-op when no inserted line starts with `*' at column 0"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      (insert "plain prose\n- bullet\n  indented\n")
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "plain prose\n"
                      "- bullet\n"
                      "  indented\n"
                      "#+end_user\n"))))

  ;; ---------------------------------------------------------------------
  ;; Scenario 3: Mid-line insertion.  Existing line has prose; point
  ;; sits mid-line; user yanks `* H1\n** H2\n'.  The first inserted
  ;; segment (`* H1') is concatenated onto the existing line and
  ;; therefore does NOT begin at column 0; only the second segment
  ;; (`** H2'), whose BOL is column 0 and >= BEG, is escaped.
  ;; ---------------------------------------------------------------------
  (it "leaves the mid-line first segment alone and escapes later column-0 headings"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "prefix-<HERE>"
                "\n"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      (insert "* H1\n** H2\n")
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "prefix-* H1\n"
                      " ** H2\n"
                      "\n"
                      "#+end_user\n"))))

  ;; ---------------------------------------------------------------------
  ;; Scenario 4: Yank that crosses `#+end_user'.  We construct this by
  ;; positioning point inside the body and inserting a multi-line text
  ;; in which a `*' heading appears AFTER a synthetic `#+end_user' line.
  ;; The post-closer heading is no longer inside any block body, so it
  ;; must NOT be escaped; the pre-closer heading IS inside the body and
  ;; must be escaped.
  ;; ---------------------------------------------------------------------
  (it "escapes only the portion of the inserted range that lies inside the body"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      ;; The yanked text contains its own `#+end_user' line.  After
      ;; insertion the buffer has TWO `#+end_user' lines; only lines
      ;; whose BOL is strictly inside the surviving (first) block body
      ;; get the escape.
      (insert "* before-closer\n#+end_user\n* after-closer\n")
      (let ((body (buffer-substring-no-properties (point-min)
                                                  (point-max))))
        (expect body :to-equal
                (concat "#+begin_user\n"
                        " * before-closer\n"
                        "#+end_user\n"
                        "* after-closer\n"
                        "#+end_user\n")))))

  ;; ---------------------------------------------------------------------
  ;; Scenario 5: Insertion entirely outside any chat block (the buffer
  ;; has no blocks at all).  No escape should be applied.
  ;; ---------------------------------------------------------------------
  (it "does not escape headings inserted outside any chat block"
    (gptel-chat-paste-test--with-chat-buffer
        "<HERE>"
      (gptel-chat-paste-test--goto-marker "<HERE>")
      (insert "* H1\n- list\n** H2\n")
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "* H1\n"
                      "- list\n"
                      "** H2\n"))))

  ;; ---------------------------------------------------------------------
  ;; Scenario 6: Idempotence.  Inserting already-escaped content (lines
  ;; with leading whitespace before `*') must NOT add another prefix.
  ;; ---------------------------------------------------------------------
  (it "is idempotent on already-escaped content"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      ;; The leading space breaks the `^\*+ ' regex, so the hook must
      ;; not match — the line stays exactly one space deep.
      (insert " * H1\n  ** H2\n")
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      " * H1\n"
                      "  ** H2\n"
                      "#+end_user\n"))))

  ;; ---------------------------------------------------------------------
  ;; Scenario 7: Programmatic `(insert "* Test\n")' inside a chat-block
  ;; body — the hook must fire on programmatic inserts the same way it
  ;; fires on user yanks, because both go through the same after-change
  ;; path.
  ;; ---------------------------------------------------------------------
  (it "fires on programmatic `insert' calls"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      (insert "* Test\n")
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      " * Test\n"
                      "#+end_user\n"))))

  ;; ---------------------------------------------------------------------
  ;; Defensive coverage: a pure deletion (END = BEG) must NOT trigger any
  ;; rewrite even if the resulting buffer state contains a column-0 `*'
  ;; line.  The hook gate is `(> end beg)' — pure deletions have END = BEG
  ;; and short-circuit out; replacements (LENGTH > 0 AND END > BEG) flow
  ;; through (covered by the replacement scenarios below).
  ;; ---------------------------------------------------------------------
  (it "does nothing on pure deletion (END = BEG)"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                ;; Pre-existing column-0 heading inserted via the
                ;; pre-mode-activation fixture path.  The on-activation
                ;; migration normalizes it to ` * preexisting' (one-space
                ;; escape), so by the time the after-change hook is
                ;; tested below the heading is already escaped.  The
                ;; test still validates the pure-deletion (END = BEG)
                ;; path: the after-change hook sees a deletion and must
                ;; not touch the buffer further.
                "* preexisting\n"
                "trailing\n"
                "#+end_user\n")
      (goto-char (point-min))
      (search-forward "trailing")
      ;; Delete the word `trailing' — a pure deletion (END = BEG).
      ;; The hook must skip.
      (delete-region (match-beginning 0) (match-end 0))
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      " * preexisting\n"
                      "\n"
                      "#+end_user\n"))))

  ;; ---------------------------------------------------------------------
  ;; Replacement scenario A: `replace-match' is the canonical
  ;; `query-replace' shape — a single mid-line replacement that fires
  ;; `after-change-functions' with LENGTH > 0 (length of the matched
  ;; text) AND END > BEG (length of the replacement).  Mid-line
  ;; replacement: the resulting `* H1' is concatenated onto the existing
  ;; line text (`some ' precedes), so its BOL is < BEG and the hook's
  ;; per-line scan correctly leaves it alone (mid-line semantics).
  ;; ---------------------------------------------------------------------
  (it "leaves mid-line replacement alone (replace-match in body, no col-0 *)"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "some foo here\n"
                "#+end_user\n")
      (goto-char (point-min))
      (search-forward "foo")
      (replace-match "* H1" t t)
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "some * H1 here\n"
                      "#+end_user\n"))))

  ;; ---------------------------------------------------------------------
  ;; Replacement scenario B (load-bearing): a single replacement event
  ;; (LENGTH > 0 AND END > BEG) that lands a column-0 `*' at BOL inside
  ;; a chat-block body.  This is the case the gate widening from
  ;; `(zerop length)' to `(> end beg)' was prescribed for —
  ;; `query-replace' / `replace-match' that introduces a brand-new
  ;; column-0 `*' line where none existed before.
  ;;
  ;; We use `search-forward' + `replace-match' to fire a single
  ;; `after-change-functions' event with both LENGTH > 0 (length of
  ;; matched `foo') AND END > BEG (length of replacement `* H1').
  ;; The match starts at column 0 so the replacement also starts at
  ;; column 0 — its BOL = BEG, which clears the BOL >= BEG guard, the
  ;; `\\*+ ' regex matches, and the predicate confirms body
  ;; membership.  Without the widening (`(zerop length)' gate),
  ;; LENGTH = 3 short-circuits and the column-0 `*' survives,
  ;; corrupting the block.
  ;; ---------------------------------------------------------------------
  (it "escapes column-0 `*' introduced via single-event replacement (replace-match) at BOL"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "foo bar\n"
                "#+end_user\n")
      ;; Position so the search match `foo' starts at the BOL of the
      ;; body line (column 0).
      (goto-char (point-min))
      (forward-line 1)                  ; on `foo bar' line, column 0
      (search-forward "foo")
      (replace-match "* H1" t t)
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      " * H1 bar\n"
                      "#+end_user\n"))))

  ;; ---------------------------------------------------------------------
  ;; Replacement scenario C: multi-line replacement via
  ;; `replace-region-contents'-shaped flow.  Delete + insert is two
  ;; events (the insert event would be caught by even the original
  ;; `(zerop length)' gate); to exercise the load-bearing single-event
  ;; replacement shape we use `replace-match' on a multi-line match,
  ;; substituting a multi-line replacement whose second line starts
  ;; with `*' at column 0.  This validates that the widened gate
  ;; correctly covers replacements whose second-and-later inserted
  ;; lines start at column 0 BOL, mirroring the multi-line yank
  ;; behaviour from scenario 1.
  ;; ---------------------------------------------------------------------
  (it "escapes column-0 `*' on later lines of a multi-line replacement"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "marker\n"
                "#+end_user\n")
      (goto-char (point-min))
      (search-forward "marker")
      ;; Single-event replacement: matched `marker' (LENGTH = 6) is
      ;; replaced with `prose\n* H2' (END - BEG = 8, spans two lines,
      ;; second line starts with column-0 `*').  The widened gate
      ;; admits the event; the per-line scan walks past the first
      ;; (mid-line) segment and escapes the column-0 `*' on the
      ;; second line.
      (replace-match "prose\n* H2" t t)
      (expect (buffer-substring-no-properties (point-min)
                                              (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "prose\n"
                      " * H2\n"
                      "#+end_user\n")))))

(provide 'gptel-chat-paste-escape-spec)

;;; paste-escape-spec.el ends here
