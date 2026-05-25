;;; home-org-spec.el --- Tests for workspace home.org reader -*- lexical-binding: t; -*-

;; Covers register/boundary/home-org-read-pipeline (stages 1-5) and the
;; reader-module half of register/invariant/home-org-user-authored-
;; after-creation (reader module performs zero file writes).

(require 'buttercup)

(defconst home-org-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(defconst home-org-spec--module-path
  (expand-file-name "../home-org.el" home-org-spec--this-dir)
  "Absolute path to the reader module under test.")

(load home-org-spec--module-path)

(defun home-org-spec--make-tmp-home ()
  "Create and return an empty temp directory for use as a workspace HOME."
  (make-temp-file "ws-home-org-spec-" t))

(defmacro home-org-spec--with-tmp-home (var &rest body)
  "Bind VAR to a fresh temp HOME directory and run BODY, cleaning up after."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (home-org-spec--make-tmp-home)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun home-org-spec--write-home-org (home contents)
  "Write CONTENTS into <HOME>/home.org."
  (let ((path (expand-file-name "home.org" home)))
    (with-temp-file path
      (insert contents))))

;;; Stage 1: path resolution

(describe "workspace-home-org-path"
  (it "returns HOME/home.org as an absolute path"
    (expect (workspace-home-org-path "/tmp/ws-alpha")
            :to-equal "/tmp/ws-alpha/home.org"))

  (it "is pure — does not touch the filesystem"
    ;; Path resolves cleanly for a directory that does not exist.
    (expect (workspace-home-org-path "/nonexistent/path/that/does/not/exist")
            :to-equal "/nonexistent/path/that/does/not/exist/home.org")))

;;; Stage 2: existence check

(describe "workspace-home-org-exists-p"
  (it "returns nil when HOME has no home.org"
    (home-org-spec--with-tmp-home home
      (expect (workspace-home-org-exists-p home) :to-be nil)))

  (it "returns non-nil when HOME contains a readable home.org"
    (home-org-spec--with-tmp-home home
      (home-org-spec--write-home-org home "#+TITLE: Alpha\n")
      (expect (workspace-home-org-exists-p home) :not :to-be nil)))

  (it "returns nil when HOME itself does not exist"
    (expect (workspace-home-org-exists-p
             "/nonexistent/path/that/does/not/exist")
            :to-be nil)))

;;; Stages 3-5: title reader

(describe "workspace-home-org-title"
  (it "returns nil on a missing file (stage 2 short-circuit)"
    (home-org-spec--with-tmp-home home
      (expect (workspace-home-org-title home) :to-be nil)))

  (it "returns nil on a file with no #+TITLE: keyword (stage 4 short-circuit)"
    (home-org-spec--with-tmp-home home
      (home-org-spec--write-home-org home "* Just a heading\nSome text.\n")
      (expect (workspace-home-org-title home) :to-be nil)))

  (it "returns the value of a basic #+TITLE: keyword"
    (home-org-spec--with-tmp-home home
      (home-org-spec--write-home-org home "#+TITLE: Foo\n")
      (expect (workspace-home-org-title home) :to-equal "Foo")))

  (it "trims surrounding whitespace from the value"
    (home-org-spec--with-tmp-home home
      (home-org-spec--write-home-org home "#+TITLE:   spaces around   \n")
      (expect (workspace-home-org-title home) :to-equal "spaces around")))

  (it "matches lowercase #+title: case-insensitively"
    (home-org-spec--with-tmp-home home
      (home-org-spec--write-home-org home "#+title: Lowercase\n")
      (expect (workspace-home-org-title home) :to-equal "Lowercase")))

  (it "returns nil when the value is empty after trimming (stage 5 short-circuit)"
    (home-org-spec--with-tmp-home home
      ;; The regex requires at least one space/tab after the colon, so a
      ;; bare "#+TITLE:" with no following whitespace does not match
      ;; (stage 4 short-circuits). The stage 5 short-circuit is reached
      ;; when there IS following whitespace but no actual value.
      (home-org-spec--write-home-org home "#+TITLE:    \n")
      (expect (workspace-home-org-title home) :to-be nil)))

  (it "finds #+TITLE: even when it is not on the first line"
    (home-org-spec--with-tmp-home home
      (home-org-spec--write-home-org
       home
       "# -*- mode: org -*-\n# Some comment\n#+TITLE: Later Title\n")
      (expect (workspace-home-org-title home) :to-equal "Later Title")))

  (it "returns the first #+TITLE: when several are present"
    ;; Documents current behavior: re-search-forward stops at the first
    ;; match. If two titles disagree the user gets the topmost.
    (home-org-spec--with-tmp-home home
      (home-org-spec--write-home-org
       home
       "#+TITLE: First\n#+TITLE: Second\n")
      (expect (workspace-home-org-title home) :to-equal "First"))))

;;; Invariant: reader module has zero file-write primitives.

(describe "Invariant: reader module is read-only"
  (it "config/workspaces/home-org.el contains no file-write primitives"
    (let ((write-syms '(write-region with-temp-file append-to-file
                                     delete-file rename-file copy-file
                                     write-file save-buffer)))
      (expect (file-readable-p home-org-spec--module-path) :not :to-be nil)
      (with-temp-buffer
        (insert-file-contents home-org-spec--module-path)
        (dolist (sym write-syms)
          (goto-char (point-min))
          (expect (re-search-forward
                   (concat "(" (regexp-quote (symbol-name sym)) "\\_>")
                   nil t)
                  :to-be nil))))))

(provide 'home-org-spec)
;;; home-org-spec.el ends here
