;;; single-root-spec.el --- One sessions root for creation and enumeration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Enforcement for register/invariant/sessions-single-root
;; (spec: sessions-persistence § "Single sessions root").
;;
;; Every session the module creates lands under
;; `jf/gptel-sessions-directory', and every enumerator walks that same
;; root, so "created" implies "listed" by construction.  The module
;; consults no other package to choose a location, exposes no
;; per-context alternative root, and has no "-global"-suffixed
;; creation command.
;;
;; Scenario mapping:
;; - "Every created session is enumerable": a session created with
;;   `jf/gptel-sessions-directory' bound to a temp dir is found by
;;   `jf/gptel--init-registry' in the same process.
;; - "No other package is consulted": the tangled
;;   config/gptel/sessions/*.el files contain no `workspace-' /
;;   `org-graph' symbol, and the removed resolver / command are unbound.
;;
;; Tests mount on a temporary sessions root and run the real creation
;; path; only the scope-profile drawer renderer is stubbed (it needs a
;; registered preset, which is orthogonal to root resolution).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)

(defconst single-root-spec--sessions-src-dir
  (expand-file-name
   "../.."
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to config/gptel/sessions/ (the module source directory).")

(defvar single-root-spec--tempdirs nil
  "Temporary directories created during the suite, for cleanup.")

(defun single-root-spec--make-tempdir ()
  "Create a fresh temporary directory and register it for cleanup."
  (let ((dir (make-temp-file "gptel-single-root-" t)))
    (push dir single-root-spec--tempdirs)
    dir))

(defun single-root-spec--create-session (session-id)
  "Run the real creation path for SESSION-ID under the bound root.
Returns the plist from `jf/gptel--create-session-core'."
  (cl-letf (((symbol-function 'jf/gptel-scope-profile--create-for-session)
             (lambda (&rest _) ":PROPERTIES:\n:GPTEL_PRESET: executor\n:END:\n")))
    (let ((session-dir (jf/gptel--create-session-directory session-id)))
      (jf/gptel--create-session-core session-id session-dir 'executor))))

(describe "Single sessions root (register/invariant/sessions-single-root)"

  :var (root)

  (before-each
    (setq root (single-root-spec--make-tempdir)))

  (after-each
    (dolist (dir single-root-spec--tempdirs)
      (when (and dir (file-directory-p dir))
        (delete-directory dir t)))
    (setq single-root-spec--tempdirs nil))

  (describe "creation targets jf/gptel-sessions-directory"

    (it "creates the session directory directly under the bound root"
      ;; Scenario: "Every created session is enumerable" (creation half)
      (let ((jf/gptel-sessions-directory root))
        (let ((session-dir (jf/gptel--create-session-directory "notes-20260913120000")))
          (expect (file-directory-p session-dir) :to-be t)
          (expect (file-name-directory (directory-file-name session-dir))
                  :to-equal (file-name-as-directory (expand-file-name root))))))

    (it "resolves the root through jf/gptel--ensure-sessions-root alone"
      (let ((jf/gptel-sessions-directory root))
        (spy-on 'jf/gptel--ensure-sessions-root :and-call-through)
        (jf/gptel--create-session-directory "notes-20260913120001")
        (expect 'jf/gptel--ensure-sessions-root :to-have-been-called-times 1)))

    (it "takes only the session id: there is no FORCE-GLOBAL parameter"
      (expect (func-arity 'jf/gptel--create-session-directory) :to-equal '(1 . 1)))

    (it "jf/gptel-persistent-session no longer threads a FORCE-GLOBAL argument"
      (expect (func-arity 'jf/gptel-persistent-session) :to-equal '(1 . 4))))

  (describe "enumeration walks the same root"

    (it "lists a session created in the same process via jf/gptel--init-registry"
      ;; Scenario: "Every created session is enumerable"
      (let ((jf/gptel-sessions-directory root)
            (jf/gptel--session-registry (make-hash-table :test 'equal)))
        (let ((info (single-root-spec--create-session "notes-20260913120000")))
          (expect (file-exists-p (plist-get info :session-file)) :to-be t)
          (jf/gptel--init-registry)
          (let ((entry (gethash (jf/gptel--registry-key "notes-20260913120000" "main")
                                jf/gptel--session-registry)))
            (expect entry :not :to-be nil)
            (expect (plist-get entry :session-id) :to-equal "notes-20260913120000")
            (expect (plist-get entry :session-dir)
                    :to-equal (plist-get info :session-dir))))))

    (it "finds the created session through every enumerator"
      (let ((jf/gptel-sessions-directory root))
        (let ((info (single-root-spec--create-session "notes-20260913120000")))
          (expect (jf/gptel--list-session-directories)
                  :to-equal (list (plist-get info :session-dir)))
          (expect (jf/gptel--find-session-directory "notes-20260913120000")
                  :to-equal (plist-get info :session-dir))
          (let ((branches (jf/gptel--find-all-branches-with-agents)))
            (expect (length branches) :to-equal 1)
            (expect (plist-get (car branches) :session-id)
                    :to-equal "notes-20260913120000")
            (expect (plist-get (car branches) :branch-name) :to-equal "main"))))))

  (describe "no other package is consulted"

    (it "has no per-context root resolver and no -global creation command"
      (expect (fboundp 'jf/gptel--target-sessions-root) :to-be nil)
      (expect (fboundp 'jf/gptel-persistent-session-global) :to-be nil))

    (it "tangled config/gptel/sessions/*.el name no workspace- or org-graph symbol"
      ;; Scenario: "No other package is consulted".  Interim grep lint
      ;; for register/invariant/packages-independent-no-cross-naming;
      ;; the integrations boundary lint absorbs it once it lands.
      (let ((offenders nil))
        (dolist (file (directory-files single-root-spec--sessions-src-dir t "\\.el\\'"))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "workspace-\\|org-graph" nil t)
              (push (format "%s:%d: %s"
                            (file-name-nondirectory file)
                            (line-number-at-pos)
                            (string-trim (thing-at-point 'line t)))
                    offenders))))
        (expect offenders :to-equal nil)))))

(provide 'single-root-spec)
;;; single-root-spec.el ends here
