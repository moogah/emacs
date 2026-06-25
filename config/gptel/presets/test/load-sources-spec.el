;;; load-sources-spec.el --- Specs for the fragment-source directory loader -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral specs for `jf/gptel-fragment--load-sources-all', the directory
;; loader that loads flat fragment SOURCE modules under
;; `config/gptel/presets/sources/' at gptel init so each source's composer-seam
;; side effect runs in production (not only under absolute-path unit loads).
;;
;; This is the wiring the `wire-fragment-sources-load' task adds: the env source
;; (`presets/sources/environment.el') was shipping DARK — never loaded at
;; runtime, so the composer seam `jf/gptel-fragment-environment-fn' stayed
;; `#'ignore' and `menu.el's soft `(require 'jf-gptel-fragment-environment nil
;; t)' silently no-op'd.
;;
;; Covered contract (register/boundary/sources-directory-load):
;;   - stage-1: the loader DISCOVERS the flat `sources/*.el' files via the real
;;     `jf/gptel-fragment-sources-directory' (derived from
;;     `jf/gptel-presets-directory'), parallel to `register-all's `<name>/preset.el'
;;     descent — NOT an explicit per-source file list.
;;   - stage-3 POST-CONDITION: after loading, `jf/gptel-fragment-environment-fn'
;;     is NOT `#'ignore' (it is the env producer), and the env source's feature
;;     is `provide'd so the chat menu's soft require resolves.
;;   - the loader is idempotent (re-load re-runs the same seam `setq').
;;   - a missing sources dir is a graceful no-op returning 0 (no error).
;;
;; This spec exercises the DISCOVERY+WIRING mechanism against the real source
;; directory; it deliberately does NOT load `environment.el' by absolute path
;; (that is what the unit spec for the env source already does).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (presets-dir (expand-file-name "../" test-dir))
       (gptel-dir (expand-file-name "../" presets-dir)))
  ;; `jf/gptel-presets-directory' (constants) is the root the sources dir is
  ;; derived from; load it so `jf/gptel-fragment-sources-directory' points at
  ;; the real `config/gptel/presets/sources/'.
  (require 'gptel-session-constants
           (expand-file-name "sessions/constants.el" gptel-dir))
  ;; The composer/renderer defines the seam var `jf/gptel-fragment-environment-fn'
  ;; that the env source mutates; it must load before the source.
  (require 'jf-gptel-fragments
           (expand-file-name "fragments.el" presets-dir))
  ;; The loader under test lives in the registration module.
  (require 'gptel-preset-registration
           (expand-file-name "registration.el" presets-dir)))

(describe "fragment-source directory loader"

  (describe "jf/gptel-fragment-sources-directory"

    (it "is derived from jf/gptel-presets-directory (real sources/ subdir)"
      (expect jf/gptel-fragment-sources-directory
              :to-equal (expand-file-name "sources/" jf/gptel-presets-directory)))

    (it "points at an existing directory in this checkout"
      (expect (file-directory-p jf/gptel-fragment-sources-directory)
              :to-be-truthy)))

  (describe "jf/gptel-fragment--load-sources-all"

    (before-each
      ;; Reset the seam to its dark default so the post-condition assertion is
      ;; meaningful: the loader must FLIP it away from `#'ignore'.
      (setq jf/gptel-fragment-environment-fn #'ignore))

    (it "loads at least one source from the real sources/ directory"
      ;; stage-1: discovery finds the flat env source (and any siblings) without
      ;; an explicit per-source list.
      (expect (jf/gptel-fragment--load-sources-all)
              :to-be-greater-than 0))

    (it "populates the env composer seam (no longer #'ignore)"
      ;; stage-3 POST-CONDITION: the env producer is wired after init.
      (jf/gptel-fragment--load-sources-all)
      (expect jf/gptel-fragment-environment-fn :not :to-equal #'ignore)
      (expect (functionp jf/gptel-fragment-environment-fn) :to-be-truthy))

    (it "provides jf-gptel-fragment-environment so the menu's soft require resolves"
      ;; menu.el keeps a soft `(require 'jf-gptel-fragment-environment nil t)';
      ;; the directory loader must make that feature available at init.
      (jf/gptel-fragment--load-sources-all)
      (expect (featurep 'jf-gptel-fragment-environment) :to-be-truthy))

    (it "is idempotent: re-loading keeps the seam populated, count unchanged"
      (let ((first (jf/gptel-fragment--load-sources-all))
            (second (jf/gptel-fragment--load-sources-all)))
        (expect second :to-equal first)
        (expect jf/gptel-fragment-environment-fn :not :to-equal #'ignore)))

    (it "is a graceful no-op (returns 0) when the sources dir is missing"
      (let ((jf/gptel-fragment-sources-directory
             (expand-file-name "does-not-exist-xyzzy/"
                               jf/gptel-presets-directory)))
        (expect (jf/gptel-fragment--load-sources-all) :to-equal 0))))

  (describe "load-error fail-policy (source .el that throws on load)"
    ;; A source whose .el signals while loading must NOT be swallowed with only
    ;; a warn-log that leaves its composer seam dark.  The shared loader helper
    ;; (a) logs at ERROR level naming the file AND the seam, and (b) records the
    ;; failure so a post-init self-check can assert "N source(s) failed; seam X
    ;; is dark".  Here we point the loader at a TEMP fixture dir (not the real
    ;; sources/) holding one good and one throwing source.

    (let (tmp-dir orig-dir)

      (before-each
        (setq orig-dir jf/gptel-fragment-sources-directory)
        (setq tmp-dir (make-temp-file "gptel-sources-failpolicy-test" t))
        (setq jf/gptel-fragment-sources-directory (file-name-as-directory tmp-dir))
        (jf/gptel-loader-clear-failures))

      (after-each
        (when (and tmp-dir (file-directory-p tmp-dir))
          (delete-directory tmp-dir t))
        (setq jf/gptel-fragment-sources-directory orig-dir)
        (jf/gptel-loader-clear-failures))

      (cl-flet ((write-source
                  (name body)
                  (with-temp-file (expand-file-name name tmp-dir)
                    (insert body))))

        (it "does not hard-fail the whole load when one source throws"
          ;; A bad source must not abort the others: gptel init survives.
          (write-source "good.el"
                        "(provide 'gptel-test-good-source)")
          (write-source "bad.el"
                        "(error \"boom from bad source\")")
          ;; The good source still loads (count 1), no error escapes.
          (expect (jf/gptel-fragment--load-sources-all) :to-equal 1))

        (it "records the failure (not a silent warn) naming the file and seam"
          (write-source "bad.el" "(error \"boom from bad source\")")
          (jf/gptel-fragment--load-sources-all)
          (let ((failures (jf/gptel-loader-failures)))
            (expect (length failures) :to-equal 1)
            (let ((rec (car failures)))
              (expect (plist-get rec :file) :to-match "bad\\.el\\'")
              ;; The seam this source failed to populate is named, so a
              ;; self-check can report which seam is dark.
              (expect (plist-get rec :seam)
                      :to-equal 'jf/gptel-fragment-environment-fn))))

        (it "logs the load error at ERROR level (not warn)"
          (write-source "bad.el" "(error \"boom from bad source\")")
          (let ((logged nil))
            (cl-letf (((symbol-function 'jf/gptel--log)
                       (lambda (level &rest args)
                         (push (cons level (apply #'format args)) logged))))
              (jf/gptel-fragment--load-sources-all))
            ;; The per-entry failure log is ERROR level and names the file.
            (expect (cl-find-if
                     (lambda (e) (and (eq (car e) 'error)
                                      (string-match-p "bad\\.el" (cdr e))))
                     logged)
                    :to-be-truthy)
            ;; And it must NOT be downgraded to a bare warn.
            (expect (cl-find-if
                     (lambda (e) (and (eq (car e) 'warn)
                                      (string-match-p "bad\\.el" (cdr e))))
                     logged)
                    :to-be nil)))

        (it "flags the env seam as dark via the post-init self-check"
          (write-source "bad.el" "(error \"boom from bad source\")")
          (jf/gptel-fragment--load-sources-all)
          (expect (jf/gptel-loader-seam-dark-p 'jf/gptel-fragment-environment-fn)
                  :to-be-truthy))))))

(provide 'load-sources-spec)
;;; load-sources-spec.el ends here
