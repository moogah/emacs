;;; bash-coerce-spec.el --- jf/gptel-bash--coerce-to-json-safe contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Pins the contract for `jf/gptel-bash--coerce-to-json-safe', the
;; output sanitizer in config/gptel/scope/scope-shell-tools.el that
;; converts captured subprocess output into a JSON-serializable
;; multibyte string.
;;
;; Background — why this exists at all:
;;
;; `json-serialize' signals `wrong-type-argument json-value-p' when
;; a string leaf contains chars in Emacs's internal raw-byte range
;; (#x3FFF80..#x3FFFFF).  Those chars appear in subprocess output
;; whenever the locale/coding decoder can't decode a byte sequence
;; as valid UTF-8 — `cat' on a binary fragment, a tool that emits
;; legacy-encoded text, a no-conversion read, etc.
;;
;; Captured in a live session (May 2026): the bash tool returned a
;; result whose `:output' contained raw bytes from a cat-of-org-file
;; that had been written via raw-text encoding.  json-serialize
;; rejected the plist, the wrapper's outer condition-case caught
;; the error and produced a `tool_exception' payload, the next
;; HTTP request never went out, and the conversation halted.
;;
;; The coerce helper closes that hole at the source by ensuring
;; the bash tool never returns a string with raw byte chars to its
;; caller.  Sibling helper `jf/gptel-scope--serialize-tool-result'
;; in the wrapper is a downstream backstop for the same issue;
;; tests for it live in non-utf8-output-spec.el.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name "../.." test-dir)))
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope-shell-tools.el" scope-dir)))

(describe "jf/gptel-bash--coerce-to-json-safe"

  (it "passes ASCII strings through unchanged"
    (expect (jf/gptel-bash--coerce-to-json-safe "hello world")
            :to-equal "hello world"))

  (it "passes valid multibyte UTF-8 strings through unchanged"
    ;; Proper Unicode chars (U+2192 RIGHTWARDS ARROW etc.) are NOT
    ;; the raw-byte form; json-serialize already accepts them, so the
    ;; coercer must not touch them.
    (let ((s "foo → bar ✓"))
      (expect (jf/gptel-bash--coerce-to-json-safe s) :to-equal s)))

  (it "converts raw-byte chars into Latin-1 code points"
    ;; The captured failure shape: a multibyte string where some
    ;; chars are in the raw-byte range #x3FFF80..#x3FFFFF.  Those
    ;; chars are the bytes the UTF-8 decoder failed on.  Coerce
    ;; should re-emit them as their Latin-1 code points.
    (let* ((s (string-to-multibyte (unibyte-string 65 226 134 146 66)))
           (coerced (jf/gptel-bash--coerce-to-json-safe s)))
      (expect (mapcar #'identity (string-to-list coerced))
              :to-equal (list ?A #xE2 #x86 #x92 ?B))))

  (it "produces a json-serializable string for raw-byte input"
    ;; The whole point of this helper.  Pre-coerce: json-serialize
    ;; signals; post-coerce: json-serialize succeeds and round-trips
    ;; through json-parse-string to the expected char sequence.
    (let* ((s (string-to-multibyte (unibyte-string 65 226 134 146 66)))
           (pre-error (condition-case err (progn (json-serialize s) nil)
                        (wrong-type-argument err))))
      (expect pre-error :to-be-truthy)
      (let* ((json (json-serialize (jf/gptel-bash--coerce-to-json-safe s)))
             (parsed (json-parse-string json)))
        ;; Parsed string contains the same five chars (A, three Latin-1
        ;; chars, B) regardless of how json-serialize chose to escape.
        (expect (length parsed) :to-equal 5)
        (expect (mapcar #'identity (string-to-list parsed))
                :to-equal (list ?A #xE2 #x86 #x92 ?B)))))

  (it "handles a unibyte raw-byte string"
    ;; Defensive coverage: a unibyte string of raw bytes (rather than
    ;; a multibyte string holding raw-byte chars) round-trips cleanly.
    ;; Unibyte strings with bytes >= 0x80 don't carry raw-byte chars
    ;; per se (their chars are 0..255), so coerce passes them through
    ;; and json-serialize encodes them natively.
    (let* ((s (unibyte-string 255 254 253))
           (coerced (jf/gptel-bash--coerce-to-json-safe s))
           (json (json-serialize coerced))
           (parsed (json-parse-string json)))
      (expect (mapcar #'identity (string-to-list parsed))
              :to-equal (list 255 254 253))))

  (it "returns nil unchanged"
    (expect (jf/gptel-bash--coerce-to-json-safe nil) :to-be nil))

  (it "returns the empty string unchanged"
    (expect (jf/gptel-bash--coerce-to-json-safe "") :to-equal "")))

(provide 'bash-coerce-spec)

;;; bash-coerce-spec.el ends here
