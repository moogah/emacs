;;; composer-spec.el --- Specs for the fragment composer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the composition layer of
;; `config/gptel/presets/fragments.el':
;;
;; - Composition shape: ordered list of fragment references
;;   (static -> :text verbatim; dynamic -> (funcall :fn context)).
;; - `jf/gptel-fragment-compose': realizes references in list order and joins
;;   the non-empty results with a blank line; order is preserved exactly.
;; - Empty/absent contributions are SKIPPED (not rejected, not emitted as a
;;   blank block).
;; - Dynamic references reflect live input at compose time; default to the tail
;;   but explicit non-tail placement is honored.
;; - `jf/gptel-fragment--default-composition': chat/agent context defaults with
;;   the role fragment occupying the role position and a tail dynamic env ref.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test.
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (presets-dir (expand-file-name ".." test-dir)))
  (require 'jf-gptel-fragments (expand-file-name "fragments.el" presets-dir)))

(describe "Fragment reference constructors"

  (it "builds a static reference carrying verbatim text"
    (let ((ref (jf/gptel-fragment-ref-static "hello")))
      (expect (plist-get ref :kind) :to-equal 'static)
      (expect (plist-get ref :text) :to-equal "hello")))

  (it "coerces nil static text to the empty string"
    (expect (plist-get (jf/gptel-fragment-ref-static nil) :text)
            :to-equal ""))

  (it "builds a dynamic reference carrying a function"
    (let* ((fn (lambda (_ctx) "x"))
           (ref (jf/gptel-fragment-ref-dynamic fn)))
      (expect (plist-get ref :kind) :to-equal 'dynamic)
      (expect (plist-get ref :fn) :to-equal fn))))

(describe "jf/gptel-fragment-compose"

  (it "joins fragments in list order [A,B,C] with a blank line"
    (let ((composition (list (jf/gptel-fragment-ref-static "A")
                             (jf/gptel-fragment-ref-static "B")
                             (jf/gptel-fragment-ref-static "C"))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "A\n\nB\n\nC")))

  (it "renders a single-fragment composition as just that fragment"
    (let ((composition (list (jf/gptel-fragment-ref-static "only"))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "only")))

  (it "preserves order even when the same texts are reordered"
    (let ((c1 (list (jf/gptel-fragment-ref-static "A")
                    (jf/gptel-fragment-ref-static "B")))
          (c2 (list (jf/gptel-fragment-ref-static "B")
                    (jf/gptel-fragment-ref-static "A"))))
      (expect (jf/gptel-fragment-compose c1 'claude) :to-equal "A\n\nB")
      (expect (jf/gptel-fragment-compose c2 'claude) :to-equal "B\n\nA")))

  (it "consumes static text verbatim (no compose-time rendering)"
    ;; Pre-rendered XML text passes through untouched.
    (let ((composition (list (jf/gptel-fragment-ref-static
                              "<role>\nYou are X.\n</role>"))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "<role>\nYou are X.\n</role>")))

  (it "evaluates a dynamic fragment at compose time, reflecting live input"
    (let* ((counter 0)
           (ref (jf/gptel-fragment-ref-dynamic
                 (lambda (_ctx)
                   (setq counter (1+ counter))
                   (format "call-%d" counter))))
           (composition (list (jf/gptel-fragment-ref-static "lead") ref)))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "lead\n\ncall-1")
      ;; Re-composing re-evaluates the dynamic fragment.
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "lead\n\ncall-2")))

  (it "passes the compose-time context through to dynamic functions"
    (let ((ref (jf/gptel-fragment-ref-dynamic
                (lambda (ctx) (format "ctx=%s" ctx)))))
      (expect (jf/gptel-fragment-compose (list ref) 'claude 'agent)
              :to-equal "ctx=agent")))

  (it "honors a dynamic fragment at the tail (default position)"
    (let ((composition (list (jf/gptel-fragment-ref-static "static-prefix")
                             (jf/gptel-fragment-ref-dynamic
                              (lambda (_ctx) "dynamic-tail")))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "static-prefix\n\ndynamic-tail")))

  (it "honors explicit non-tail dynamic placement"
    (let ((composition (list (jf/gptel-fragment-ref-dynamic
                              (lambda (_ctx) "dynamic-head"))
                             (jf/gptel-fragment-ref-static "static-tail"))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "dynamic-head\n\nstatic-tail")))

  (it "coerces a non-string dynamic result to a string"
    (let ((composition (list (jf/gptel-fragment-ref-dynamic (lambda (_) 42)))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "42")))

  (it "signals for a reference with an unrecognized kind"
    (expect (jf/gptel-fragment-compose (list (list :kind 'bogus)) 'claude)
            :to-throw 'jf/gptel-fragment-bad-reference)))

(describe "Empty / absent contributions"

  (it "skips an empty static reference rather than emitting a blank block"
    (let ((composition (list (jf/gptel-fragment-ref-static "A")
                             (jf/gptel-fragment-ref-static "")
                             (jf/gptel-fragment-ref-static "B"))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "A\n\nB")))

  (it "skips a whitespace-only contribution"
    (let ((composition (list (jf/gptel-fragment-ref-static "A")
                             (jf/gptel-fragment-ref-static "   \n  ")
                             (jf/gptel-fragment-ref-static "B"))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "A\n\nB")))

  (it "skips a dynamic reference that produces empty text"
    (let ((composition (list (jf/gptel-fragment-ref-static "A")
                             (jf/gptel-fragment-ref-dynamic (lambda (_) ""))
                             (jf/gptel-fragment-ref-static "B"))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "A\n\nB")))

  (it "treats a nil dynamic result as an empty (skipped) contribution"
    (let ((composition (list (jf/gptel-fragment-ref-static "A")
                             (jf/gptel-fragment-ref-dynamic #'ignore)
                             (jf/gptel-fragment-ref-static "B"))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal "A\n\nB")))

  (it "yields the empty string when every contribution is empty"
    (let ((composition (list (jf/gptel-fragment-ref-static "")
                             (jf/gptel-fragment-ref-static "  "))))
      (expect (jf/gptel-fragment-compose composition 'claude)
              :to-equal ""))))

(describe "jf/gptel-fragment--default-composition"

  (it "builds the chat default = [prelude, role, env]"
    (let ((jf/gptel-fragment-chat-prelude-text "PRELUDE")
          (jf/gptel-fragment-environment-fn (lambda (_) "ENV")))
      (let* ((role (jf/gptel-fragment-ref-static "ROLE"))
             (composition (jf/gptel-fragment--default-composition 'chat role)))
        (expect (length composition) :to-equal 3)
        (expect (plist-get (nth 0 composition) :kind) :to-equal 'static)
        (expect (plist-get (nth 0 composition) :text) :to-equal "PRELUDE")
        (expect (nth 1 composition) :to-equal role)
        (expect (plist-get (nth 2 composition) :kind) :to-equal 'dynamic)
        (expect (jf/gptel-fragment-compose composition 'claude)
                :to-equal "PRELUDE\n\nROLE\n\nENV"))))

  (it "builds the agent default = [preamble, role, env]"
    (let ((jf/gptel-fragment-agent-preamble-text "PREAMBLE")
          (jf/gptel-fragment-environment-fn (lambda (_) "ENV")))
      (let* ((role (jf/gptel-fragment-ref-static "ROLE"))
             (composition (jf/gptel-fragment--default-composition 'agent role)))
        (expect (plist-get (nth 0 composition) :text) :to-equal "PREAMBLE")
        (expect (jf/gptel-fragment-compose composition 'claude)
                :to-equal "PREAMBLE\n\nROLE\n\nENV"))))

  (it "places the dynamic environment reference at the tail"
    (let ((composition (jf/gptel-fragment--default-composition
                        'chat (jf/gptel-fragment-ref-static "ROLE"))))
      (expect (plist-get (car (last composition)) :kind) :to-equal 'dynamic)))

  (it "uses the author's single role fragment in the role position"
    (let ((jf/gptel-fragment-chat-prelude-text "PRELUDE")
          (jf/gptel-fragment-environment-fn #'ignore))
      ;; Common case: author supplies only a role fragment; context default
      ;; surrounds it with prelude + (empty) env.
      (let* ((role (jf/gptel-fragment-ref-static
                    "<role>\nYou are X.\n</role>"))
             (composition (jf/gptel-fragment--default-composition 'chat role)))
        (expect (jf/gptel-fragment-compose composition 'claude)
                :to-equal "PRELUDE\n\n<role>\nYou are X.\n</role>"))))

  (it "collapses an absent role to an empty contribution (prelude still leads)"
    (let ((jf/gptel-fragment-chat-prelude-text "PRELUDE")
          (jf/gptel-fragment-environment-fn (lambda (_) "ENV")))
      ;; No role-ref supplied: role slot is empty and skipped at compose time.
      (let ((composition (jf/gptel-fragment--default-composition 'chat)))
        (expect (length composition) :to-equal 3)
        (expect (jf/gptel-fragment-compose composition 'claude)
                :to-equal "PRELUDE\n\nENV"))))

  (it "signals for an unknown send context"
    (expect (jf/gptel-fragment--default-composition 'frobnicate)
            :to-throw 'jf/gptel-fragment-unknown-context)))

(provide 'composer-spec)
;;; composer-spec.el ends here
