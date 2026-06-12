;;; integration-registry-spec.el --- Tests for workspace integration registry -*- lexical-binding: t; -*-

;; Covers the `workspace-integrations' capability (design.md §Decisions
;; 1-4): the ordered-alist registry, `workspace-register-integration'
;; (register / replace-in-place / order / neither-surface user-error),
;; the `workspace--integration-payload' constructor (shape +
;; :sessions-dir derivation + :context passthrough), the
;; `workspace--run-one-integration' result protocol + error guard, and
;; `workspace--dispatch-create-integrations' (in-order, skip no-handler,
;; never-signals).
;;
;; The registry is generic: this spec names NO gptel symbol (the
;; directionality contract — workspaces publishes, gptel attaches).

(require 'buttercup)
(require 'cl-lib)

(defconst integration-registry-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

;; The registry depends on `workspace--sessions-dir' from data-model.el.
(load (expand-file-name "../data-model.el" integration-registry-spec--this-dir))
(load (expand-file-name "../integrations.el" integration-registry-spec--this-dir))

(describe "workspace-integrations registry"

  ;; Isolate the global registry per test (Decision 1 — mutation in place).
  (before-each
    (setq workspace--integrations nil))

  (describe "workspace-register-integration"

    (it "registers an integration and stores it as (ID . PLIST)"
      (workspace-register-integration 'alpha :label "Alpha" :on-create #'ignore)
      (expect (length workspace--integrations) :to-equal 1)
      (let ((cell (assq 'alpha workspace--integrations)))
        (expect (car cell) :to-equal 'alpha)
        (expect (plist-get (cdr cell) :label) :to-equal "Alpha")
        (expect (plist-get (cdr cell) :on-create) :to-equal #'ignore)))

    (it "returns the registered ID"
      (expect (workspace-register-integration 'alpha :on-create #'ignore)
              :to-equal 'alpha))

    (it "accepts a :menu-only integration"
      (workspace-register-integration 'm :label "M" :menu '(?m . some-command))
      (let ((cell (assq 'm workspace--integrations)))
        (expect (plist-get (cdr cell) :menu) :to-equal '(?m . some-command))
        (expect (plist-get (cdr cell) :on-create) :to-be nil)))

    (it "signals user-error when neither :on-create nor :menu is given"
      (expect (workspace-register-integration 'inert :label "Inert")
              :to-throw 'user-error))

    (it "preserves registration order across multiple registrations"
      (workspace-register-integration 'a :on-create #'ignore)
      (workspace-register-integration 'b :on-create #'ignore)
      (workspace-register-integration 'c :on-create #'ignore)
      (expect (mapcar #'car workspace--integrations) :to-equal '(a b c)))

    (it "replaces an existing ID in place, preserving its position"
      (workspace-register-integration 'a :label "A" :on-create #'ignore)
      (workspace-register-integration 'b :label "B" :on-create #'ignore)
      (workspace-register-integration 'c :label "C" :on-create #'ignore)
      ;; Re-register the middle entry with a new label.
      (workspace-register-integration 'b :label "B2" :on-create #'ignore)
      (expect (mapcar #'car workspace--integrations) :to-equal '(a b c))
      (expect (plist-get (cdr (assq 'b workspace--integrations)) :label)
              :to-equal "B2")
      (expect (length workspace--integrations) :to-equal 3)))

  (describe "workspace--integration-payload"

    (it "builds a flat plist with :name :home :sessions-dir :context"
      (let ((payload (workspace--integration-payload "proj" "/tmp/proj" 'fresh)))
        (expect (plist-get payload :name) :to-equal "proj")
        (expect (plist-get payload :home) :to-equal "/tmp/proj")
        (expect (plist-get payload :context) :to-equal 'fresh)))

    (it "derives :sessions-dir via workspace--sessions-dir"
      (let ((payload (workspace--integration-payload "proj" "/tmp/proj" 'fresh)))
        (expect (plist-get payload :sessions-dir)
                :to-equal (workspace--sessions-dir "/tmp/proj"))))

    (it "passes through each documented :context symbol"
      (dolist (ctx '(fresh anchored-scaffolded anchored-existing))
        (expect (plist-get (workspace--integration-payload "p" "/tmp/p" ctx)
                           :context)
                :to-equal ctx))))

  (describe "workspace--run-one-integration"

    (it "maps an `ok' handler return to (ID . ok)"
      (let ((entry (cons 'h (list :on-create (lambda (_p) 'ok)))))
        (expect (workspace--run-one-integration entry '(:name "p"))
                :to-equal '(h . ok))))

    (it "maps a `skipped' handler return to (ID . skipped)"
      (let ((entry (cons 'h (list :on-create (lambda (_p) 'skipped)))))
        (expect (workspace--run-one-integration entry '(:name "p"))
                :to-equal '(h . skipped))))

    (it "maps a (failed . REASON) handler return to (ID failed . REASON)"
      (let ((entry (cons 'h (list :on-create (lambda (_p) '(failed . "boom"))))))
        (expect (workspace--run-one-integration entry '(:name "p"))
                :to-equal '(h failed . "boom"))))

    (it "treats an unknown return value leniently as ok"
      (let ((entry (cons 'h (list :on-create (lambda (_p) 42)))))
        (expect (workspace--run-one-integration entry '(:name "p"))
                :to-equal '(h . ok))))

    (it "normalizes a signalled error to (ID failed . MESSAGE)"
      (let ((entry (cons 'h (list :on-create
                                  (lambda (_p) (error "kaboom"))))))
        (let ((result (workspace--run-one-integration entry '(:name "p"))))
          (expect (car result) :to-equal 'h)
          (expect (cadr result) :to-equal 'failed)
          (expect (cddr result) :to-equal "kaboom"))))

    (it "passes the payload through to the handler"
      (let* ((seen nil)
             (entry (cons 'h (list :on-create
                                   (lambda (p) (setq seen p) 'ok)))))
        (workspace--run-one-integration entry '(:name "proj" :context fresh))
        (expect (plist-get seen :name) :to-equal "proj")
        (expect (plist-get seen :context) :to-equal 'fresh))))

  (describe "workspace--dispatch-create-integrations"

    (it "runs every :on-create handler once, in registration order"
      (let ((calls nil))
        (workspace-register-integration
         'a :on-create (lambda (_p) (push 'a calls) 'ok))
        (workspace-register-integration
         'b :on-create (lambda (_p) (push 'b calls) 'ok))
        (workspace-register-integration
         'c :on-create (lambda (_p) (push 'c calls) 'ok))
        (workspace--dispatch-create-integrations "proj" "/tmp/proj" 'fresh)
        (expect (nreverse calls) :to-equal '(a b c))))

    (it "returns the result alist in registration order"
      (workspace-register-integration 'a :on-create (lambda (_p) 'ok))
      (workspace-register-integration 'b :on-create (lambda (_p) 'skipped))
      (expect (workspace--dispatch-create-integrations "p" "/tmp/p" 'fresh)
              :to-equal '((a . ok) (b . skipped))))

    (it "skips entries lacking an :on-create handler"
      (workspace-register-integration 'menu-only :menu '(?m . cmd))
      (workspace-register-integration 'has-handler :on-create (lambda (_p) 'ok))
      (let ((results (workspace--dispatch-create-integrations
                      "p" "/tmp/p" 'fresh)))
        (expect (mapcar #'car results) :to-equal '(has-handler))))

    (it "feeds the constructed anchor payload to handlers"
      (let ((seen nil))
        (workspace-register-integration
         'a :on-create (lambda (p) (setq seen p) 'ok))
        (workspace--dispatch-create-integrations "proj" "/tmp/proj" 'fresh)
        (expect (plist-get seen :name) :to-equal "proj")
        (expect (plist-get seen :home) :to-equal "/tmp/proj")
        (expect (plist-get seen :sessions-dir)
                :to-equal (workspace--sessions-dir "/tmp/proj"))
        (expect (plist-get seen :context) :to-equal 'fresh)))

    (it "never signals when a handler errors, and surfaces it via message"
      (workspace-register-integration
       'boom :on-create (lambda (_p) (error "explode")))
      (spy-on 'message)
      (let (results)
        (expect (setq results (workspace--dispatch-create-integrations
                               "p" "/tmp/p" 'fresh))
                :not :to-throw)
        ;; Single (ID failed . REASON) result.
        (expect (caar results) :to-equal 'boom)
        (expect (cadar results) :to-equal 'failed)
        (expect (cddar results) :to-equal "explode")
        (expect 'message :to-have-been-called-with
                "workspace: integration %s failed: %s" 'boom "explode")))

    (it "messages each (failed . reason) outcome the handler returns"
      (workspace-register-integration
       'f :on-create (lambda (_p) '(failed . "declined")))
      (spy-on 'message)
      (workspace--dispatch-create-integrations "p" "/tmp/p" 'fresh)
      (expect 'message :to-have-been-called-with
              "workspace: integration %s failed: %s" 'f "declined"))

    (it "does not message for ok or skipped outcomes"
      (workspace-register-integration 'ok-one :on-create (lambda (_p) 'ok))
      (workspace-register-integration 'skip-one :on-create (lambda (_p) 'skipped))
      (spy-on 'message)
      (workspace--dispatch-create-integrations "p" "/tmp/p" 'fresh)
      (expect 'message :not :to-have-been-called))

    (it "returns nil and never signals with an empty registry"
      (expect (workspace--dispatch-create-integrations "p" "/tmp/p" 'fresh)
              :to-equal nil))))

(provide 'integration-registry-spec)
;;; integration-registry-spec.el ends here
