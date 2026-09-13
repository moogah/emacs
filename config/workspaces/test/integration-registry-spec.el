;;; integration-registry-spec.el --- Tests for workspace integration registry -*- lexical-binding: t; -*-

;; Covers the `workspace-integrations' capability (design.md §Decisions
;; 1-4): the ordered-alist registry, `workspace-register-integration'
;; (register / replace-in-place / order / no-surface user-error /
;; :on-purge surface), the `workspace--integration-payload' constructor
;; (shape from the live record: :name :home :node-id :context, NO
;; :sessions-dir, and :context passthrough), the
;; `workspace--run-one-integration' result protocol + error guard, and
;; `workspace--dispatch-create-integrations' /
;; `workspace--dispatch-purge-integrations' (in-order, skip no-handler,
;; never-signals).
;;
;; The registry is generic: this spec names NO gptel symbol (the
;; directionality contract — workspaces publishes, gptel attaches).

(require 'buttercup)
(require 'cl-lib)

(defconst integration-registry-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

;; The registry reads the record accessors from data-model.el.
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

    (it "accepts an :on-purge-only integration"
      (workspace-register-integration 'p :label "P" :on-purge #'ignore)
      (let ((cell (assq 'p workspace--integrations)))
        (expect (plist-get (cdr cell) :on-purge) :to-equal #'ignore)
        (expect (plist-get (cdr cell) :on-create) :to-be nil)
        (expect (plist-get (cdr cell) :menu) :to-be nil)))

    (it "signals user-error when none of :on-create, :menu, :on-purge is given"
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
    ;; register/shape/workspace-integration-anchor-payload; workspace-
    ;; integrations spec "Anchor payload contract (push, not consult)".

    (it "builds a flat plist with :name :home :node-id :context from the record"
      (let ((payload (workspace--integration-payload
                      (workspace--make "proj" "/tmp/proj/" "ABC") 'fresh)))
        (expect (plist-get payload :name) :to-equal "proj")
        (expect (plist-get payload :home) :to-equal "/tmp/proj/")
        (expect (plist-get payload :node-id) :to-equal "ABC")
        (expect (plist-get payload :context) :to-equal 'fresh)))

    (it "carries :node-id nil (key present) for an identity-less workspace"
      (let ((payload (workspace--integration-payload
                      (workspace--make "proj" "/tmp/proj/") 'fresh)))
        (expect (plist-member payload :node-id) :not :to-be nil)
        (expect (plist-get payload :node-id) :to-be nil)))

    (it "has no :sessions-dir key — the package has no session concept"
      (let ((payload (workspace--integration-payload
                      (workspace--make "proj" "/tmp/proj/") 'fresh)))
        (expect (plist-member payload :sessions-dir) :to-be nil)))

    (it "carries only workspace self-knowledge keys"
      (let ((payload (workspace--integration-payload
                      (workspace--make "proj" "/tmp/proj/" "ABC") 'menu-invoke)))
        (expect (sort (cl-loop for (k _v) on payload by #'cddr collect k)
                      (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                :to-equal '(:context :home :name :node-id))))

    (it "reads :node-id from the record at call time, not from a cached snapshot"
      (let ((ws (workspace--make "proj" "/tmp/proj/")))
        (expect (plist-get (workspace--integration-payload ws 'fresh) :node-id)
                :to-be nil)
        (plist-put ws :node-id "LATE")
        (expect (plist-get (workspace--integration-payload ws 'fresh) :node-id)
                :to-equal "LATE")))

    (it "passes through each documented :context symbol"
      (let ((ws (workspace--make "p" "/tmp/p/")))
        (dolist (ctx '(fresh anchored-scaffolded anchored-existing menu-invoke purge))
          (expect (plist-get (workspace--integration-payload ws ctx) :context)
                  :to-equal ctx)))))

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
        (workspace--dispatch-create-integrations
         (workspace--make "proj" "/tmp/proj/") 'fresh)
        (expect (nreverse calls) :to-equal '(a b c))))

    (it "returns the result alist in registration order"
      (workspace-register-integration 'a :on-create (lambda (_p) 'ok))
      (workspace-register-integration 'b :on-create (lambda (_p) 'skipped))
      (expect (workspace--dispatch-create-integrations
         (workspace--make "p" "/tmp/p/") 'fresh)
              :to-equal '((a . ok) (b . skipped))))

    (it "skips entries lacking an :on-create handler"
      (workspace-register-integration 'menu-only :menu '(?m . cmd))
      (workspace-register-integration 'has-handler :on-create (lambda (_p) 'ok))
      (let ((results (workspace--dispatch-create-integrations
                      (workspace--make "p" "/tmp/p/") 'fresh)))
        (expect (mapcar #'car results) :to-equal '(has-handler))))

    (it "feeds the constructed anchor payload to handlers"
      (let ((seen nil))
        (workspace-register-integration
         'a :on-create (lambda (p) (setq seen p) 'ok))
        (workspace--dispatch-create-integrations
         (workspace--make "proj" "/tmp/proj/") 'fresh)
        (expect (plist-get seen :name) :to-equal "proj")
        (expect (plist-get seen :home) :to-equal "/tmp/proj/")
        (expect (plist-member seen :node-id) :not :to-be nil)
        (expect (plist-member seen :sessions-dir) :to-be nil)
        (expect (plist-get seen :context) :to-equal 'fresh)))

    (it "rebuilds the payload per handler: a :node-id written by an earlier handler is visible to later ones"
      ;; workspace-integrations spec, "Creation-time dispatch": scenario
      ;; "a node ID set by an earlier handler is visible to later ones".
      ;; This unit spec loads no persistence, so handler A performs the
      ;; same in-place mutation `workspace-set-node-id' does; the
      ;; end-to-end form with the real setter lives in
      ;; integration-dispatch-spec.el.
      (let ((ws (workspace--make "proj" "/tmp/proj/"))
            (seen-by-a 'unset)
            (seen-by-b 'unset))
        (workspace-register-integration
         'a :on-create (lambda (p)
                         (setq seen-by-a (plist-get p :node-id))
                         (plist-put ws :node-id "ABC")
                         'ok))
        (workspace-register-integration
         'b :on-create (lambda (p) (setq seen-by-b (plist-get p :node-id)) 'ok))
        (workspace--dispatch-create-integrations ws 'fresh)
        (expect seen-by-a :to-be nil)
        (expect seen-by-b :to-equal "ABC")))

    (it "purge dispatch carries the record's :node-id (no registry lookup)"
      ;; `workspace-purge' unregisters BEFORE dispatching teardown, so
      ;; the payload must come from the record it hands over.
      (let ((seen nil))
        (workspace-register-integration
         'a :on-purge (lambda (p) (setq seen p) 'ok))
        (workspace--dispatch-purge-integrations
         (workspace--make "proj" "/tmp/proj/" "ABC") 'purge)
        (expect (plist-get seen :node-id) :to-equal "ABC")
        (expect (plist-get seen :context) :to-equal 'purge)))

    (it "never signals when a handler errors, and surfaces it via message"
      (workspace-register-integration
       'boom :on-create (lambda (_p) (error "explode")))
      (spy-on 'message)
      (let (results)
        (expect (setq results (workspace--dispatch-create-integrations
                               (workspace--make "p" "/tmp/p/") 'fresh))
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
      (workspace--dispatch-create-integrations
         (workspace--make "p" "/tmp/p/") 'fresh)
      (expect 'message :to-have-been-called-with
              "workspace: integration %s failed: %s" 'f "declined"))

    (it "does not message for ok or skipped outcomes"
      (workspace-register-integration 'ok-one :on-create (lambda (_p) 'ok))
      (workspace-register-integration 'skip-one :on-create (lambda (_p) 'skipped))
      (spy-on 'message)
      (workspace--dispatch-create-integrations
         (workspace--make "p" "/tmp/p/") 'fresh)
      (expect 'message :not :to-have-been-called))

    (it "returns nil and never signals with an empty registry"
      (expect (workspace--dispatch-create-integrations
         (workspace--make "p" "/tmp/p/") 'fresh)
              :to-equal nil)))

  (describe "workspace--dispatch-purge-integrations"

    (it "runs every :on-purge handler once, in registration order"
      (let ((calls nil))
        (workspace-register-integration
         'a :on-purge (lambda (_p) (push 'a calls) 'ok))
        (workspace-register-integration
         'b :on-purge (lambda (_p) (push 'b calls) 'ok))
        (workspace-register-integration
         'c :on-purge (lambda (_p) (push 'c calls) 'ok))
        (workspace--dispatch-purge-integrations
         (workspace--make "proj" "/tmp/proj/") 'fresh)
        (expect (nreverse calls) :to-equal '(a b c))))

    (it "returns the result alist in registration order"
      (workspace-register-integration 'a :on-purge (lambda (_p) 'ok))
      (workspace-register-integration 'b :on-purge (lambda (_p) 'skipped))
      (expect (workspace--dispatch-purge-integrations
         (workspace--make "p" "/tmp/p/") 'fresh)
              :to-equal '((a . ok) (b . skipped))))

    (it "skips entries lacking an :on-purge handler"
      (workspace-register-integration 'create-only :on-create (lambda (_p) 'ok))
      (workspace-register-integration 'has-purge :on-purge (lambda (_p) 'ok))
      (let ((results (workspace--dispatch-purge-integrations
                      (workspace--make "p" "/tmp/p/") 'fresh)))
        (expect (mapcar #'car results) :to-equal '(has-purge))))

    (it "feeds the constructed anchor payload to handlers"
      (let ((seen nil))
        (workspace-register-integration
         'a :on-purge (lambda (p) (setq seen p) 'ok))
        (workspace--dispatch-purge-integrations
         (workspace--make "proj" "/tmp/proj/") 'fresh)
        (expect (plist-get seen :name) :to-equal "proj")
        (expect (plist-get seen :home) :to-equal "/tmp/proj/")
        (expect (plist-member seen :node-id) :not :to-be nil)
        (expect (plist-member seen :sessions-dir) :to-be nil)
        (expect (plist-get seen :context) :to-equal 'fresh)))

    (it "never signals when a handler errors, and surfaces it via message"
      (workspace-register-integration
       'boom :on-purge (lambda (_p) (error "explode")))
      (spy-on 'message)
      (let (results)
        (expect (setq results (workspace--dispatch-purge-integrations
                               (workspace--make "p" "/tmp/p/") 'fresh))
                :not :to-throw)
        (expect (caar results) :to-equal 'boom)
        (expect (cadar results) :to-equal 'failed)
        (expect (cddar results) :to-equal "explode")
        (expect 'message :to-have-been-called-with
                "workspace: integration %s purge-teardown failed: %s"
                'boom "explode")))

    (it "messages each (failed . reason) outcome the handler returns"
      (workspace-register-integration
       'f :on-purge (lambda (_p) '(failed . "declined")))
      (spy-on 'message)
      (workspace--dispatch-purge-integrations
         (workspace--make "p" "/tmp/p/") 'fresh)
      (expect 'message :to-have-been-called-with
              "workspace: integration %s purge-teardown failed: %s"
              'f "declined"))

    (it "does not message for ok or skipped outcomes"
      (workspace-register-integration 'ok-one :on-purge (lambda (_p) 'ok))
      (workspace-register-integration 'skip-one :on-purge (lambda (_p) 'skipped))
      (spy-on 'message)
      (workspace--dispatch-purge-integrations
         (workspace--make "p" "/tmp/p/") 'fresh)
      (expect 'message :not :to-have-been-called))

    (it "returns nil and never signals with an empty registry"
      (expect (workspace--dispatch-purge-integrations
         (workspace--make "p" "/tmp/p/") 'fresh)
              :to-equal nil))))

(provide 'integration-registry-spec)
;;; integration-registry-spec.el ends here
