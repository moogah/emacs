;;; dispatcher-spec.el --- jf/agent-call: tiers, argument mapping, envelope -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md § Seams:
;;   seam/dispatcher-args    JSON arguments → positional values exactly as
;;                           `gptel--handle-tool-use' maps them; the envelope
;;   seam/tier-enforcement   every refusal happens before anything runs
;;   seam/async-protocol     asynchronous tools (D24: refused until the
;;                           request + poll protocol lands)
;;
;; Each spec cites the scenario it pins (`Scenario: <capability> § <title>')
;; or the seam whose clause it asserts.  The tools are stub `gptel-tool's
;; registered with `gptel-make-tool' and placed in the agent presets by
;; `emacs-agent-test-with-tools', which binds gptel's registry and preset
;; table to copies.  The request-id spec stays pending while D24 defers
;; the asynchronous protocol.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defvar dispatcher-spec--calls nil
  "Argument lists the recording stub tools received, newest first.")

(defun dispatcher-spec--recorder (&rest args)
  "Stub tool function: record ARGS and return a fixed string."
  (push args dispatcher-spec--calls)
  "recorded")

(defun dispatcher-spec--call (tier name json)
  "Dispatch NAME on TIER with JSON and return the parsed envelope."
  (emacs-agent-test-envelope (jf/agent-call tier name json)))

(defun dispatcher-spec--tool (name preset &rest props)
  "A stub tool spec NAME in PRESET recording its calls, with PROPS."
  (append (list :name name :preset preset :function #'dispatcher-spec--recorder)
          props))


(describe "jf/agent-call: arguments reach the tool as gptel passes them (seam/dispatcher-args)"
  (before-each (setq dispatcher-spec--calls nil))

  (it "passes {\"c\":3,\"a\":1} to a tool declaring (a b c) as 1, nil, 3 (Scenario: control-surface-discoverability § Arguments are mapped positionally by name)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool
               "emacs_agent_test_abc" 'read
               :args (list (emacs-agent-test-arg "a" nil 'integer)
                           (emacs-agent-test-arg "b" t 'integer)
                           (emacs-agent-test-arg "c" t 'integer))))
      (let ((reply (dispatcher-spec--call "call" "emacs_agent_test_abc"
                                          "{\"c\":3,\"a\":1}")))
        (expect (plist-get reply :ok) :to-be t)
        (expect (plist-get reply :result) :to-equal "recorded")
        (expect dispatcher-spec--calls :to-equal '((1 nil 3))))))

  (it "decodes arrays as vectors, objects as plists in sender order, false as :json-false and null as nil (seam/dispatcher-args)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool
               "emacs_agent_test_shapes" 'read
               :args (list (emacs-agent-test-arg "items" nil 'array)
                           (emacs-agent-test-arg "obj" nil 'object)
                           (emacs-agent-test-arg "flag" nil 'boolean)
                           (emacs-agent-test-arg "gone" t))))
      (dispatcher-spec--call
       "call" "emacs_agent_test_shapes"
       "{\"items\":[\"x\",\"y\"],\"obj\":{\"z\":1,\"a\":2},\"flag\":false,\"gone\":null}")
      (expect dispatcher-spec--calls
              :to-equal '((["x" "y"] (:z 1 :a 2) :json-false nil)))))

  (it "ignores keys the tool does not declare, as gptel does (seam/dispatcher-args)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool "emacs_agent_test_one" 'read
                                     :args (list (emacs-agent-test-arg "a"))))
      (expect (plist-get (dispatcher-spec--call "call" "emacs_agent_test_one"
                                                "{\"a\":\"x\",\"extra\":1}")
                         :ok)
              :to-be t)
      (expect dispatcher-spec--calls :to-equal '(("x")))))

  (it "turns a non-string result into text as gptel--to-string does (seam/dispatcher-args)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_plist" :preset 'read
                    :function (lambda () '(:id "x" :n 1)))
              (list :name "emacs_agent_test_symbol" :preset 'read
                    :function (lambda () 'done)))
      (expect (plist-get (dispatcher-spec--call "call" "emacs_agent_test_plist" "{}")
                         :result)
              :to-equal "(:id \"x\" :n 1)")
      (expect (plist-get (dispatcher-spec--call "call" "emacs_agent_test_symbol" "{}")
                         :result)
              :to-equal "done")))

  (it "answers malformed or non-object JSON with a failure envelope and runs nothing (seam/dispatcher-args)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool "emacs_agent_test_one" 'read
                                     :args (list (emacs-agent-test-arg "a" t))))
      (dolist (json '("{\"a\":" "[1,2]" "\"a\""))
        (let ((reply (dispatcher-spec--call "call" "emacs_agent_test_one" json)))
          (expect (plist-get reply :ok) :to-be nil)
          (expect (plist-get reply :error) :to-match "JSON")))
      (expect dispatcher-spec--calls :to-be nil))))


(describe "jf/agent-call: a failing tool is contained (seam/dispatcher-args)"
  (before-each (setq dispatcher-spec--calls nil))

  (it "returns a failure envelope for a signalling tool and serves the next call (Scenario: control-surface-discoverability § Tool error is contained)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_fails" :preset 'read
                    :args (list (emacs-agent-test-arg "what"))
                    :function (lambda (what) (user-error "Nope: %s" what)))
              (dispatcher-spec--tool "emacs_agent_test_ok" 'read))
      (let ((reply (dispatcher-spec--call "call" "emacs_agent_test_fails"
                                          "{\"what\":\"x\"}")))
        (expect (plist-get reply :ok) :to-be nil)
        ;; The text gptel gives the model for the same error.
        (expect (plist-get reply :error) :to-equal "user-error Nope: x"))
      (let ((next (dispatcher-spec--call "call" "emacs_agent_test_ok" "{}")))
        (expect (plist-get next :ok) :to-be t)
        (expect (plist-get next :result) :to-equal "recorded")))))


(describe "jf/agent-call: the reply's encoding (seam/dispatcher-args)"

  ;; `json-parse-string' also accepts unibyte UTF-8, so these specs test
  ;; the reply string itself: unibyte, emacsclient prints non-ASCII as
  ;; octal escapes (\342\200\230 for ‘).

  (it "answers a non-ASCII result with a multibyte reply carrying the characters (seam/dispatcher-args)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_non_ascii" :preset 'read
                    :function (lambda () "é ‘quoted’ →")))
      (let ((reply (jf/agent-call "call" "emacs_agent_test_non_ascii" "{}")))
        (expect (multibyte-string-p reply) :to-be t)
        (expect (string-search "é ‘quoted’ →" reply) :not :to-be nil)
        (expect (plist-get (emacs-agent-test-envelope reply) :result)
                :to-equal "é ‘quoted’ →"))))

  (it "answers a non-ASCII error with a multibyte reply carrying the characters (seam/dispatcher-args)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_non_ascii_error" :preset 'read
                    :function (lambda () (user-error "Café ‘missing’"))))
      (let ((reply (jf/agent-call "call" "emacs_agent_test_non_ascii_error" "{}")))
        (expect (multibyte-string-p reply) :to-be t)
        (expect (plist-get (emacs-agent-test-envelope reply) :error)
                :to-equal "user-error Café ‘missing’"))))

  (it "answers the fixed failure envelope for a result or error text JSON cannot carry, and keeps serving (seam/dispatcher-args)"
    (let ((fixed "{\"ok\":false,\"error\":\"The reply is not valid Unicode text and cannot be sent as JSON\"}")
          (texts (list (unibyte-string #xff #xfe)                     ; raw bytes, unibyte
                       (string ?a (unibyte-char-to-multibyte #xff))   ; a raw byte, multibyte
                       (string ?a #x3fff00))))                        ; not a Unicode character
      (emacs-agent-test-with-tools
          (list (list :name "emacs_agent_test_bytes" :preset 'read
                      :args (list (emacs-agent-test-arg "n" nil 'integer))
                      :function (lambda (n) (nth n texts)))
                (list :name "emacs_agent_test_bytes_error" :preset 'read
                      :function (lambda () (user-error "Bad %s" (car texts))))
                (dispatcher-spec--tool "emacs_agent_test_ok" 'read))
        (dotimes (n (length texts))
          (expect (cons n (jf/agent-call "call" "emacs_agent_test_bytes"
                                         (format "{\"n\":%d}" n)))
                  :to-equal (cons n fixed)))
        (expect (jf/agent-call "call" "emacs_agent_test_bytes_error" "{}")
                :to-equal fixed)
        (expect (plist-get (dispatcher-spec--call "call" "emacs_agent_test_ok" "{}") :ok)
                :to-be t)))))


(describe "jf/agent-call: tiers (seam/tier-enforcement)"
  (before-each (setq dispatcher-spec--calls nil))

  (it "refuses a write-preset tool on the call tier, naming call-write, and does not run it (Scenario: agent-target-policy § Read tier refuses a write tool)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool "emacs_agent_test_writer" 'write))
      (let ((reply (dispatcher-spec--call "call" "emacs_agent_test_writer" "{}")))
        (expect (plist-get reply :ok) :to-be nil)
        (expect (plist-get reply :error) :to-match "call-write"))
      (expect dispatcher-spec--calls :to-be nil)))

  (it "runs a write-preset tool on call-write and a read-preset tool on both tiers (seam/tier-enforcement)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool "emacs_agent_test_writer" 'write)
              (dispatcher-spec--tool "emacs_agent_test_reader" 'read))
      (expect (plist-get (dispatcher-spec--call "call-write" "emacs_agent_test_writer" "{}")
                         :ok)
              :to-be t)
      (expect (plist-get (dispatcher-spec--call "call" "emacs_agent_test_reader" "{}")
                         :ok)
              :to-be t)
      (expect (plist-get (dispatcher-spec--call "call-write" "emacs_agent_test_reader" "{}")
                         :ok)
              :to-be t)
      (expect (length dispatcher-spec--calls) :to-equal 3)))

  (it "refuses a registered tool in neither preset on both tiers and does not run it (Scenario: agent-target-policy § Tool outside both presets is refused)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool "emacs_agent_test_outsider" nil))
      (dolist (tier '("call" "call-write"))
        (let ((reply (dispatcher-spec--call tier "emacs_agent_test_outsider" "{}")))
          (expect (plist-get reply :ok) :to-be nil)
          (expect (plist-get reply :error) :to-match "emacs_agent_test_outsider")))
      (expect dispatcher-spec--calls :to-be nil)))

  (it "refuses a tier other than call and call-write, naming both, and runs nothing (seam/tier-enforcement)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool "emacs_agent_test_reader" 'read))
      (dolist (tier '("eval" "read" "" "CALL"))
        (let ((reply (dispatcher-spec--call tier "emacs_agent_test_reader" "{}")))
          (expect (plist-get reply :ok) :to-be nil)
          (expect (plist-get reply :error) :to-match "call-write")))
      (expect dispatcher-spec--calls :to-be nil)))

  (it "invokes a tool a module registered with gptel-make-tool once a preset names it, with no other registration (Scenario: control-surface-discoverability § A module's gptel tool is callable by an external agent)"
    ;; The stub is registered exactly as org-graph registers its tools:
    ;; one `gptel-make-tool' in the module's own category, and a preset
    ;; entry; nothing else names it.
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool "emacs_agent_test_module_tool" 'read
                                     :args (list (emacs-agent-test-arg "q"))))
      (let ((reply (dispatcher-spec--call "call" "emacs_agent_test_module_tool"
                                          "{\"q\":\"graph\"}")))
        (expect (plist-get reply :ok) :to-be t)
        (expect dispatcher-spec--calls :to-equal '(("graph")))))))


(describe "jf/agent-call: asynchronous tools (seam/async-protocol)"
  (before-each (setq dispatcher-spec--calls nil))

  (it "refuses an asynchronous tool with \"asynchronous tools are not supported\", without calling it, at once (Scenario: agent-target-policy § Asynchronous tool before the protocol exists)"
    (emacs-agent-test-with-tools
        (list (dispatcher-spec--tool "emacs_agent_test_async" 'read :async t))
      (let* ((start (float-time))
             (reply (dispatcher-spec--call "call" "emacs_agent_test_async" "{}")))
        (expect (plist-get reply :ok) :to-be nil)
        (expect (plist-get reply :error) :to-match "asynchronous tools are not supported")
        (expect (< (- (float-time) start) 1.0) :to-be t))
      (expect dispatcher-spec--calls :to-be nil)))

  ;; Stays pending while D24 defers the protocol.
  (xit "returns a request id for an asynchronous tool and its result on a later query (Scenario: agent-target-policy § Asynchronous tool returns a request id)"
    (let (callback)
      (emacs-agent-test-with-tools
          (list (list :name "emacs_agent_test_async" :preset 'read :async t
                      :function (lambda (cb &rest _) (setq callback cb))))
        (let* ((reply (dispatcher-spec--call "call" "emacs_agent_test_async" "{}"))
               (id (plist-get reply :id)))
          (expect (plist-get reply :pending) :to-be t)
          (expect id :not :to-be nil)
          (expect (plist-get (emacs-agent-test-envelope (jf/agent-result id)) :pending)
                  :to-be t)
          (funcall callback "done")
          (let ((result (emacs-agent-test-envelope (jf/agent-result id))))
            (expect (plist-get result :ok) :to-be t)
            (expect (plist-get result :result) :to-equal "done")))))))


(describe "jf/agent-eval: the eval tier's entry on the live session (seam/dispatcher-args)"

  (it "evaluates a form string and returns its printed value in an envelope (seam/dispatcher-args)"
    (let ((reply (emacs-agent-test-envelope (jf/agent-eval "(list 1 \"two\" 'three)"))))
      (expect (plist-get reply :ok) :to-be t)
      (expect (plist-get reply :result) :to-equal "(1 \"two\" three)")))

  (it "returns a failure envelope for a signalling form and keeps serving (seam/dispatcher-args)"
    (let ((reply (emacs-agent-test-envelope (jf/agent-eval "(error \"emacs-agent boom\")"))))
      (expect (plist-get reply :ok) :to-be nil)
      (expect (plist-get reply :error) :to-match "emacs-agent boom"))
    (expect (plist-get (emacs-agent-test-envelope (jf/agent-eval "(+ 1 2)")) :result)
            :to-equal "3")))

;;; dispatcher-spec.el ends here
