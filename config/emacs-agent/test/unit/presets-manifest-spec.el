;;; presets-manifest-spec.el --- Agent presets, registry access and the manifest -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md § Seams:
;;   seam/agent-presets     emacs-agent-read / emacs-agent-write; the tier
;;                          vocabulary's one mapping
;;   seam/registry-access   one lookup of gptel's registry; unique names
;;   seam/manifest          jf/agent-api, generated from presets + registry
;;
;; The test process has loaded the whole configuration (init.el), so the
;; real registry holds org-graph's, org-roam's and the workspaces tools:
;; the manifest asserted here is a full-configuration target's manifest,
;; which is what the live session serves.  Stub tools come from
;; `emacs-agent-test-with-tools'.  Pending until config/emacs-agent/
;; presets.org and manifest.org exist.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defconst presets-manifest-spec--read-tools
  '("org_graph_query" "org_graph_typed_edges"
    "search_roam_nodes" "list_roam_nodes" "read_roam_node"
    "get_roam_node_metadata" "query_roam_backlinks"
    "emacs_describe_symbol" "emacs_find_definition"
    "emacs_variable_value" "emacs_apropos")
  "The initial emacs-agent-read preset (control-surface-discoverability
§ Presets as Allowlists, with the introspection tools named by D26).")

(defconst presets-manifest-spec--write-tools
  '("org_graph_write_node" "workspace_add_layout" "workspace_remove_layout")
  "The initial emacs-agent-write preset.")

(defun presets-manifest-spec--manifest ()
  "The manifest as a list of plists."
  (append (json-parse-string (jf/agent-api) :object-type 'plist
                             :false-object nil :null-object nil)
          nil))

(defun presets-manifest-spec--entry (name)
  "The manifest entry for NAME, or nil."
  (seq-find (lambda (entry) (equal (plist-get entry :name) name))
            (presets-manifest-spec--manifest)))

(defun presets-manifest-spec--sorted (names)
  "NAMES sorted, for set comparison."
  (sort (copy-sequence names) #'string<))


(describe "The agent presets (seam/agent-presets)"

  (it "registers emacs-agent-read and emacs-agent-write with the initial tools (seam/agent-presets)"
    (expect (presets-manifest-spec--sorted
             (jf/agent-preset-tool-names 'emacs-agent-read))
            :to-equal (presets-manifest-spec--sorted presets-manifest-spec--read-tools))
    (expect (presets-manifest-spec--sorted
             (jf/agent-preset-tool-names 'emacs-agent-write))
            :to-equal (presets-manifest-spec--sorted presets-manifest-spec--write-tools)))

  (it "maps the tier call to the read preset and call-write to both, and nothing else (seam/agent-presets)"
    (expect (jf/agent--tier-presets "call") :to-equal '(emacs-agent-read))
    (expect (jf/agent--tier-presets "call-write")
            :to-equal '(emacs-agent-read emacs-agent-write))
    (dolist (other '("eval" "read" "write" ""))
      (expect (jf/agent--tier-presets other) :to-be nil)))

  (it "answers read, write or nil for a tool's manifest tier (seam/agent-presets)"
    (expect (jf/agent-tool-tier "org_graph_query") :to-equal "read")
    (expect (jf/agent-tool-tier "org_graph_write_node") :to-equal "write")
    (expect (jf/agent-tool-tier "create_file") :to-be nil)))


(describe "gptel's registry, read in one place (seam/registry-access)"

  (it "finds every preset tool under exactly one category of gptel's registry (seam/registry-access)"
    (dolist (name (append (jf/agent-preset-tool-names 'emacs-agent-read)
                          (jf/agent-preset-tool-names 'emacs-agent-write)))
      (expect (cons name (seq-count (lambda (category) (assoc name (cdr category)))
                                    gptel--known-tools))
              :to-equal (cons name 1))
      (expect (gptel-tool-name (jf/agent--registry-lookup name)) :to-equal name)))

  (it "answers nil for a name nothing registered, without signalling (seam/registry-access)"
    (expect (jf/agent--registry-lookup "emacs_agent_test_no_such_tool") :to-be nil)))


(describe "The manifest (seam/manifest)"

  (it "lists every preset tool of a full configuration with its tier and argument schema (Scenario: control-surface-discoverability § Agent discovers the callable tools)"
    (let ((manifest (presets-manifest-spec--manifest)))
      (expect (presets-manifest-spec--sorted
               (mapcar (lambda (entry) (plist-get entry :name)) manifest))
              :to-equal (presets-manifest-spec--sorted
                         (append presets-manifest-spec--read-tools
                                 presets-manifest-spec--write-tools)))
      (let ((query (presets-manifest-spec--entry "org_graph_query")))
        (expect (plist-get query :tier) :to-equal "read")
        (expect (plist-get query :category) :to-equal "org-graph")
        (expect (stringp (plist-get query :description)) :to-be t)
        (expect (append (plist-get query :args) nil)
                :to-equal
                (list (list :name "title_match" :type "string" :optional t
                            :description "Case-insensitive substring to match against note titles.")
                      (list :name "tag" :type "string" :optional t
                            :description "Exact filetag a note must carry (e.g. \"topic\")."))))
      (let* ((write (presets-manifest-spec--entry "org_graph_write_node"))
             (title (seq-find (lambda (arg) (equal (plist-get arg :name) "title"))
                              (plist-get write :args)))
             (tags (seq-find (lambda (arg) (equal (plist-get arg :name) "tags"))
                             (plist-get write :args))))
        (expect (plist-get write :tier) :to-equal "write")
        (expect (plist-get title :optional) :to-be nil)
        (expect (plist-get tags :type) :to-equal "array"))))

  (it "includes a tool added to emacs-agent-read on the next manifest, with tier read (Scenario: control-surface-discoverability § Manifest follows the presets)"
    (expect (presets-manifest-spec--entry "emacs_agent_test_added") :to-be nil)
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_added" :preset 'read
                    :args (list (emacs-agent-test-arg "q" t))))
      (let ((entry (presets-manifest-spec--entry "emacs_agent_test_added")))
        (expect (plist-get entry :tier) :to-equal "read")
        (expect (plist-get entry :category) :to-equal "emacs-agent-test")
        (expect (plist-get (aref (plist-get entry :args) 0) :name) :to-equal "q"))))

  (it "returns a multibyte string carrying a non-ASCII description as characters (seam/manifest)"
    ;; `json-parse-string' also accepts unibyte UTF-8, so the reply string
    ;; itself is tested: unibyte, emacsclient prints ‘ as \342\200\230.
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_non_ascii" :preset 'read
                    :description "Finds a café’s ‘notes’ →"))
      (let ((manifest (jf/agent-api)))
        (expect (multibyte-string-p manifest) :to-be t)
        (expect (string-search "Finds a café’s ‘notes’ →" manifest) :not :to-be nil)
        (expect (plist-get (presets-manifest-spec--entry "emacs_agent_test_non_ascii")
                           :description)
                :to-equal "Finds a café’s ‘notes’ →"))))

  (it "keeps a registered tool named in neither preset out of the manifest and the tiers (Scenario: control-surface-discoverability § Preset membership decides exposure)"
    (let ((ran nil))
      (emacs-agent-test-with-tools
          (list (list :name "emacs_agent_test_outsider" :preset nil
                      :function (lambda () (setq ran t) "ran")))
        (expect (gptel-get-tool "emacs_agent_test_outsider") :not :to-be nil)
        (expect (presets-manifest-spec--entry "emacs_agent_test_outsider") :to-be nil)
        (dolist (tier '("call" "call-write"))
          (expect (plist-get (emacs-agent-test-envelope
                              (jf/agent-call tier "emacs_agent_test_outsider" "{}"))
                             :ok)
                  :to-be nil)))
      (expect ran :to-be nil)))

  (it "omits a preset tool that is not registered and names it when invoked (Scenario: control-surface-discoverability § A preset tool missing from the registry)"
    (emacs-agent-test-with-tools
        (list (list :name "emacs_agent_test_unloaded" :preset 'read :register nil))
      (expect (member "emacs_agent_test_unloaded"
                      (jf/agent-preset-tool-names 'emacs-agent-read))
              :not :to-be nil)
      (expect (presets-manifest-spec--entry "emacs_agent_test_unloaded") :to-be nil)
      (let ((reply (emacs-agent-test-envelope
                    (jf/agent-call "call" "emacs_agent_test_unloaded" "{}"))))
        (expect (plist-get reply :ok) :to-be nil)
        (expect (plist-get reply :error) :to-match "emacs_agent_test_unloaded"))))

  (it "lists none of the control helpers, and no gptel tool runs one (Scenario: emacs-control-tools § Helpers absent from the manifest)"
    (let ((helpers '(jf/type-text jf/send-keys jf/run-command jf/frame-state)))
      (dolist (entry (presets-manifest-spec--manifest))
        (let ((tool (jf/agent--registry-lookup (plist-get entry :name))))
          (expect (memq (gptel-tool-function tool) helpers) :to-be nil)))
      (dolist (category gptel--known-tools)
        (dolist (named (cdr category))
          (expect (cons (car named) (memq (gptel-tool-function (cdr named)) helpers))
                  :to-equal (list (car named))))))))

;;; presets-manifest-spec.el ends here
