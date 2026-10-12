;;; agent-tools-spec.el --- The workspace_add_layout / workspace_remove_layout gptel tools -*- lexical-binding: t; -*-

;; Change workspaces-programmatic-layouts, design.md D7 and § Seams:
;;   seam/agent-tools          registration and the two tool functions
;;   seam/recipe-shape         the recipe the add tool stores
;;   seam/directory-ownership  the `directory' argument
;;   seam/write-protection     refusals reach the agent unchanged
;;
;; Each spec cites the scenario of specs/workspaces/spec.md it pins
;; (`Scenario: workspaces § <title>').  The tools are driven the way gptel
;; drives them (`agent-tools-spec--call'): found in gptel's registry by
;; name, their arguments laid out in the order of the tool's `:args', JSON
;; arrays as vectors and objects as plists, and an error signalled by the
;; function turned into the text the model receives.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'tab-bar)

(defconst agent-tools-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(defconst agent-tools-spec--root
  (locate-dominating-file agent-tools-spec--this-dir "early-init.el")
  "Root of the configuration checkout.")

(let ((dir agent-tools-spec--this-dir))
  ;; gptel and its compat dependency, for a run outside the harness (which
  ;; has loaded gptel already through init.el).
  (when agent-tools-spec--root
    (dolist (pkg '("compat" "gptel"))
      (let ((d (expand-file-name (format "runtime/straight/build/%s" pkg)
                                 agent-tools-spec--root)))
        (when (file-directory-p d) (add-to-list 'load-path d)))))
  (require 'gptel nil t)
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir))
  (load (expand-file-name "../workspaces.el"        dir))
  (load (expand-file-name "portable-bookmarks-fixtures.el" dir))
  (load (expand-file-name "../agent-tools.el" dir)))

(defvar agent-tools-spec--saved-state-override nil
  "`workspace-state-directory-override' as it was before the spec.")

(defun agent-tools-spec--setup ()
  "Isolate the registry, the bookmarks and the state file."
  (workspace-bookmark-fixtures-setup-with-empty-registry)
  (setq agent-tools-spec--saved-state-override workspace-state-directory-override
        workspace-state-directory-override
        (workspace-bookmark-fixtures-temp-dir "wsat-state-")))

(defun agent-tools-spec--teardown ()
  "Close every tab but the first, then undo `agent-tools-spec--setup'."
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (tab-bar-select-tab 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq workspace-state-directory-override agent-tools-spec--saved-state-override)
  (workspace-bookmark-fixtures-teardown))

(defun agent-tools-spec--call (tool-name &rest arguments)
  "Call the gptel tool TOOL-NAME with ARGUMENTS as gptel's tool-use handler does.
ARGUMENTS is a plist keyed by the tool's argument names (`:layout_name').
Return (:ok RESULT) when the function returns, or (:error MESSAGE) when
it signals, MESSAGE being what the model would read."
  (let* ((tool (gptel-get-tool tool-name))
         (values (mapcar (lambda (arg)
                           (plist-get arguments
                                      (intern (concat ":" (plist-get arg :name)))))
                         (gptel-tool-args tool))))
    (condition-case err
        (list :ok (apply (gptel-tool-function tool) values))
      (error (list :error (error-message-string err))))))

(defun agent-tools-spec--lines (count)
  "COUNT numbered lines of text."
  (mapconcat (lambda (i) (format "line %d\n" i)) (number-sequence 1 count) ""))

(defun agent-tools-spec--layout (ws-name layout-name)
  "The layout LAYOUT-NAME of WS-NAME in the registry, or nil."
  (let* ((ws (gethash ws-name workspace--registry))
         (group (and ws (workspace--find-group ws layout-name))))
    (and group (workspace--group-recent-layout group))))

(defun agent-tools-spec--child-report ()
  "Load the workspaces parts, agent-tools.el and then gptel in a child Emacs.
The child is `emacs -Q --batch' with only the straight build directories
of gptel and compat on its load path, so gptel is loaded strictly after
the workspaces package.  Return (:exit CODE :output TEXT)."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (build (expand-file-name "runtime/straight/build/" agent-tools-spec--root))
         (module-dir (expand-file-name "../" agent-tools-spec--this-dir))
         (parts '("data-model" "integrations" "home-org" "scaffold" "tabs"
                  "buffer-membership" "layouts" "persistence" "agent-tools"))
         (args (append
                (list "-Q" "--batch"
                      "-L" (expand-file-name "compat" build)
                      "-L" (expand-file-name "gptel" build)
                      "--eval" "(unless (featurep 'bufferlo) (defalias 'bufferlo-mode #'ignore) (provide 'bufferlo))")
                (mapcan (lambda (part)
                          (list "-l" (expand-file-name (concat part ".el") module-dir)))
                        parts)
                (list "--eval"
                      (concat "(progn (setq kill-emacs-hook nil)"
                              " (princ (format \"gptel-before=%S\\n\" (featurep 'gptel)))"
                              " (require 'gptel)"
                              " (dolist (name '(\"workspace_add_layout\" \"workspace_remove_layout\"))"
                              "   (princ (format \"%s=%S\\n\" name"
                              "     (condition-case nil (and (gptel-get-tool name) t) (error nil))))))")))))
    (with-temp-buffer
      (let ((exit (apply #'call-process emacs nil t nil args)))
        (list :exit exit :output (buffer-string))))))

(describe "workspace_add_layout (seam/agent-tools)"
  (before-each (agent-tools-spec--setup))
  (after-each (agent-tools-spec--teardown))

  (it "stores an agent recipe with absolute paths and visits neither file (Scenario: workspaces § Agent adds a demonstration layout)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "code"))
           (a (workspace-bookmark-fixtures-write-file
               home "config/a.org" (agent-tools-spec--lines 60)))
           (design (workspace-bookmark-fixtures-write-file home "design.md"))
           (buffers (buffer-list))
           (result (agent-tools-spec--call
                    "workspace_add_layout"
                    :workspace "code" :layout_name "demo" :split "columns"
                    :panes (vector (list :file "config/a.org" :line 40)
                                   (list :file "design.md"))
                    :description "Demo" :source "change-x")))
      (expect (car result) :to-be :ok)
      (let ((demo (agent-tools-spec--layout "code" "demo")))
        (expect (workspace--layout-origin demo) :to-be 'agent)
        (expect (workspace--layout-description demo) :to-equal "Demo")
        (expect (workspace--layout-source demo) :to-equal "change-x")
        (expect (workspace--layout-working-state demo) :to-be nil)
        (expect (workspace--layout-saved-state demo)
                :to-equal `(:recipe :split columns
                                    :panes ((:file ,a :line 40) (:file ,design)))))
      (expect (workspace-bookmark-fixtures-visits-p a) :to-be nil)
      (expect (workspace-bookmark-fixtures-visits-p design) :to-be nil)
      (expect (seq-difference (buffer-list) buffers) :to-equal nil)))

  (it "stores a nested split with the same nesting (Scenario: workspaces § Agent adds a nested layout)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "code"))
           (ask (workspace-bookmark-fixtures-write-file home "ask.org"))
           (scaffold (workspace-bookmark-fixtures-write-file
                      home "scaffold.org" (agent-tools-spec--lines 200)))
           (design (workspace-bookmark-fixtures-write-file home "design.md"))
           (result (agent-tools-spec--call
                    "workspace_add_layout"
                    :workspace "code" :layout_name "ask/a1" :split "columns"
                    :panes (vector (list :file "ask.org")
                                   (list :split "rows"
                                         :panes (vector (list :file "scaffold.org" :line 157)
                                                        (list :file "design.md")))))))
      (expect (car result) :to-be :ok)
      (expect (workspace--layout-saved-state (agent-tools-spec--layout "code" "ask/a1"))
              :to-equal `(:recipe :split columns
                                  :panes ((:file ,ask)
                                          (:split rows
                                                  :panes ((:file ,scaffold :line 157)
                                                          (:file ,design))))))))

  (it "adds to the workspace whose home owns the given directory (Scenario: workspaces § Agent addresses the workspace by directory)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "emacs-mcp"))
           (repo (file-name-as-directory (expand-file-name "emacs" home)))
           (file (workspace-bookmark-fixtures-write-file repo "init.org")))
      (workspace-bookmark-fixtures-make-workspace "other")
      (let ((result (agent-tools-spec--call
                     "workspace_add_layout"
                     :directory repo :layout_name "demo" :split "rows"
                     :panes (vector (list :file file)))))
        (expect (car result) :to-be :ok)
        (expect (agent-tools-spec--layout "emacs-mcp" "demo") :not :to-be nil)
        (expect (agent-tools-spec--layout "other" "demo") :to-be nil))))

  (it "expands a relative pane path against the given directory (Scenario: workspaces § Relative paths count from the given directory)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "emacs-mcp"))
           (repo (expand-file-name "emacs" home))
           (a (workspace-bookmark-fixtures-write-file repo "config/a.org")))
      ;; The same relative name under the home too, so a home-based
      ;; expansion would succeed and store the wrong file.
      (workspace-bookmark-fixtures-write-file home "config/a.org")
      (let ((result (agent-tools-spec--call
                     "workspace_add_layout"
                     :directory (directory-file-name repo) :layout_name "demo"
                     :split "columns"
                     :panes (vector (list :file "config/a.org")))))
        (expect (car result) :to-be :ok)
        (expect (workspace--layout-saved-state
                 (agent-tools-spec--layout "emacs-mcp" "demo"))
                :to-equal `(:recipe :split columns :panes ((:file ,a)))))))

  (it "refuses a directory that names a file and adds nothing (seam/agent-tools)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "code"))
           (file (workspace-bookmark-fixtures-write-file home "emacs/file.org"))
           (entry nil))
      (workspace-bookmark-fixtures-write-file home "emacs/config/a.org")
      (setq entry (copy-tree (gethash "code" workspace--registry)))
      (let ((result (agent-tools-spec--call
                     "workspace_add_layout"
                     :directory file :layout_name "demo" :split "columns"
                     :panes (vector (list :file "config/a.org")))))
        (expect (car result) :to-be :error)
        (expect (cadr result)
                :to-equal (format "Directory \"%s\" is not a directory" file))
        (expect (agent-tools-spec--layout "code" "demo") :to-be nil)
        (expect (gethash "code" workspace--registry) :to-equal entry))))

  (it "reports a missing file by name and adds nothing (Scenario: workspaces § Invalid file is reported)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "code"))
           (entry nil))
      (workspace-bookmark-fixtures-write-file home "design.md")
      (setq entry (copy-tree (gethash "code" workspace--registry)))
      (let ((result (agent-tools-spec--call
                     "workspace_add_layout"
                     :workspace "code" :layout_name "demo" :split "columns"
                     :panes (vector (list :file "nope.org")
                                    (list :file "design.md")))))
        (expect (car result) :to-be :error)
        (expect (cadr result) :to-match "nope\\.org")
        (expect (agent-tools-spec--layout "code" "demo") :to-be nil)
        (expect (gethash "code" workspace--registry) :to-equal entry))))

  (it "refuses a remote pane file by name without contacting it, and adds nothing (seam/agent-tools)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "code"))
           (entry nil))
      (workspace-bookmark-fixtures-write-file home "design.md")
      (setq entry (copy-tree (gethash "code" workspace--registry)))
      (let ((result (workspaces-test-with-fake-remote
                        ("/wsx-remote:" :allow (expand-file-name))
                      (agent-tools-spec--call
                       "workspace_add_layout"
                       :workspace "code" :layout_name "demo" :split "columns"
                       :panes (vector (list :file "/wsx-remote:host:/etc/hosts")
                                      (list :file "design.md"))))))
        (expect (car result) :to-be :error)
        (expect (cadr result) :to-match "is remote")
        (expect workspaces-test-fake-remote-touches :to-be nil)
        (expect (agent-tools-spec--layout "code" "demo") :to-be nil)
        (expect (gethash "code" workspace--registry) :to-equal entry))))

  (it "treats a JSON null as not given and re-emits pane keys in canonical order (seam/agent-tools)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "code"))
           (a (workspace-bookmark-fixtures-write-file
               home "a.org" (agent-tools-spec--lines 10)))
           (result (agent-tools-spec--call
                    "workspace_add_layout"
                    :workspace "code" :directory :null :layout_name "demo"
                    :split "columns"
                    :panes (vector (list :line 3 :file "a.org"))
                    :description :null :source :null)))
      (expect (car result) :to-be :ok)
      (let ((demo (agent-tools-spec--layout "code" "demo")))
        (expect (workspace--layout-description demo) :to-be nil)
        (expect (workspace--layout-source demo) :to-be nil)
        (expect (workspace--layout-saved-state demo)
                :to-equal `(:recipe :split columns :panes ((:file ,a :line 3)))))))

  (it "refuses a pane with an unknown key and adds nothing (seam/agent-tools)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "code"))
           (entry nil))
      (workspace-bookmark-fixtures-write-file home "a.org")
      (setq entry (copy-tree (gethash "code" workspace--registry)))
      (let ((result (agent-tools-spec--call
                     "workspace_add_layout"
                     :workspace "code" :layout_name "demo" :split "columns"
                     :panes (vector (list :file "a.org" :colour "red")))))
        (expect (car result) :to-be :error)
        (expect (agent-tools-spec--layout "code" "demo") :to-be nil)
        (expect (gethash "code" workspace--registry) :to-equal entry))))

  (it "reports that a layout belongs to the user and leaves it as it was (Scenario: workspaces § Refusal is reported to the agent)"
    (let ((home (workspace-bookmark-fixtures-make-workspace "code")))
      (workspace-bookmark-fixtures-write-file home "design.md")
      ;; A layout with no metadata is the user's (seam/layout-metadata).
      (puthash "code"
               (workspace--upsert-group (gethash "code" workspace--registry)
                                        "review" (workspace--layout-make 'user-state))
               workspace--registry)
      (let* ((before (copy-tree (agent-tools-spec--layout "code" "review")))
             (result (agent-tools-spec--call
                      "workspace_add_layout"
                      :workspace "code" :layout_name "review" :split "columns"
                      :panes (vector (list :file "design.md")))))
        (expect (car result) :to-be :error)
        (expect (cadr result) :to-match "review")
        (expect (cadr result) :to-match "belongs to the user")
        (expect (agent-tools-spec--layout "code" "review") :to-equal before)))))

(describe "workspace_remove_layout (seam/agent-tools)"
  (before-each (agent-tools-spec--setup))
  (after-each (agent-tools-spec--teardown))

  (it "removes an agent layout that is not on screen and reports success (Scenario: workspaces § Agent removes its ask layout)"
    (let ((home (workspace-bookmark-fixtures-make-workspace "code")))
      (workspace-bookmark-fixtures-write-file home "ask.org")
      (expect (car (agent-tools-spec--call
                    "workspace_add_layout"
                    :workspace "code" :layout_name "ask/a1" :split "columns"
                    :panes (vector (list :file "ask.org"))
                    :source "a1"))
              :to-be :ok)
      (let ((result (agent-tools-spec--call
                     "workspace_remove_layout"
                     :workspace "code" :layout_name "ask/a1")))
        (expect (car result) :to-be :ok)
        (expect (cadr result) :to-match "Removed layout \"ask/a1\"")
        (expect (agent-tools-spec--layout "code" "ask/a1") :to-be nil))))

  (it "reports success when removing a layout the workspace does not have (Scenario: workspaces § Removing a layout that was never made reports success)"
    (let ((home (workspace-bookmark-fixtures-make-workspace "code"))
          (entry nil))
      (workspace-bookmark-fixtures-write-file home "ask.org")
      (expect (car (agent-tools-spec--call
                    "workspace_add_layout"
                    :workspace "code" :layout_name "ask/a1" :split "columns"
                    :panes (vector (list :file "ask.org"))))
              :to-be :ok)
      (setq entry (copy-tree (gethash "code" workspace--registry)))
      (spy-on 'workspace--flush-state :and-call-through)
      (let ((result (agent-tools-spec--call
                     "workspace_remove_layout"
                     :directory home :layout_name "ask/a2")))
        (expect (car result) :to-be :ok)
        (expect (cadr result)
                :to-equal "No layout \"ask/a2\" in workspace \"code\"; nothing to remove")
        (expect (gethash "code" workspace--registry) :to-equal entry)
        (expect 'workspace--flush-state :not :to-have-been-called)))))

(describe "Registering the tools (seam/agent-tools)"

  (it "registers both tools when gptel is loaded after the workspaces package (Scenario: workspaces § Tools are registered when gptel loads after workspaces)"
    (let ((report (agent-tools-spec--child-report)))
      (expect (plist-get report :exit) :to-equal 0)
      (expect (plist-get report :output) :to-match "^gptel-before=nil$")
      (expect (plist-get report :output) :to-match "^workspace_add_layout=t$")
      (expect (plist-get report :output) :to-match "^workspace_remove_layout=t$")))

  (it "registers both tools in the `workspaces' category when gptel was loaded first (seam/agent-tools)"
    ;; The harness loads gptel through init.el; agent-tools.el was loaded
    ;; after it, above.
    (dolist (name '("workspace_add_layout" "workspace_remove_layout"))
      (let ((tool (gptel-get-tool name)))
        (expect (gptel-tool-p tool) :to-be-truthy)
        (expect (gptel-tool-category tool) :to-equal "workspaces")))))

;;; agent-tools-spec.el ends here
