;;; agent-tools.el --- gptel tools that add and remove workspace layouts -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'workspace-data-model)
(require 'workspace-layouts)

(declare-function workspace-for-directory "workspaces" (dir))
(declare-function workspace-get "workspaces" (name))
(declare-function gptel-make-tool "gptel")

(defun workspace-agent-tools--given (value)
  "Return VALUE, or nil when it is gptel's JSON null `:null'."
  (unless (eq value :null) value))

(defun workspace-agent-tools--string-arg (name value &optional required)
  "Return VALUE, the argument NAME, as a string, or nil when not given.
Signal a `user-error' naming NAME when VALUE is given but is not a
non-empty string, or when REQUIRED and it is not given."
  (let ((value (workspace-agent-tools--given value)))
    (cond
     ((null value)
      (when required
        (user-error "Argument %s is required" name))
      nil)
     ((and (stringp value) (not (string-empty-p value))) value)
     (t (user-error "Argument %s must be a non-empty string, not %S"
                    name value)))))

(defun workspace-agent-tools--sequence (where value)
  "Return VALUE, a JSON array at WHERE, as a list.
Signal a `user-error' naming WHERE when VALUE is not a non-empty array."
  (let ((value (workspace-agent-tools--given value)))
    (unless (and (or (vectorp value) (proper-list-p value))
                 (> (length value) 0))
      (user-error "%s must be a non-empty array of panes" where))
    (append value nil)))

(defun workspace-agent-tools--object (where value)
  "Return VALUE, a JSON object at WHERE, as an alist of (KEY . VALUE).
KEY is the keyword gptel used; a value of `:null' is dropped, as not
given.  Signal a `user-error' naming WHERE when VALUE is not an object."
  (unless (and (proper-list-p value)
               (cl-evenp (length value))
               (cl-loop for key in value by #'cddr always (keywordp key)))
    (user-error "%s must be an object, not %S" where value))
  (cl-loop for (key val) on value by #'cddr
           when (workspace-agent-tools--given val)
           collect (cons key val)))

(defun workspace-agent-tools--resolve-workspace (workspace directory)
  "Return the name of the workspace the tool arguments address.
WORKSPACE is a registered name and DIRECTORY an absolute directory
inside a workspace's home; exactly one must be given.  Signal a
`user-error' naming the problem otherwise.  Changes nothing."
  (let ((workspace (workspace-agent-tools--string-arg "workspace" workspace))
        (directory (workspace-agent-tools--string-arg "directory" directory)))
    (cond
     ((and workspace directory)
      (user-error "Give either workspace or directory, not both"))
     (workspace
      (unless (workspace-get workspace)
        (user-error "No workspace named \"%s\"" workspace))
      workspace)
     (directory
      (unless (file-name-absolute-p directory)
        (user-error "Directory \"%s\" is not absolute" directory))
      (or (workspace-for-directory directory)
          (user-error "No workspace home contains directory \"%s\""
                      directory)))
     (t
      (user-error "Give workspace (a workspace name) or directory \
\(a directory inside a workspace home)")))))

(defconst workspace-agent-tools--pane-keys '(:file :line :split :panes)
  "The keys a pane object may carry.")

(defun workspace-agent-tools--split (where value)
  "Return the split direction symbol for VALUE, the `split' at WHERE."
  (pcase (workspace-agent-tools--given value)
    ("columns" 'columns)
    ("rows" 'rows)
    (_ (user-error "%s must be \"columns\" or \"rows\", not %S"
                   where value))))

(defun workspace-agent-tools--file-pane (where file line base)
  "Return the recipe pane for FILE and LINE at WHERE, FILE relative to BASE."
  (unless (and (stringp file) (not (string-empty-p file)))
    (user-error "%s.file must be a non-empty string, not %S" where file))
  (when (and line (not (and (natnump line) (> line 0))))
    (user-error "%s.line must be a positive integer, not %S" where line))
  (let ((path (expand-file-name file base)))
    (unless (file-exists-p path)
      (user-error "File \"%s\" (%s) does not exist" file path))
    (when (file-directory-p path)
      (user-error "File \"%s\" (%s) is a directory, not a file" file path))
    (if line
        (list :file path :line line)
      (list :file path))))

(defun workspace-agent-tools--pane (where pane base)
  "Return the recipe pane for PANE, the pane object at WHERE."
  (let* ((fields (workspace-agent-tools--object where pane))
         (unknown (seq-remove
                   (lambda (key) (memq key workspace-agent-tools--pane-keys))
                   (mapcar #'car fields)))
         (has (lambda (key) (assq key fields)))
         (get (lambda (key) (cdr (assq key fields)))))
    (cond
     (unknown
      (user-error "%s has unknown key%s %s (a pane has file and \
optional line, or split and panes)"
                  where (if (cdr unknown) "s" "")
                  (mapconcat (lambda (k) (substring (symbol-name k) 1))
                             unknown ", ")))
     ((and (funcall has :file)
           (or (funcall has :split) (funcall has :panes)))
      (user-error "%s has both file and split/panes; a pane is \
one or the other" where))
     ((funcall has :file)
      (workspace-agent-tools--file-pane
       where (funcall get :file) (funcall get :line) base))
     ((funcall has :line)
      (user-error "%s has line but no file" where))
     ((and (funcall has :split) (funcall has :panes))
      (list :split (workspace-agent-tools--split
                    (concat where ".split") (funcall get :split))
            :panes (workspace-agent-tools--panes
                    (concat where ".panes") (funcall get :panes) base)))
     (t
      (user-error "%s needs file, or both split and panes" where)))))

(defun workspace-agent-tools--panes (where panes base)
  "Return the recipe panes for PANES, the pane array at WHERE."
  (seq-map-indexed
   (lambda (pane i)
     (workspace-agent-tools--pane (format "%s[%d]" where i) pane base))
   (workspace-agent-tools--sequence where panes)))

(defun workspace-agent-tools--recipe-from-args (base split panes)
  "Return the recipe for the tool arguments SPLIT and PANES.
SPLIT is \"columns\" or \"rows\"; PANES is gptel's decoding of the pane
array (a vector or list of plists), nested splits included.  A relative
file name is expanded against BASE.  Signal a `user-error' naming the
first bad argument or pane, or the first missing file.  Visits no file;
the result satisfies `workspace--recipe-p'."
  (list :recipe
        :split (workspace-agent-tools--split "split" split)
        :panes (workspace-agent-tools--panes "panes" panes base)))

(defun workspace-agent-tools--count-files (panes)
  "Return the number of file panes in the recipe PANES, at any depth."
  (seq-reduce (lambda (count pane)
                (+ count (if (plist-member pane :file)
                             1
                           (workspace-agent-tools--count-files
                            (plist-get pane :panes)))))
              panes 0))

(defun workspace-agent-tools--add-layout
    (workspace directory layout-name split panes &optional description source)
  "The function of the `workspace_add_layout' tool.
Store the recipe built from SPLIT and PANES as the agent layout
LAYOUT-NAME of the workspace WORKSPACE or DIRECTORY addresses, with
DESCRIPTION and SOURCE as its metadata.  Return a confirmation string.
Opens no file and touches no frame, tab, window or buffer."
  (let* ((ws-name (workspace-agent-tools--resolve-workspace workspace directory))
         (layout-name (workspace-agent-tools--string-arg
                       "layout_name" layout-name t))
         (description (workspace-agent-tools--string-arg
                       "description" description))
         (source (workspace-agent-tools--string-arg "source" source))
         (base (workspace--home (workspace-get ws-name)))
         (recipe (workspace-agent-tools--recipe-from-args base split panes))
         (existed (workspace--find-group (workspace-get ws-name) layout-name)))
    (workspace-add-layout ws-name layout-name recipe
                          :origin 'agent
                          :description description
                          :source source)
    (let ((files (workspace-agent-tools--count-files
                  (plist-get (cdr recipe) :panes))))
      (format "%s layout \"%s\" in workspace \"%s\" (%d file pane%s); \
the user sees it when they switch to it"
              (if existed "Replaced" "Added")
              layout-name ws-name files (if (= files 1) "" "s")))))

(defun workspace-agent-tools--remove-layout (workspace directory layout-name)
  "The function of the `workspace_remove_layout' tool.
Remove the agent layout LAYOUT-NAME of the workspace WORKSPACE or
DIRECTORY addresses.  Return a confirmation string."
  (let ((ws-name (workspace-agent-tools--resolve-workspace workspace directory))
        (layout-name (workspace-agent-tools--string-arg
                      "layout_name" layout-name t)))
    (workspace-remove-layout ws-name layout-name)
    (format "Removed layout \"%s\" from workspace \"%s\"" layout-name ws-name)))

(defconst workspace-agent-tools--pane-schema
  '(:type object
    :properties (:file (:type string
                        :description "File to show: absolute, or relative to the workspace home.")
                 :line (:type integer
                        :description "Optional 1-based line to show (file panes only).")
                 :split (:type string
                         :enum ["columns" "rows"]
                         :description "Direction of a nested split (with panes).")
                 :panes (:type array
                         :items (:type object)
                         :description "The nested split's panes, of this same shape.")))
  "JSON schema of one pane object, recursion described in prose.")

(defconst workspace-agent-tools--add-description
  "Add a layout to an Emacs workspace without showing it: the user sees \
it when they switch to it.  The layout is a split of file panes: SPLIT \
\"columns\" places PANES side by side, \"rows\" stacks them.  Each pane is \
either {\"file\": PATH, \"line\": N?} (an existing file, absolute or \
relative to the workspace home; LINE optional, 1-based) or a nested \
{\"split\": \"columns\"|\"rows\", \"panes\": [...]} of the same shape, to \
any depth.  Panes in one split share its space evenly.  Address the \
workspace by exactly one of WORKSPACE (its name) or DIRECTORY (a \
directory inside its home, e.g. a repository root).  An existing \
agent-made layout of that name is replaced unless the user is looking \
at it; a layout the user made is never touched, so pick another name if \
told it belongs to the user.  Convention for a layout per orchestrator \
ask: name it \"ask/<ask-id>\" and set SOURCE to the ask id, then remove \
it with workspace_remove_layout when the ask is closed."
  "Description of the `workspace_add_layout' tool.")

(defconst workspace-agent-tools--remove-description
  "Remove a layout this or another agent added to an Emacs workspace.  \
Address the workspace by exactly one of WORKSPACE (its name) or \
DIRECTORY (a directory inside its home).  A layout the user made, or one \
the user is looking at, is not removed; the error says which."
  "Description of the `workspace_remove_layout' tool.")

(defconst workspace-agent-tools--workspace-args
  '((:name "workspace"
     :type string
     :optional t
     :description "Name of the workspace.  Give this or directory, not both.")
    (:name "directory"
     :type string
     :optional t
     :description "Absolute directory inside the workspace's home; the deepest home containing it is used.  Give this or workspace, not both."))
  "The `workspace' and `directory' arguments both tools take first.")

(defun workspace-agent-tools--register ()
  "Register the `workspace_add_layout' and `workspace_remove_layout' tools.
Requires gptel; run from `with-eval-after-load'."
  (gptel-make-tool
   :name "workspace_add_layout"
   :function #'workspace-agent-tools--add-layout
   :description workspace-agent-tools--add-description
   :args (append
          workspace-agent-tools--workspace-args
          `((:name "layout_name"
             :type string
             :description "Name of the layout, e.g. \"ask/<ask-id>\".")
            (:name "split"
             :type string
             :enum ["columns" "rows"]
             :description "\"columns\" (side by side) or \"rows\" (stacked).")
            (:name "panes"
             :type array
             :items ,workspace-agent-tools--pane-schema
             :description "Non-empty array of panes: {file, line?} or {split, panes}, nested to any depth.")
            (:name "description"
             :type string
             :optional t
             :description "One line saying what the layout is for.")
            (:name "source"
             :type string
             :optional t
             :description "What the layout was made for, e.g. an ask id.")))
   :category "workspaces")
  (gptel-make-tool
   :name "workspace_remove_layout"
   :function #'workspace-agent-tools--remove-layout
   :description workspace-agent-tools--remove-description
   :args (append
          workspace-agent-tools--workspace-args
          '((:name "layout_name"
             :type string
             :description "Name of the layout to remove.")))
   :category "workspaces"))

(with-eval-after-load 'gptel
  (workspace-agent-tools--register))

(provide 'workspace-agent-tools)
;;; agent-tools.el ends here
