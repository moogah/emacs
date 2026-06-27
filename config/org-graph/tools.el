;;; tools.el --- org-graph gptel agent tool surface -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'vulpea)

(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (require 'org-graph-query (expand-file-name "query.el" dir))
  (require 'org-graph-coordinator (expand-file-name "coordinator.el" dir)))

(defun org-graph-tools--roam-root ()
  "Return the concept-vault / typed-edge root directory, with trailing slash.
Falls back to the loader defcustom default when `org-graph-roam-root' is
unbound (e.g. when this module is loaded standalone in a spec)."
  (file-name-as-directory
   (expand-file-name
    (if (boundp 'org-graph-roam-root) org-graph-roam-root "~/org/roam/"))))

(defun org-graph-tools--note->plist (note)
  "Project a `vulpea-note' NOTE onto a flat agent-facing plist.
Keys: :id :title :tags :path."
  (list :id    (vulpea-note-id note)
        :title (vulpea-note-title note)
        :tags  (vulpea-note-tags note)
        :path  (vulpea-note-path note)))

(defun org-graph-tools--edge->plist (edge)
  "Project a query-API EDGE plist onto the agent-facing edge shape.
EDGE is (:from :rel :to :note) from `org-graph-query/*'; the result is
(:from :rel :to :title), resolving the far-end note to its title (nil
when unresolved)."
  (let ((note (plist-get edge :note)))
    (list :from  (plist-get edge :from)
          :rel   (plist-get edge :rel)
          :to    (plist-get edge :to)
          :title (and note (vulpea-note-title note)))))

(defun org-graph-tools/query (&optional title-match tag)
  "Return note plists matching TITLE-MATCH (substring) and/or TAG (filetag).
TITLE-MATCH is matched case-insensitively as a literal substring of the
note title; TAG must be present in the note's filetags.  Both are
optional and combine with AND; with neither, all indexed notes are
returned.  Each result is `org-graph-tools--note->plist'-shaped."
  (let ((needle (and title-match (downcase title-match))))
    (mapcar
     #'org-graph-tools--note->plist
     (vulpea-db-query
      (lambda (note)
        (and (or (null needle)
                 (let ((title (vulpea-note-title note)))
                   (and title
                        (string-match-p (regexp-quote needle)
                                        (downcase title)))))
             (or (null tag)
                 (and (member tag (vulpea-note-tags note)) t))))))))

(defun org-graph-tools--rel-symbol (rel-type)
  "Coerce REL-TYPE to a relation SYMBOL, or nil.
The query API matches relations as symbols (emacsql prin1/read
round-trips them); an agent passes a string, so intern it."
  (cond ((null rel-type) nil)
        ((symbolp rel-type) rel-type)
        ((stringp rel-type) (intern rel-type))))

(defun org-graph-tools/typed-edges (note-id &optional direction rel-type)
  "Return resolved typed edges for NOTE-ID as agent-facing plists.
DIRECTION is \"outgoing\", \"incoming\", or \"connected\" (default).
REL-TYPE optionally narrows to a relation (string or symbol).  Each
result is `org-graph-tools--edge->plist'-shaped: (:from :rel :to :title).
Layered on `org-graph-query/{outgoing,incoming,connected}'."
  (let* ((rel (org-graph-tools--rel-symbol rel-type))
         (edges
          (pcase direction
            ((or "outgoing" 'outgoing)
             (org-graph-query/outgoing note-id rel))
            ((or "incoming" 'incoming)
             (org-graph-query/incoming note-id rel))
            (_
             (if rel
                 (append (org-graph-query/outgoing note-id rel)
                         (org-graph-query/incoming note-id rel))
               (org-graph-query/connected note-id))))))
    (mapcar #'org-graph-tools--edge->plist edges)))

(defun org-graph-tools--slug (title)
  "Return a filesystem-safe slug derived from TITLE.
Lower-cased, non-alphanumeric runs collapsed to single hyphens, leading
and trailing hyphens trimmed.  Falls back to \"note\" when TITLE yields an
empty slug."
  (let ((slug (replace-regexp-in-string
               "\\`-+\\|-+\\'" ""
               (replace-regexp-in-string
                "[^a-z0-9]+" "-" (downcase (or title ""))))))
    (if (string-empty-p slug) "note" slug)))

(defun org-graph-tools/write-node (title &optional directory tags body)
  "Create an `agent-draft'-stamped note titled TITLE; return its id and path.
DIRECTORY defaults to the roam concept vault (`org-graph-roam-root').  A
note written OUTSIDE that root is indexed for discovery but contributes
NO typed edges (register/invariant/typed-edge-extraction-scope).  TAGS is
a list (or vector) of filetag strings; \"agent-draft\" is always added
(once).  BODY is optional note content.

The file write is serialised through
`org-graph-coordinator/with-file-lock' on the target path (D5), so two
overlapping agent writes to the same path cannot corrupt it.  Returns a
plist (:id ID :path PATH)."
  (let* ((dir (file-name-as-directory
               (expand-file-name (or directory (org-graph-tools--roam-root)))))
         (tags (append tags nil))
         (all-tags (if (member "agent-draft" tags)
                       tags
                     (append tags (list "agent-draft"))))
         (path (expand-file-name
                (format "%s-%s.org"
                        (format-time-string "%Y%m%d%H%M%S")
                        (org-graph-tools--slug title))
                dir)))
    (org-graph-coordinator/with-file-lock path
      (let ((note (vulpea-create title path :tags all-tags :body body)))
        (list :id   (vulpea-note-id note)
              :path (vulpea-note-path note))))))

(defvar org-graph-tools--registered nil
  "List of org-graph `gptel-tool' objects built by `org-graph-tools-register'.
Surfaced via `org-graph/agent-tools'; nil until registration runs.")

(defconst org-graph-tools--write-node-description
  "Create a new org note and return its id and path. The note is stamped \
with the `agent-draft' filetag for later human review. The write is \
serialised through a per-file lock so concurrent agent writes cannot \
corrupt the file. BOUNDARY: typed-edge extraction runs ONLY on notes \
under the roam concept vault (~/org/roam/); a note written into any other \
directory is indexed for search but produces NO typed edges, so author \
notes you want in the typed graph under the roam root."
  "Description for the org_graph_write_node tool.
States the roam-only typed-edge boundary (register/invariant/typed-edge-extraction-scope)
so agent prompts stay honest.")

(defun org-graph-tools-register ()
  "Construct and register the three org-graph gptel tools.
Sets and returns `org-graph-tools--registered'.  Requires gptel
(`gptel-make-tool' fbound); the loader calls this only when that holds."
  (setq org-graph-tools--registered
        (list
         (gptel-make-tool
          :name "org_graph_query"
          :function
          (lambda (&optional title_match tag)
            (format "%S" (org-graph-tools/query title_match tag)))
          :description
          "Search indexed notes by a case-insensitive title substring \
and/or an exact filetag (both optional, combined with AND). Returns a \
list of note plists (:id :title :tags :path)."
          :args (list '(:name "title_match"
                        :type string
                        :optional t
                        :description "Case-insensitive substring to match against note titles.")
                      '(:name "tag"
                        :type string
                        :optional t
                        :description "Exact filetag a note must carry (e.g. \"topic\")."))
          :category "org-graph")
         (gptel-make-tool
          :name "org_graph_typed_edges"
          :function
          (lambda (note_id &optional direction rel_type)
            (format "%S" (org-graph-tools/typed-edges note_id direction rel_type)))
          :description
          "Return the typed semantic edges touching a note. DIRECTION is \
\"outgoing\", \"incoming\", or \"connected\" (default: both). Optionally \
narrow to one relation type. Returns edge plists (:from :rel :to :title) \
where :title is the far-end note's title."
          :args (list '(:name "note_id"
                        :type string
                        :description "The org id of the note to query edges for.")
                      '(:name "direction"
                        :type string
                        :optional t
                        :enum ["outgoing" "incoming" "connected"]
                        :description "Edge direction relative to the note; defaults to connected.")
                      '(:name "rel_type"
                        :type string
                        :optional t
                        :description "Optional relation to narrow to (e.g. \"implements\")."))
          :category "org-graph")
         (gptel-make-tool
          :name "org_graph_write_node"
          :function
          (lambda (title &optional directory tags body)
            (format "%S" (org-graph-tools/write-node title directory tags body)))
          :description org-graph-tools--write-node-description
          :args (list '(:name "title"
                        :type string
                        :description "The note title.")
                      '(:name "directory"
                        :type string
                        :optional t
                        :description "Directory to write into; defaults to the roam vault (~/org/roam/).")
                      '(:name "tags"
                        :type array
                        :optional t
                        :items (:type string)
                        :description "Filetags to add; \"agent-draft\" is always added.")
                      '(:name "body"
                        :type string
                        :optional t
                        :description "Optional note body content."))
          :category "org-graph")))
  org-graph-tools--registered)

(defun org-graph/agent-tools ()
  "Return the list of org-graph `gptel-tool' objects for preset :tools slots.
The reusable accessor the `workspace-integration' task hands to the
`workspace-assistant' preset, rather than relying only on gptel's global
registry.  Returns nil until `org-graph-tools-register' has run."
  org-graph-tools--registered)

(when (fboundp 'gptel-make-tool)
  (org-graph-tools-register))

(provide 'org-graph-tools)
;;; tools.el ends here
