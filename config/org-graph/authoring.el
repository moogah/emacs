;;; authoring.el --- Human-side note authoring commands -*- lexical-binding: t; -*-

(require 'org)
(require 'org-id)
(require 'org-element)
(require 'vulpea)
(require 'org-graph-extractor)
(require 'org-graph-coordinator)

(defun org-graph/find-or-create ()
  "Find a note by title, creating it when no indexed note matches.
Thin wrapper over `vulpea-find' with completion over every indexed
note; creation is vulpea's synchronous birth-index path (file +
:ID: + org-id registration + DB insert complete before return)."
  (interactive)
  (vulpea-find :require-match nil))

(defun org-graph/insert-link ()
  "Insert an id: link to a note at point, creating the note on miss.
Thin wrapper over `vulpea-insert': active region becomes the link
description and is replaced by the link."
  (interactive)
  (vulpea-insert))

(defun org-graph-authoring--rel-symbol (type)
  "Return the canonical relation symbol for TYPE, or nil when empty.
TYPE is a symbol or string; both route through the extractor's single
normaliser `org-graph-extractor--normalize-rel' so the written tag
equals what the reader stores."
  (org-graph-extractor--normalize-rel
   (cond ((null type) nil)
         ((symbolp type) (symbol-name type))
         (t type))))

(defun org-graph-authoring--locate-node (node-id file)
  "Return the position where NODE-ID's node begins in the current buffer.
A nil NODE-ID names the file-level node (`point-min').  Otherwise the
buffer is scanned for the entry whose :ID: property equals NODE-ID
\(`org-find-property', bounded to this buffer — not `org-id-find').
Signals `user-error' naming FILE when no such node exists."
  (if (null node-id)
      (point-min)
    (or (org-find-property "ID" node-id)
        (user-error "org-graph/write-edge: no node with :ID: %s in %s"
                    node-id file))))

(defun org-graph-authoring--after-property-drawer (node-pos node-id file)
  "Return the position of the line after the property drawer at NODE-POS.
NODE-POS is a heading position or `point-min'.  Signals `user-error'
\(naming NODE-ID and FILE) when the node has no :PROPERTIES: drawer.
When the drawer's :END: is the last line of the buffer and lacks a
trailing newline, the returned position is `point-max' at that line's
end; the caller inserts a newline first."
  (let ((block (org-get-property-block node-pos)))
    (unless block
      (user-error "org-graph/write-edge: node %s in %s has no property drawer"
                  (or node-id "(file-level)") file))
    (save-excursion
      (goto-char (cdr block))
      (forward-line 1)
      (point))))

(defun org-graph-authoring--edge-drawer-at (pos drawer-name)
  "Return the drawer element opening exactly at POS named DRAWER-NAME, or nil.
The name comparison is case-insensitive (org drawer semantics).  A
drawer that starts later, an unrelated element, or a drawer of another
name yields nil."
  (save-excursion
    (goto-char pos)
    (let ((element (org-element-at-point)))
      (when (and (eq (org-element-type element) 'drawer)
                 (= (org-element-property :begin element) pos)
                 (string-equal
                  (downcase (or (org-element-property :drawer-name element) ""))
                  (downcase drawer-name)))
        element))))

(defun org-graph-authoring--drawer-has-edge-p (drawer rel target)
  "Return non-nil when DRAWER already holds an item for REL -> TARGET.
DRAWER is an unparsed drawer element (from `org-element-at-point');
its contents are parsed in a narrowed region and walked with the same
item / `id:' link rules the extractor reads with
\(`org-graph-extractor--item-relation', links at item depth only).
Equality is (normalised tag, id: link path); link descriptions are
ignored."
  (let ((tree (save-restriction
                (narrow-to-region (org-element-property :begin drawer)
                                  (org-element-property :end drawer))
                (org-element-parse-buffer))))
    (org-element-map tree 'item
      (lambda (item)
        (and (eq (org-graph-extractor--item-relation item) rel)
             (org-element-map (org-element-contents item) 'link
               (lambda (link)
                 (and (equal (org-element-property :type link) "id")
                      (equal (org-element-property :path link) target)))
               nil t 'item)))
      nil t 'item)))

(defun org-graph-authoring--append-edge-item (pos drawer-name rel target item)
  "Append ITEM for REL -> TARGET to the edge drawer at POS, creating it if absent.
POS is the line after the node's property drawer.  When a drawer named
DRAWER-NAME opens there, ITEM is inserted before its :END: unless an
equal edge is already present (then nothing changes).  Otherwise a new
drawer holding ITEM is inserted at POS.  Returns non-nil when the
buffer was modified."
  (goto-char pos)
  (unless (bolp) (insert "\n"))
  (let ((drawer (org-graph-authoring--edge-drawer-at (point) drawer-name)))
    (cond
     ((null drawer)
      (insert (format ":%s:\n%s:END:\n" drawer-name item))
      t)
     ((org-graph-authoring--drawer-has-edge-p drawer rel target)
      nil)
     (t
      (goto-char (or (org-element-property :contents-end drawer)
                     (save-excursion
                       (goto-char (org-element-property :begin drawer))
                       (re-search-forward "^[ \t]*:END:[ \t]*$"
                                          (org-element-property :end drawer))
                       (line-beginning-position))))
      (insert item)
      t))))

(defun org-graph/write-edge (file type target &optional node-id)
  "Append the typed edge TYPE -> TARGET to a node's edge drawer in FILE.

TYPE is a relation symbol or string, normalised through the
extractor's `org-graph-extractor--normalize-rel' before it is written
\(\"Part_of\" and `part-of' are the same edge).  TARGET is the org ID
the edge points at.  NODE-ID names the heading in FILE whose :ID:
equals it; nil (the default) means the file-level node.

Runs under `org-graph-coordinator/with-file-lock' on FILE.  FILE is
read into a temp buffer (never visited with `find-file', no mode
hooks), the node's :PROPERTIES: drawer is located, and the item
`- TYPE :: [[id:TARGET]]' is appended to the drawer named by
`org-graph-extractor--edge-drawer-name' directly after it — creating
that drawer on the line after the property drawer's :END: when absent.
The file is then written back with every other byte unchanged and
reindexed synchronously via `vulpea-db-update-file'.

Idempotent: when an item with the same normalised relation and the
same id: link path already exists in that drawer, nothing is written
or reindexed and nil is returned.  Otherwise returns the appended item
text.

Signals `user-error' (before taking the lock) for an empty TYPE or
TARGET or an unconfigured drawer name, and (under the lock, which is
released) when NODE-ID is not in FILE or the node has no property
drawer."
  (let* ((file (expand-file-name file))
         (rel (org-graph-authoring--rel-symbol type))
         (drawer-name (org-graph-extractor--edge-drawer-name)))
    (unless rel
      (user-error "org-graph/write-edge: empty relation type"))
    (unless (and (stringp target)
                 (not (string-empty-p (string-trim target))))
      (user-error "org-graph/write-edge: empty target id"))
    (unless drawer-name
      (user-error "org-graph/write-edge: no edge drawer name configured (org-graph-edge-drawer)"))
    (org-graph-coordinator/with-file-lock file
      (with-temp-buffer
        (insert-file-contents file)
        (let ((org-mode-hook nil)
              (org-element-use-cache nil))
          (delay-mode-hooks (org-mode))
          (let* ((node-pos (org-graph-authoring--locate-node node-id file))
                 (pos (org-graph-authoring--after-property-drawer
                       node-pos node-id file))
                 (item (format "- %s :: [[id:%s]]\n" (symbol-name rel) target)))
            (when (org-graph-authoring--append-edge-item
                   pos drawer-name rel target item)
              (write-region (point-min) (point-max) file nil 'silent)
              (vulpea-db-update-file file)
              item)))))))

(defconst org-graph-authoring--connected-block
  "#+BEGIN: org-graph-connected\n#+END:"
  "Empty connected-notes dynamic block stamped into every new project note.
Filled later by `org-dblock-write:org-graph-connected' on update.")

(defun org-graph/create-project-note (title &optional body)
  "Create an indexed vault note of type `project' titled TITLE; return its ID.

The note carries `#+filetags: :project:', the meta item
`- status :: active' (the `project' schema reads `status' via
`vulpea-note-meta-get'), an empty `org-graph-connected' dynamic block,
and BODY (when non-nil and non-empty) after it.  BODY is inserted
verbatim — it is passed to `vulpea-create' as a context value, so
template directives inside it are never expanded.

Placement follows `vulpea-default-notes-directory' and the configured
filename template; creation is `vulpea-create''s synchronous
birth-index path, so the returned ID resolves via `vulpea-db-get-by-id'
and the `project' finder offers the note with no manual sync."
  (let* ((body (and body (not (string-empty-p body))
                    (string-trim-right body "\n+")))
         (template (if body
                       (concat org-graph-authoring--connected-block "\n\n${body}\n")
                     (concat org-graph-authoring--connected-block "\n")))
         (note (vulpea-create title nil
                              :tags '("project")
                              :meta '(("status" . "active"))
                              :body template
                              :context (and body (list :body body)))))
    (vulpea-note-id note)))

(provide 'org-graph-authoring)
;;; authoring.el ends here
