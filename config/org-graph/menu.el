;;; menu.el --- Human interaction surface for the org-graph note graph -*- lexical-binding: t; -*-

(require 'transient)
(require 'vulpea)

(declare-function org-graph-query/outgoing "org-graph-query" (from-id &optional rel-type))
(declare-function org-graph-query/incoming "org-graph-query" (to-id &optional rel-type))
(declare-function org-graph/validate-note-type "org-graph-schemas" (note))
(declare-function org-graph/validate-all-of-type "org-graph-schemas" (type))
(declare-function org-graph-schemas--note-types "org-graph-schemas" ())

(defun org-graph-menu--note-id-at-point (&optional noerror)
  "Return the :ID: governing point, or signal `user-error'.
Enclosing ID-bearing heading first, then file level (inherited
lookup, same idiom as `vulpea-find-backlink').  With NOERROR
non-nil, return nil instead of signaling when nothing at point
carries an :ID:."
  (or (org-entry-get nil "ID" t)
      (unless noerror
        (user-error "No note with an :ID: at point"))))

(defconst org-graph-menu-edges-buffer-name "*org-graph-edges*"
  "Name of the shared buffer the edge-query commands render into.
Each render replaces the previous contents.")

(defun org-graph-menu--insert-edge-section (title far-key edges)
  "Insert one direction section TITLE for EDGES at point.
EDGES is a list of edge plists as returned by the `org-graph-query'
functions.  FAR-KEY is the plist key holding the far-end id relative
to the query subject: `:to' for outgoing edges, `:from' for incoming.
Each edge renders as \"- <rel> :: [[id:<far-id>][<title>]]\", the
title taken from the already-resolved `:note' slot with the raw
far-end id as fallback."
  (insert "* " title "\n")
  (if (null edges)
      (insert "No edges.\n")
    (dolist (edge edges)
      (let* ((far-id (plist-get edge far-key))
             (note (plist-get edge :note))
             (desc (or (and note (vulpea-note-title note)) far-id)))
        (insert (format "- %s :: %s\n"
                        (plist-get edge :rel)
                        (org-link-make-string (concat "id:" far-id)
                                              desc)))))))

(defun org-graph-menu--render-edges (subject-id sections)
  "Render SECTIONS of typed edges for SUBJECT-ID and display the buffer.
SECTIONS is a list of (TITLE FAR-KEY EDGES) triples, one per queried
direction, rendered in order into `org-graph-menu-edges-buffer-name'
via `org-graph-menu--insert-edge-section'.  The buffer ends up in
`org-mode' and read-only, is displayed with `pop-to-buffer', and is
returned."
  (let ((buffer (get-buffer-create org-graph-menu-edges-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (insert (format "Typed edges for id:%s\n\n" subject-id))
        (pcase-dolist (`(,title ,far-key ,edges) sections)
          (org-graph-menu--insert-edge-section title far-key edges)))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    buffer))

(defun org-graph/edges-outgoing-at-point ()
  "Show typed edges authored on the note at point.
Resolves the subject via `org-graph-menu--note-id-at-point' and
renders `org-graph-query/outgoing' results into the shared edges
buffer (see `org-graph-menu--render-edges')."
  (interactive)
  (let ((id (org-graph-menu--note-id-at-point)))
    (org-graph-menu--render-edges
     id (list (list "Outgoing" :to (org-graph-query/outgoing id))))))

(defun org-graph/edges-incoming-at-point ()
  "Show typed edges pointing at the note at point.
Resolves the subject via `org-graph-menu--note-id-at-point' and
renders `org-graph-query/incoming' results into the shared edges
buffer (see `org-graph-menu--render-edges')."
  (interactive)
  (let ((id (org-graph-menu--note-id-at-point)))
    (org-graph-menu--render-edges
     id (list (list "Incoming" :from (org-graph-query/incoming id))))))

(defun org-graph/edges-connected-at-point ()
  "Show every typed edge touching the note at point, both directions.
Resolves the subject via `org-graph-menu--note-id-at-point' and
renders an Outgoing and an Incoming section, deliberately issuing the
two directional queries instead of `org-graph-query/connected': the
renderer needs per-direction far-end attribution, which a flat union
cannot supply for a self-edge.  Read-time enrichment added to
`org-graph-query/connected' must be mirrored here."
  (interactive)
  (let ((id (org-graph-menu--note-id-at-point)))
    (org-graph-menu--render-edges
     id (list (list "Outgoing" :to (org-graph-query/outgoing id))
              (list "Incoming" :from (org-graph-query/incoming id))))))

(defun org-graph-menu--report-violations (subject violations)
  "Echo a summary of schema VIOLATIONS for SUBJECT, a description string."
  (if (null violations)
      (message "org-graph: %s conforms to the note-type schemas" subject)
    (message "org-graph: %d violation(s) for %s:\n%s"
             (length violations) subject
             (mapconcat (lambda (v) (format "- %S" v)) violations "\n"))))

(defun org-graph/validate-note-at-point-or-prompt ()
  "Validate the note at point; with no note at point, validate a whole type.
Thin interactive front door over the schemas module: point inside an
ID-bearing note validates that note via `org-graph/validate-note-type';
otherwise prompt for a type from `org-graph-schemas--note-types' and
run `org-graph/validate-all-of-type'.  No validation logic lives here."
  (interactive)
  (let ((id (org-graph-menu--note-id-at-point 'noerror)))
    (if id
        (let ((note (vulpea-db-get-by-id id)))
          (unless note
            (user-error "Note %s is not in the vulpea index" id))
          (org-graph-menu--report-violations
           (format "note %S" (vulpea-note-title note))
           (org-graph/validate-note-type note)))
      (let ((type (intern (completing-read
                           "Validate all notes of type: "
                           (mapcar #'symbol-name (org-graph-schemas--note-types))
                           nil t))))
        (org-graph-menu--report-violations
         (format "note type `%s'" type)
         (org-graph/validate-all-of-type type))))))

(transient-define-prefix org-graph-menu ()
  "Human interaction surface for the org-graph note graph."
  [["Find"
    ("t" "topic" org-graph/find-topic)
    ("d" "debug" org-graph/find-debug)
    ("l" "log" org-graph/find-log)
    ("r" "reference" org-graph/find-reference)
    ("p" "project" org-graph/find-project)
    ("a" "any" org-graph/find-any)
    ("D" "agent drafts" org-graph/find-agent-drafts)]
   ["Author"
    ("f" "find or create" org-graph/find-or-create)
    ("i" "insert link" org-graph/insert-link)]
   ["Edges (at point)"
    ("o" "outgoing" org-graph/edges-outgoing-at-point)
    ("n" "incoming" org-graph/edges-incoming-at-point)
    ("c" "connected" org-graph/edges-connected-at-point)]
   ["Maintain"
    ("s" "re-index (sync)" org-graph/configure-sync)
    ("v" "validate note type" org-graph/validate-note-at-point-or-prompt)
    ("h" "doctor" vulpea-doctor)]])

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<SPC> v") #'org-graph-menu))

(provide 'org-graph-menu)
;;; menu.el ends here
