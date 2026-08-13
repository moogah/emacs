;;; authoring.el --- Human-side note authoring commands -*- lexical-binding: t; -*-

(require 'vulpea)

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

(provide 'org-graph-authoring)
;;; authoring.el ends here
