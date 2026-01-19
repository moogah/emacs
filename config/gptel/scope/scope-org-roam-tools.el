;; Dependencies


;; [[file:scope-org-roam-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'org-roam)
(require 'jf-gptel-scope-core)
;; Dependencies:1 ends here

;; Create Org-Roam Node Tool (Scope-Aware)

;; Create an org-roam node, checking subdirectory and tags against scope.


;; [[file:scope-org-roam-tools.org::*Create Org-Roam Node Tool (Scope-Aware)][Create Org-Roam Node Tool (Scope-Aware):1]]
(gptel-make-tool
 :name "create_roam_node_in_scope"
 :description "Create an org-roam node, respecting scope plan.
Checks subdirectory and tags against approved patterns.

Subdirectory patterns: 'subdirectory:gptel/**' allows nodes in gptel/ subdirectory
Tag patterns: 'tag:gptel' allows nodes with 'gptel' tag

Returns scope violation if subdirectory or tags not approved.
Use request_scope_expansion to ask user for approval if needed."
 :args (list '(:name "title"
               :type string
               :description "Node title")
             '(:name "tags"
               :type array
               :items (:type string)
               :optional t
               :description "Tags for node (list of strings)")
             '(:name "subdirectory"
               :type string
               :optional t
               :description "Subdirectory in org-roam-directory (e.g., 'gptel' for org-roam-directory/gptel/)")
             '(:name "aliases"
               :type array
               :items (:type string)
               :optional t
               :description "Node aliases (list of strings)"))
 :category "org-roam"
 :function
 (lambda (title &optional tags subdirectory aliases)
   ;; Convert array parameters from vectors to lists if needed (JSON arrays come as vectors)
   (when (vectorp tags)
     (setq tags (append tags nil)))
   (when (vectorp aliases)
     (setq aliases (append aliases nil)))

   (let* ((session-id (jf/gptel-scope--get-session-id))
          (plan (jf/gptel-scope--load-plan session-id)))

     (unless plan
       (cl-return-from nil
         (list :success nil
               :error "no_scope_plan"
               :message "No scope plan found for this session.")))

     ;; Check scope
     (let ((check-result (jf/gptel-scope--check-org-roam-write plan subdirectory tags)))
       (if (plist-get check-result :allowed)
           ;; Allowed - execute creation
           (let* ((node-id (org-id-new))
                  (target-dir (if subdirectory
                                  (expand-file-name subdirectory org-roam-directory)
                                org-roam-directory))
                  (slug (org-roam-node-slug (org-roam-node-create :title title)))
                  (file-path (expand-file-name (concat slug ".org") target-dir)))

             ;; Ensure target directory exists
             (unless (file-exists-p target-dir)
               (make-directory target-dir t))

             ;; Create org-roam file
             (with-temp-file file-path
               (insert (format "#+title: %s\n" title))
               (when tags
                 (insert (format "#+filetags: :%s:\n" (string-join tags ":"))))
               (when aliases
                 (insert (format "#+roam_aliases: %s\n" (string-join aliases " "))))
               (insert (format "\n:PROPERTIES:\n:ID: %s\n:END:\n\n" node-id)))

             ;; Update org-roam database
             (org-roam-db-update-file file-path)

             (list :success t
                   :node_id node-id
                   :file_path file-path
                   :message (format "Node created: %s" title)))

         ;; Denied
         (jf/gptel-scope--format-error "org_roam" "write"
                                       (format "subdirectory:%s tags:%s"
                                               (or subdirectory "")
                                               (if tags (string-join tags ",") ""))
                                       check-result))))))
;; Create Org-Roam Node Tool (Scope-Aware):1 ends here

;; Link Org-Roam Nodes Tool (Scope-Aware)

;; Create link between two org-roam nodes, checking link scope.


;; [[file:scope-org-roam-tools.org::*Link Org-Roam Nodes Tool (Scope-Aware)][Link Org-Roam Nodes Tool (Scope-Aware):1]]
(gptel-make-tool
 :name "link_roam_nodes_in_scope"
 :description "Create link between two org-roam nodes, respecting scope plan.

Link patterns in scope:
- 'node_id:*' allows linking any nodes (wildcard)
- 'node_id:SPECIFIC-ID' allows linking specific node

Adds a link in source node pointing to target node.
Returns scope violation if link operation not approved."
 :args (list '(:name "source_id"
               :type string
               :description "Source node ID (UUID)")
             '(:name "target_id"
               :type string
               :description "Target node ID (UUID)")
             '(:name "description"
               :type string
               :optional t
               :description "Link description (defaults to target node title)"))
 :category "org-roam"
 :function
 (lambda (source-id target-id &optional description)
   (let* ((session-id (jf/gptel-scope--get-session-id))
          (plan (jf/gptel-scope--load-plan session-id)))

     (unless plan
       (cl-return-from nil
         (list :success nil
               :error "no_scope_plan"
               :message "No scope plan found for this session.")))

     ;; Check scope for link operations
     (let* ((link-scope (plist-get (plist-get plan :org_roam) :link))
            (allowed (or (member "node_id:*" link-scope)  ; Wildcard
                        (member (concat "node_id:" source-id) link-scope)
                        (member (concat "node_id:" target-id) link-scope))))

       (if allowed
           ;; Allowed - create link
           (condition-case err
               (let* ((source-node (org-roam-node-from-id source-id))
                      (target-node (org-roam-node-from-id target-id))
                      (source-file (org-roam-node-file source-node)))

                 (unless source-node
                   (cl-return-from nil
                     (list :success nil
                           :error "node_not_found"
                           :message (format "Source node not found: %s" source-id))))

                 (unless target-node
                   (cl-return-from nil
                     (list :success nil
                           :error "node_not_found"
                           :message (format "Target node not found: %s" target-id))))

                 ;; Insert link in source file
                 (with-current-buffer (find-file-noselect source-file)
                   (goto-char (point-max))
                   (insert (format "\n- [[id:%s][%s]]"
                                 target-id
                                 (or description (org-roam-node-title target-node))))
                   (save-buffer))

                 (list :success t
                       :message (format "Linked %s -> %s"
                                       (org-roam-node-title source-node)
                                       (org-roam-node-title target-node))))
             (error
              (list :success nil
                    :error "link_failed"
                    :message (format "Failed to create link: %s" (error-message-string err)))))

         ;; Denied
         (list :success nil
               :error "scope_violation"
               :resource_type "org_roam"
               :operation "link"
               :resource (format "node_id:%s -> node_id:%s" source-id target-id)
               :allowed_patterns link-scope
               :message (format "Link operation denied. Allowed patterns: %s. Use request_scope_expansion to ask user for approval."
                              (if link-scope
                                  (string-join link-scope ", ")
                                "none"))))))))
;; Link Org-Roam Nodes Tool (Scope-Aware):1 ends here

;; Add Tags to Org-Roam Node Tool (Scope-Aware)

;; Add tags to an existing org-roam node, checking tag scope.


;; [[file:scope-org-roam-tools.org::*Add Tags to Org-Roam Node Tool (Scope-Aware)][Add Tags to Org-Roam Node Tool (Scope-Aware):1]]
(gptel-make-tool
 :name "add_roam_tags_in_scope"
 :description "Add tags to an existing org-roam node, respecting scope plan.

Tag patterns in scope:
- 'tag:gptel' allows adding 'gptel' tag
- 'tag:project' allows adding 'project' tag

Returns scope violation if tags not approved."
 :args (list '(:name "node_id"
               :type string
               :description "Node ID (UUID)")
             '(:name "tags"
               :type array
               :items (:type string)
               :description "Tags to add (list of strings)"))
 :category "org-roam"
 :function
 (lambda (node-id tags)
   ;; Convert tags from vector to list if needed (JSON arrays come as vectors)
   (when (vectorp tags)
     (setq tags (append tags nil)))

   (let* ((session-id (jf/gptel-scope--get-session-id))
          (plan (jf/gptel-scope--load-plan session-id)))

     (unless plan
       (cl-return-from nil
         (list :success nil
               :error "no_scope_plan"
               :message "No scope plan found for this session.")))

     ;; Check scope - treat as write operation with tags
     (let ((check-result (jf/gptel-scope--check-org-roam-write plan nil tags)))
       (if (plist-get check-result :allowed)
           ;; Allowed - add tags
           (condition-case err
               (let* ((node (org-roam-node-from-id node-id))
                      (node-file (org-roam-node-file node)))

                 (unless node
                   (cl-return-from nil
                     (list :success nil
                           :error "node_not_found"
                           :message (format "Node not found: %s" node-id))))

                 ;; Update file tags
                 (with-current-buffer (find-file-noselect node-file)
                   (goto-char (point-min))
                   ;; Find or create #+filetags: line
                   (if (re-search-forward "^#\\+filetags: \\(.*\\)$" nil t)
                       ;; Append to existing tags
                       (let* ((existing-tags (match-string 1))
                              (tag-list (split-string existing-tags ":" t))
                              (new-tags (cl-remove-duplicates (append tag-list tags) :test #'string=))
                              (new-tag-string (concat ":" (string-join new-tags ":") ":")))
                         (replace-match new-tag-string nil nil nil 1))
                     ;; Insert new filetags line after title
                     (goto-char (point-min))
                     (if (re-search-forward "^#\\+title:" nil t)
                         (progn
                           (end-of-line)
                           (insert (format "\n#+filetags: :%s:" (string-join tags ":"))))
                       (insert (format "#+filetags: :%s:\n" (string-join tags ":")))))
                   (save-buffer))

                 ;; Update org-roam database
                 (org-roam-db-update-file node-file)

                 (list :success t
                       :message (format "Tags added to node: %s" (string-join tags ", "))))
             (error
              (list :success nil
                    :error "tag_update_failed"
                    :message (format "Failed to add tags: %s" (error-message-string err)))))

         ;; Denied
         (jf/gptel-scope--format-error "org_roam" "write"
                                       (format "tags:%s" (string-join tags ","))
                                       check-result))))))
;; Add Tags to Org-Roam Node Tool (Scope-Aware):1 ends here

;; Provide Feature


;; [[file:scope-org-roam-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-org-roam-tools)
;;; scope-org-roam-tools.el ends here
;; Provide Feature:1 ends here
