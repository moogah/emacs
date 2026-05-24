;;; data-model-spec.el --- Unit tests for workspace data layer -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'bookmark)
(load (expand-file-name "../data-model.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name))))

(describe "workspace--make"
  (it "returns a workspace with the given name and empty groups"
    (let ((ws (workspace--make "code")))
      (expect (workspace--name ws) :to-equal "code")
      (expect (workspace--layout-groups ws) :to-equal nil)
      (expect (workspace--recent-group ws) :to-equal nil)
      (expect (workspace--buffer-files ws) :to-equal nil))))

(describe "workspace--upsert-group"
  (it "adds a group when none exists with that name"
    (let* ((ws (workspace--make "code"))
           (layout (workspace--layout-make 'fs1 100))
           (ws (workspace--upsert-group ws "home" layout)))
      (expect (length (workspace--layout-groups ws)) :to-equal 1)
      (expect (workspace--group-name
               (car (workspace--layout-groups ws)))
              :to-equal "home")))

  (it "replaces an existing group on second call with the same name"
    (let* ((ws (workspace--make "code"))
           (l1 (workspace--layout-make 'fs1 100))
           (l2 (workspace--layout-make 'fs2 200))
           (ws (workspace--upsert-group ws "home" l1))
           (ws (workspace--upsert-group ws "home" l2)))
      (expect (length (workspace--layout-groups ws)) :to-equal 1)
      (let* ((g (workspace--find-group ws "home"))
             (current (workspace--group-recent-layout g)))
        (expect (workspace--layout-frameset current) :to-equal 'fs2))))

  (it "preserves the original position of a replaced group"
    (let* ((ws (workspace--make "code"))
           (la (workspace--layout-make 'a 100))
           (lb (workspace--layout-make 'b 200))
           (lc (workspace--layout-make 'c 300))
           (lb2 (workspace--layout-make 'b2 400))
           (ws (workspace--upsert-group ws "alpha" la))
           (ws (workspace--upsert-group ws "beta"  lb))
           (ws (workspace--upsert-group ws "gamma" lc))
           (ws (workspace--upsert-group ws "beta"  lb2)))
      (expect (mapcar #'workspace--group-name
                      (workspace--layout-groups ws))
              :to-equal '("alpha" "beta" "gamma")))))

(describe "workspace--set-recent-group"
  (it "is non-destructive on the input workspace"
    (let* ((ws (workspace--make "code"))
           (_ (workspace--set-recent-group ws "home")))
      (expect (workspace--recent-group ws) :to-equal nil)))

  (it "sets the recent-layout-group on the returned workspace"
    (let* ((ws (workspace--make "code"))
           (ws2 (workspace--set-recent-group ws "home")))
      (expect (workspace--recent-group ws2) :to-equal "home"))))

(describe "workspace--find-group"
  (it "returns nil for an unknown name"
    (let ((ws (workspace--make "code")))
      (expect (workspace--find-group ws "missing") :to-equal nil)))

  (it "returns the matching group when present"
    (let* ((ws (workspace--make "code"))
           (layout (workspace--layout-make 'fs 100))
           (ws (workspace--upsert-group ws "home" layout))
           (g (workspace--find-group ws "home")))
      (expect (workspace--group-name g) :to-equal "home"))))

(describe "workspace--remove-group"
  (it "removes the named group; group count drops by one"
    (let* ((ws (workspace--make "code"))
           (ws (workspace--upsert-group ws "home"  (workspace--layout-make 'a 100)))
           (ws (workspace--upsert-group ws "magit" (workspace--layout-make 'b 200))))
      (expect (length (workspace--layout-groups ws)) :to-equal 2)
      (let ((ws (workspace--remove-group ws "magit")))
        (expect (length (workspace--layout-groups ws)) :to-equal 1)
        (expect (workspace--find-group ws "magit") :to-equal nil)
        (expect (workspace--find-group ws "home") :not :to-be nil)))))

(describe "workspace--group-name-reserved-p"
  (it "returns t for \"home\""
    (expect (workspace--group-name-reserved-p "home") :to-be t))

  (it "returns nil for arbitrary names"
    (expect (workspace--group-name-reserved-p "magit") :to-be nil)
    (expect (workspace--group-name-reserved-p "tests") :to-be nil)
    (expect (workspace--group-name-reserved-p "") :to-be nil))

  (it "returns nil for non-string input"
    (expect (workspace--group-name-reserved-p nil) :to-be nil)
    (expect (workspace--group-name-reserved-p 'home) :to-be nil)))

(describe "workspace--add-buffer-file"
  (it "appends a new path"
    (let* ((ws (workspace--make "code"))
           (ws (workspace--add-buffer-file ws "~/a.el")))
      (expect (workspace--buffer-files ws) :to-equal '("~/a.el"))))

  (it "deduplicates the same path"
    (let* ((ws (workspace--make "code"))
           (ws (workspace--add-buffer-file ws "~/a.el"))
           (ws (workspace--add-buffer-file ws "~/a.el")))
      (expect (workspace--buffer-files ws) :to-equal '("~/a.el"))))

  (it "preserves order across multiple distinct paths"
    (let* ((ws (workspace--make "code"))
           (ws (workspace--add-buffer-file ws "~/a.el"))
           (ws (workspace--add-buffer-file ws "~/b.el"))
           (ws (workspace--add-buffer-file ws "~/c.el")))
      (expect (workspace--buffer-files ws)
              :to-equal '("~/a.el" "~/b.el" "~/c.el")))))

(describe "workspace--remove-buffer-file"
  (it "removes the named path"
    (let* ((ws (workspace--make "code"))
           (ws (workspace--add-buffer-file ws "~/a.el"))
           (ws (workspace--add-buffer-file ws "~/b.el"))
           (ws (workspace--remove-buffer-file ws "~/a.el")))
      (expect (workspace--buffer-files ws) :to-equal '("~/b.el"))))

  (it "is a no-op when the path is absent"
    (let* ((ws (workspace--make "code"))
           (ws (workspace--add-buffer-file ws "~/a.el"))
           (ws (workspace--remove-buffer-file ws "~/missing.el")))
      (expect (workspace--buffer-files ws) :to-equal '("~/a.el")))))

(describe "workspace--layout-make"
  (it "defaults :timestamp to current time as integer"
    (let* ((before (time-convert nil 'integer))
           (layout (workspace--layout-make 'fs))
           (after (time-convert nil 'integer)))
      (expect (workspace--layout-timestamp layout) :to-be-truthy)
      (expect (workspace--layout-timestamp layout) :to-be-weakly-greater-than before)
      (expect (workspace--layout-timestamp layout) :to-be-weakly-less-than after)))

  (it "preserves the provided timestamp"
    (let ((layout (workspace--layout-make 'fs 12345)))
      (expect (workspace--layout-timestamp layout) :to-equal 12345)))

  (it "always carries :git-state nil"
    (let ((layout (workspace--layout-make 'fs)))
      (expect (plist-member layout :git-state) :to-be-truthy)
      (expect (plist-get layout :git-state) :to-equal nil))))

(describe "workspace-buffer struct"
  (it "constructs with all-nil defaults"
    (let ((wb (make-workspace-buffer)))
      (expect (workspace-buffer-bookmark wb) :to-be nil)
      (expect (workspace-buffer-filename wb) :to-be nil)
      (expect (workspace-buffer-name wb) :to-be nil)
      (expect (workspace-buffer-narrowed-p wb) :to-be nil)
      (expect (workspace-buffer-indirect-p wb) :to-be nil)
      (expect (workspace-buffer-local-variables wb) :to-be nil)
      (expect (workspace-buffer-etc wb) :to-be nil)))

  (it "round-trips slots through accessors"
    (let ((wb (make-workspace-buffer
               :bookmark '("foo" (filename . "/tmp/foo")
                                 (position . 42))
               :filename "/tmp/foo"
               :name "foo"
               :narrowed-p t
               :indirect-p nil)))
      (expect (workspace-buffer-name wb) :to-equal "foo")
      (expect (workspace-buffer-filename wb) :to-equal "/tmp/foo")
      (expect (workspace-buffer-narrowed-p wb) :to-be t)
      (expect (workspace-buffer-indirect-p wb) :to-be nil)
      (expect (bookmark-prop-get (workspace-buffer-bookmark wb) 'position)
              :to-equal 42)))

  (it "accepts a nil bookmark (non-bookmarkable buffer)"
    (let ((wb (make-workspace-buffer
               :bookmark nil
               :filename nil
               :name "*scratch*")))
      (expect (workspace-buffer-bookmark wb) :to-be nil)
      (expect (workspace-buffer-name wb) :to-equal "*scratch*")))

  (it "narrowed-p and indirect-p flags survive prin1/read round-trip"
    (let* ((wb (make-workspace-buffer
                :filename "/tmp/foo"
                :name "foo"
                :narrowed-p t
                :indirect-p t))
           (round-tripped (read (prin1-to-string wb))))
      (expect (workspace-buffer-p round-tripped) :to-be t)
      (expect (workspace-buffer-narrowed-p round-tripped) :to-be t)
      (expect (workspace-buffer-indirect-p round-tripped) :to-be t)
      (expect (workspace-buffer-name round-tripped) :to-equal "foo"))))

(provide 'data-model-spec)
;;; data-model-spec.el ends here
