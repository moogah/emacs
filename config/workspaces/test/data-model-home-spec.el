;;; data-model-home-spec.el --- Tests for :home slot and broken-state tag -*- lexical-binding: t; -*-

;; Covers the data-model contract introduced by task
;; add-home-slot-to-data-model: the required :home slot on every
;; workspace plist, its accessors (workspace--home, workspace--set-home),
;; and the runtime-only :broken tag (workspace--broken-p,
;; workspace--mark-broken, workspace--clear-broken).
;;
;; Pinned register entries (cycle 1 expectations):
;;   register/shape/workspace-plist-v3
;;   register/invariant/home-required-no-floating-workspaces
;;   register/invariant/registry-name-equals-basename (constructor side)

(require 'buttercup)
(require 'cl-lib)
(load (expand-file-name "../data-model.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name))))

(describe "workspace-data-model"

  (describe ":home accessor"

    (it "workspace--make produces a plist carrying both :name and :home"
      (let ((ws (workspace--make "foo" "/tmp/foo")))
        (expect (workspace--name ws) :to-equal "foo")
        (expect (workspace--home ws) :to-equal "/tmp/foo")))

    (it "workspace--home returns the path stored in :home"
      (let ((ws (workspace--make "alpha" "/tmp/alpha/")))
        (expect (workspace--home ws) :to-equal "/tmp/alpha/")))

    (it "workspace--home is defensive: returns nil when :home is absent"
      ;; A stale/corrupted plist (e.g. from a v2 persistence file that
      ;; somehow slipped past the skip-with-notice gate) must not error.
      (let ((ws (list :name "alpha")))
        (expect (workspace--home ws) :to-be nil)))

    (it "workspace--make signature requires HOME (no floating workspaces)"
      ;; Calling workspace--make with only NAME must signal
      ;; wrong-number-of-arguments. This is the structural enforcement
      ;; of register/invariant/home-required-no-floating-workspaces.
      (expect (workspace--make "alpha")
              :to-throw 'wrong-number-of-arguments))

    (it "workspace--set-home returns a NEW workspace with :home updated"
      (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
             (ws2 (workspace--set-home ws "/tmp/renamed")))
        (expect (workspace--home ws2) :to-equal "/tmp/renamed")
        ;; The :name is unchanged by set-home; the caller is responsible
        ;; for any re-anchor coordination (see workspace-re-anchor in
        ;; later cycle).
        (expect (workspace--name ws2) :to-equal "alpha")))

    (it "workspace--set-home does not mutate the input workspace"
      (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
             (_  (workspace--set-home ws "/tmp/elsewhere")))
        (expect (workspace--home ws) :to-equal "/tmp/alpha"))))

  (describe "name-equals-basename construction contract"
    ;; Constructor-side check for register/invariant/
    ;; registry-name-equals-basename. The data layer doesn't enforce
    ;; equality, but the canonical call site (tabs.org, later) passes
    ;; NAME = basename(HOME). This test asserts the round-trip at the
    ;; pure data layer.

    (it "name = basename(home) when constructed by the canonical pattern"
      (let* ((home "/tmp/alpha/")
             (name (file-name-nondirectory (directory-file-name home)))
             (ws (workspace--make name home)))
        (expect (workspace--name ws)
                :to-equal
                (file-name-nondirectory
                 (directory-file-name (workspace--home ws)))))))

  (describe "broken-state runtime tag"

    (it "workspace--broken-p is nil for a freshly-made workspace"
      (let ((ws (workspace--make "alpha" "/tmp/alpha")))
        (expect (workspace--broken-p ws) :to-be nil)))

    (it "workspace--mark-broken returns a NEW workspace flagged broken"
      (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
             (ws2 (workspace--mark-broken ws)))
        (expect (workspace--broken-p ws2) :to-be t)))

    (it "workspace--mark-broken does not mutate the input workspace"
      (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
             (_  (workspace--mark-broken ws)))
        (expect (workspace--broken-p ws) :to-be nil)))

    (it "workspace--clear-broken returns a NEW workspace with :broken nil"
      (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
             (ws-broken (workspace--mark-broken ws))
             (ws-cleared (workspace--clear-broken ws-broken)))
        (expect (workspace--broken-p ws-broken) :to-be t)
        (expect (workspace--broken-p ws-cleared) :to-be nil)))

    (it "workspace--clear-broken does not mutate the input workspace"
      (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
             (ws-broken (workspace--mark-broken ws))
             (_         (workspace--clear-broken ws-broken)))
        (expect (workspace--broken-p ws-broken) :to-be t)))

    (it "mark/clear/mark round-trip works without retaining stale state"
      (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
             (ws (workspace--mark-broken ws))
             (ws (workspace--clear-broken ws))
             (ws (workspace--mark-broken ws)))
        (expect (workspace--broken-p ws) :to-be t)))

    (it "marking broken preserves the rest of the workspace shape"
      ;; Marking broken should NOT disturb :name, :home, :layout-groups,
      ;; :recent-layout-group, or :buffer-files.
      (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
             (layout (workspace--layout-make 'fs 100))
             (ws (workspace--upsert-group ws "home" layout))
             (ws (workspace--add-buffer-file ws "~/a.el"))
             (ws (workspace--set-recent-group ws "home"))
             (ws-broken (workspace--mark-broken ws)))
        (expect (workspace--name ws-broken)          :to-equal "alpha")
        (expect (workspace--home ws-broken)          :to-equal "/tmp/alpha")
        (expect (workspace--recent-group ws-broken)  :to-equal "home")
        (expect (workspace--buffer-files ws-broken)  :to-equal '("~/a.el"))
        (expect (length (workspace--layout-groups ws-broken)) :to-equal 1)))))

(provide 'data-model-home-spec)
;;; data-model-home-spec.el ends here
