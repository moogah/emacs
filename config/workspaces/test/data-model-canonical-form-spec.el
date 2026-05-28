;;; data-model-canonical-form-spec.el --- Canonical :home form invariant -*- lexical-binding: t; -*-

;; Pins the canonical-form contract for the :home slot of
;; register/shape/workspace-plist-v3, introduced by cycle-5 task
;; canonicalize-workspace-home-path-form (closes cycle-4 reviewer
;; Finding 1 on workspace-new-anchor-existing and architect
;; arch-cycle-20260526-191802-02 multi-slot frame).
;;
;; The contract: workspace--make canonicalises HOME to:
;;   - absolute (expand-file-name applied)
;;   - trailing slash present (file-name-as-directory applied)
;;   - symlinks NOT resolved (preserves user's resolution choice)
;;
;; All three known producers reach :home through the constructor
;; (or workspace--set-home), so this single attachment point pins the
;; cross-producer equality the shape entry requires:
;;   1. workspace--new-default-path     (config/workspaces/tabs.org)
;;   2. workspace--new-anchor-existing  (config/workspaces/tabs.org)
;;   3. workspace-re-anchor             (config/workspaces/workspaces.org)
;;      → reaches via workspace--set-home

(require 'buttercup)
(require 'cl-lib)
(load (expand-file-name "../data-model.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name))))

(describe "workspace--make canonicalises :home"

  (it "trailing-slash and no-trailing-slash inputs produce identical :home"
    ;; The cycle-4 asymmetry: workspace--new-default-path passed
    ;; "/tmp/ws" (no slash) while workspace--new-anchor-existing
    ;; passed "/tmp/ws/" (slash).  After canonicalisation in the
    ;; constructor, both must yield the same :home.
    (let ((without (workspace--make "ws" "/tmp/ws"))
          (with    (workspace--make "ws" "/tmp/ws/")))
      (expect (workspace--home without)
              :to-equal (workspace--home with))
      (expect (workspace--home without) :to-equal "/tmp/ws/")))

  (it "trailing slash is always present on :home"
    ;; Structural pin: every workspace constructed via workspace--make
    ;; carries a :home that ends in "/".  Producers that compute home
    ;; via expand-file-name no longer need to wrap in
    ;; file-name-as-directory at the call site.
    (let ((ws (workspace--make "foo" "/tmp/foo")))
      (expect (string-suffix-p "/" (workspace--home ws)) :to-be t)))

  (it "tilde expansion is applied (expand-file-name normalisation)"
    ;; The cycle-3 broken-home-tolerance frame proved string=
    ;; equality matters; tilde-prefixed inputs must collapse to the
    ;; same absolute form as their pre-expanded twin.
    (let* ((expanded (expand-file-name "~/some-ws/"))
           (from-tilde (workspace--make "some-ws" "~/some-ws"))
           (from-abs   (workspace--make "some-ws" expanded)))
      (expect (workspace--home from-tilde)
              :to-equal (workspace--home from-abs))
      (expect (file-name-absolute-p (workspace--home from-tilde))
              :to-be t)))

  (it "already-canonical input is a no-op (idempotent)"
    ;; Producers that already canonicalise at the call site (e.g.
    ;; workspace--new-anchor-existing wraps its read-directory-name
    ;; result in file-name-as-directory) must observe no change.
    (let* ((canonical "/tmp/already-canonical/")
           (ws (workspace--make "already-canonical" canonical)))
      (expect (workspace--home ws) :to-equal canonical)))

  (it "double-slash and trailing-slash interplay collapses cleanly"
    ;; expand-file-name normalises "//" sequences inside the path;
    ;; file-name-as-directory adds at most one trailing slash.
    (let ((ws (workspace--make "ws" "/tmp//ws//")))
      ;; The canonical form does NOT introduce double slashes, even if
      ;; the input had them.  expand-file-name collapses internal "//".
      (expect (workspace--home ws) :to-equal "/tmp/ws/"))))

(describe "workspace--set-home canonicalises :home"

  (it "post-construction re-anchor also yields canonical form"
    ;; workspace-re-anchor is the third producer (cycle-3); it reaches
    ;; :home via workspace--set-home, NOT workspace--make.  The setter
    ;; must canonicalise identically so the re-anchor path can't
    ;; smuggle a non-canonical :home back into the registry.
    (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
           (re-anchored (workspace--set-home ws "/tmp/beta")))
      (expect (workspace--home re-anchored) :to-equal "/tmp/beta/")))

  (it "set-home is idempotent on canonical input"
    (let* ((ws (workspace--make "alpha" "/tmp/alpha"))
           (re-anchored (workspace--set-home ws "/tmp/beta/")))
      (expect (workspace--home re-anchored) :to-equal "/tmp/beta/")))

  (it "set-home + make agree on the same input"
    ;; Cross-attachment-point invariant: whichever path inserts the
    ;; :home, the resulting form is the same.
    (let* ((from-make (workspace--make "ws" "/tmp/ws"))
           (seed      (workspace--make "ws" "/other"))
           (from-set  (workspace--set-home seed "/tmp/ws")))
      (expect (workspace--home from-set)
              :to-equal (workspace--home from-make)))))

(provide 'data-model-canonical-form-spec)
;;; data-model-canonical-form-spec.el ends here
