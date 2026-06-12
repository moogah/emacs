;;; home-org-read-pipeline.el --- speculated boundary -*- lexical-binding: t; -*-

;; scaffolding-of: register/boundary/home-org-read-pipeline
;; generated-at: 2026-05-25T18:04:59Z
;; license: implementor-may-revise
;;
;; Boundary shell: the home.org read-on-demand pipeline. Single
;; user-facing function (workspace-home-org-title) composed of five
;; stages, each of which the Implementor must satisfy in
;; config/workspaces/home-org.org's tangled output. The deliberate
;; short-circuit at stage 2 (readability-check) is the structural
;; enforcement of the "missing home.org → display name falls back
;; to registry-name" promise.
;;
;; Two contractual properties this pipeline pins, beyond the per-
;; stage contracts:
;;   - NO caching layer. Every call re-reads the file. The boundary
;;     entry forbids a memoising wrapper; adding one is a divergence.
;;   - NO org-mode load. Stage 3 uses with-temp-buffer +
;;     insert-file-contents only; activating org-mode (even
;;     transparently via auto-mode-alist) is a divergence.

;; Stage 1: path-resolve.
(defun workspace-home-org-path (home)
  "Stage 1: return the absolute path to home.org inside HOME directory.

Speculation: (expand-file-name \"home.org\" HOME). Pure; no I/O.
The Implementor MUST keep this pure — if path resolution starts
consulting environment variables, defcustoms, or the filesystem,
that is a boundary divergence."
  (ignore home)
  (error "TODO: implement workspace-home-org-path — \
return (expand-file-name \"home.org\" HOME)."))

;; Stage 2: readability-check (also the sibling helper's exit point).
(defun workspace-home-org-exists-p (home)
  "Stage 2: return non-nil if HOME contains a readable home.org file.

Speculated: (file-readable-p (workspace-home-org-path home)).
This function is BOTH the standalone exists-check used by the
anchor-existing flow's case-distinguisher AND stage 2 of the title
read. Both consumers want the same answer; do not fork the logic."
  (ignore home)
  (error "TODO: implement workspace-home-org-exists-p — \
return (file-readable-p (workspace-home-org-path HOME))."))

;; Stages 3-5: load, regex-scan, trim-and-filter (one composed function).
(defun workspace-home-org-title (home)
  "Return the trimmed #+TITLE: keyword value from HOME/home.org, or nil.

Implements stages 3-5 of register/boundary/home-org-read-pipeline:
  3. load-to-temp-buffer  — with-temp-buffer + insert-file-contents
                            (NO org-mode; NO file-handlers beyond the
                            built-in insert-file-contents path)
  4. regex-scan-title     — case-insensitive match for
                            ^#\\+TITLE:[ \\t]+\\(.*\\)$
  5. trim-and-filter      — string-trim + reject empty

Returns nil at any stage-2 short-circuit (file missing/unreadable),
at any stage-4 miss (no #+TITLE: line), or at any stage-5 reject
(trimmed value is empty).

NO caching. Every call re-reads the file. If a future task wants
caching, it MUST update register/boundary/home-org-read-pipeline
first; adding a cache here silently is a divergence finding."
  (ignore home)
  (error "TODO: implement workspace-home-org-title per the staged \
contract above. The compact form per design D4 is:
  (when (workspace-home-org-exists-p home)
    (with-temp-buffer
      (insert-file-contents (workspace-home-org-path home))
      (goto-char (point-min))
      (when (re-search-forward \"^#\\\\+TITLE:[ \\\\t]+\\\\(.*\\\\)$\" nil t)
        (let ((title (string-trim (match-string 1))))
          (and (not (string-empty-p title)) title)))))
The case-insensitive match is inherited from the default
case-fold-search=t in temp buffers; if a future Emacs changes that
default, switch to an explicit (let ((case-fold-search t)) ...)."))

(provide 'home-org-read-pipeline)
;;; home-org-read-pipeline.el ends here
