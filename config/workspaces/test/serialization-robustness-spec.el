;;; serialization-robustness-spec.el --- Readable-by-construction serialization -*- lexical-binding: t; -*-

;; Layer A of workspaces-persistence-robustness: the serialized
;; window-state must be read-able by construction.  A live Emacs object
;; (buffer/marker/...) must never reach `prin1' output, because a single
;; `#<…>' token makes the whole on-disk `.eld' unreadable.  These specs
;; exercise the window-parameter translators (window-preserved-size) and
;; the bookmark-record scrub in layouts.el.

(require 'buttercup)
(require 'cl-lib)
(require 'map)
(require 'bookmark)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../tabs.el"              dir))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir)))

(defun serialization-robustness-spec--readable-p (form)
  "Return non-nil if FORM round-trips through `prin1' → `read'.
Two-pronged: no `#<' token in the printed form AND `read' of the
printed form does not signal."
  (let ((printed (prin1-to-string form)))
    (and (not (string-match-p "#<" printed))
         (condition-case _err
             (progn (read printed) t)
           (error nil)))))

(describe "workspace--unreadable-object-p"
  (it "flags live Emacs objects read cannot reconstruct"
    (with-temp-buffer
      (expect (workspace--unreadable-object-p (current-buffer)) :to-be-truthy)
      (expect (workspace--unreadable-object-p (point-marker)) :to-be-truthy))
    (expect (workspace--unreadable-object-p (selected-window)) :to-be-truthy)
    (expect (workspace--unreadable-object-p (selected-frame)) :to-be-truthy)
    (expect (workspace--unreadable-object-p (lambda () nil)) :to-be-truthy))

  (it "does not flag readable values, including named-function symbols"
    (expect (workspace--unreadable-object-p "string") :to-be nil)
    (expect (workspace--unreadable-object-p 42) :to-be nil)
    (expect (workspace--unreadable-object-p 'car) :to-be nil) ; functionp symbol
    (expect (workspace--unreadable-object-p '(a b c)) :to-be nil)
    (expect (workspace--unreadable-object-p nil) :to-be nil)))

(describe "window-preserved-size translator (serialize readability)"
  (it "(a) killed buffer in window-preserved-size — no #<, read succeeds, value is the stale NAME"
    ;; Regression: window-state-get '…writable emits the buffer object
    ;; in window-preserved-size verbatim; once killed it prints as
    ;; #<killed buffer>, corrupting the whole save.
    (let* ((buf (get-buffer-create "ws-killed-test"))
           (win (selected-window)))
      (set-window-buffer win buf)
      (with-selected-window win (window-preserve-size win t t))
      (let* ((state (let ((window-persistent-parameters
                           (append workspace-window-persistent-parameters
                                   window-persistent-parameters)))
                      (window-state-get win 'writable))))
        (kill-buffer buf)
        ;; The captured state still references the (now dead) buffer
        ;; object inside window-preserved-size.  Serialize it.
        (let ((serialized (workspace--window-state-serialize state)))
          (expect (serialization-robustness-spec--readable-p serialized)
                  :to-be-truthy)
          ;; The translated value carries the stale buffer NAME string.
          (expect (string-match-p "ws-killed-test"
                                  (prin1-to-string serialized))
                  :to-be-truthy)))))

  (it "(b) live preserved-size window round-trips to a name string"
    (with-temp-buffer
      (rename-buffer " *ws-live-preserved*" t)
      (let* ((bname (buffer-name))
             (state `(leaf (buffer ,bname . ((point . 1)))
                           (parameters
                            (window-preserved-size
                             . (,(current-buffer) t 80))))))
        (let* ((serialized (workspace--window-state-serialize state))
               (leaf-pos (cl-position 'leaf serialized))
               (attrs (cdr (cl-subseq serialized leaf-pos)))
               (params (map-elt attrs 'parameters))
               (preserved (map-elt params 'window-preserved-size)))
          (expect (serialization-robustness-spec--readable-p serialized)
                  :to-be-truthy)
          (expect (car preserved) :to-equal bname)
          (expect (nth 1 preserved) :to-be t)
          (expect (nth 2 preserved) :to-equal 80)))))

  (it "(c) a narrowed buffer serializes readably"
    (with-temp-buffer
      (rename-buffer " *ws-narrowed*" t)
      (insert "one\ntwo\nthree\n")
      (narrow-to-region (point-min) 4)
      (let* ((bname (buffer-name))
             (state `(leaf (buffer ,bname . ((point . 1))) (parameters))))
        (expect (serialization-robustness-spec--readable-p
                 (workspace--window-state-serialize state))
                :to-be-truthy))))

  (it "(d) an indirect buffer serializes readably"
    (with-temp-buffer
      (rename-buffer " *ws-base*" t)
      (insert "base content\n")
      (let* ((indirect (make-indirect-buffer (current-buffer)
                                             " *ws-indirect*" t)))
        (unwind-protect
            (let* ((bname (buffer-name indirect))
                   (state `(leaf (buffer ,bname . ((point . 1)))
                                 (parameters))))
              (expect (serialization-robustness-spec--readable-p
                       (workspace--window-state-serialize state))
                      :to-be-truthy))
          (kill-buffer indirect)))))

  (it "(e) a special/non-file buffer (*scratch*) serializes readably"
    (let* ((bname (buffer-name (get-buffer-create "*scratch*")))
           (state `(leaf (buffer ,bname . ((point . 1))) (parameters))))
      (expect (serialization-robustness-spec--readable-p
               (workspace--window-state-serialize state))
              :to-be-truthy))))

(describe "bookmark-record scrub (workspace--serialize-buffer)"
  (it "(f) nulls an unreadable value embedded in a bookmark record, keeps keys, no #<"
    (with-temp-buffer
      (rename-buffer " *ws-bookmark-scrub*" t)
      (let ((marker (point-marker))
            (live-buf (current-buffer)))
        ;; Stub bookmark-make-record to return a record embedding a
        ;; marker and a buffer alongside readable props.
        (spy-on 'bookmark-make-record :and-return-value
                `("ws-bookmark-scrub"
                  (filename . "/tmp/ws-scrub")
                  (front-context-string . "abc")
                  (a-marker . ,marker)
                  (a-buffer . ,live-buf)
                  (handler . some-handler-symbol)))
        (let* ((wb (workspace--serialize-buffer live-buf))
               (record (workspace-buffer-bookmark wb)))
          (expect (serialization-robustness-spec--readable-p record)
                  :to-be-truthy)
          ;; Keys preserved; only offending VALUES nulled.
          (expect (bookmark-prop-get record 'a-marker) :to-be nil)
          (expect (bookmark-prop-get record 'a-buffer) :to-be nil)
          ;; Readable props survive, including a function SYMBOL (readable).
          (expect (bookmark-prop-get record 'filename)
                  :to-equal "/tmp/ws-scrub")
          (expect (bookmark-prop-get record 'front-context-string)
                  :to-equal "abc")
          (expect (bookmark-prop-get record 'handler)
                  :to-equal 'some-handler-symbol)
          ;; The keys themselves are still present (nulled, not removed).
          (expect (assq 'a-marker (cdr record)) :to-be-truthy)
          (expect (assq 'a-buffer (cdr record)) :to-be-truthy))))))

(describe "window-preserved-size translator (deserialize round-trip)"
  (it "resolves a serialized NAME back to the live buffer"
    (with-temp-buffer
      (rename-buffer " *ws-deser-live*" t)
      (let* ((bname (buffer-name))
             (live-buf (current-buffer))
             (state `(leaf (buffer ,bname . ((point . 1)))
                           (parameters
                            (window-preserved-size . (,bname t 80))))))
        (let* ((restored (workspace--window-state-deserialize state))
               (leaf-pos (cl-position 'leaf restored))
               (attrs (cdr (cl-subseq restored leaf-pos)))
               (params (map-elt attrs 'parameters))
               (preserved (map-elt params 'window-preserved-size)))
          (expect (car preserved) :to-equal live-buf)
          (expect (nth 1 preserved) :to-be t)
          (expect (nth 2 preserved) :to-equal 80)))))

  (it "yields nil buffer when the serialized NAME is absent from the session"
    (let* ((state `(leaf (buffer "GHOST" . ((point . 1)))
                         (parameters
                          (window-preserved-size
                           . ("ws-no-such-buffer-xyz" t 80))))))
      (let* ((restored (workspace--window-state-deserialize state))
             (leaf-pos (cl-position 'leaf restored))
             (attrs (cdr (cl-subseq restored leaf-pos)))
             (params (map-elt attrs 'parameters))
             (preserved (map-elt params 'window-preserved-size)))
        (expect (car preserved) :to-be nil))))

  (it "drops the parameter (without aborting) when its deserializer throws"
    ;; Mirror activities' restore robustness: a translator that errors
    ;; drops just that param, leaving the rest of the leaf intact.
    ;; `workspace--window-state-deserialize' guards the per-param
    ;; deserialize with `condition-case-unless-debug', which re-raises
    ;; when `debug-on-error' is set (buttercup's batch default).  Bind
    ;; it off so the guard catches, exercising the production drop path.
    (let ((debug-on-error nil)
          (workspace-window-parameter-translators
           `((window-preserved-size
              (serialize . ,#'identity)
              (deserialize . ,(lambda (_v) (error "boom")))))))
      (let* ((state `(leaf (buffer "GHOST" . ((point . 1)))
                           (parameters
                            (window-preserved-size . ("x" t 80))
                            (window-side . left))))
             (restored (workspace--window-state-deserialize state))
             (leaf-pos (cl-position 'leaf restored))
             (attrs (cdr (cl-subseq restored leaf-pos)))
             (params (map-elt attrs 'parameters)))
        ;; The throwing param is gone; the sibling param survives.
        (expect (map-contains-key params 'window-preserved-size) :to-be nil)
        (expect (map-elt params 'window-side) :to-equal 'left)))))

;; ---------------------------------------------------------------------------
;; Layer B: corruption-safe persistence I/O
;;
;; These specs exercise the write/read boundary in persistence.el:
;; atomic write, write-time readable assert, backup-on-corrupt read,
;; absent-vs-unreadable signalling, and the session-wide autosave gate.
;; Every spec sandboxes I/O via `workspace-state-directory-override' so
;; nothing touches the developer's real state file.
;; ---------------------------------------------------------------------------

(defvar serialization-robustness-spec--state-dir nil
  "Per-test sandbox dir backing `workspace--state-file'.")

(defun serialization-robustness-spec--state-file ()
  "Absolute path of the sandboxed state file for the current test."
  (expand-file-name "workspaces.eld" serialization-robustness-spec--state-dir))

(defun serialization-robustness-spec--corrupt-siblings ()
  "Return the list of `workspaces.eld.corrupt-*' backups in the sandbox."
  (directory-files serialization-robustness-spec--state-dir nil
                   "\\`workspaces\\.eld\\.corrupt-"))

(defun serialization-robustness-spec--tmp-siblings ()
  "Return the list of leftover `workspaces.eld.tmp-*' temp files."
  (directory-files serialization-robustness-spec--state-dir nil
                   "\\`workspaces\\.eld\\.tmp-"))

(describe "Layer B persistence I/O"
  (before-each
    (setq serialization-robustness-spec--state-dir
          (file-name-as-directory (make-temp-file "ws-robust-io-" t))
          workspace-state-directory-override
          serialization-robustness-spec--state-dir
          workspace--persistence-blocked nil
          workspace--persistence-blocked-warned nil)
    (clrhash workspace--registry))

  (after-each
    (setq workspace-state-directory-override nil
          workspace--persistence-blocked nil
          workspace--persistence-blocked-warned nil)
    (when (and serialization-robustness-spec--state-dir
               (file-directory-p serialization-robustness-spec--state-dir))
      (delete-directory serialization-robustness-spec--state-dir t))
    (clrhash workspace--registry))

  (describe "corruption-injection / no-clobber"
    (it "backs up the corrupt file, blocks, returns the sentinel, leaves the registry empty"
      (let ((file (serialization-robustness-spec--state-file)))
        ;; Write a literally-unreadable file: a `#<…>' token aborts `read'.
        (make-directory serialization-robustness-spec--state-dir t)
        (with-temp-file file
          (insert "(:version 3 :workspaces (#<killed buffer>))"))
        (let ((result (workspace--read-state)))
          ;; (c) the sentinel — distinct from nil (absent).
          (expect result :to-be 'workspace--unreadable)
          ;; (b) the session block flag is set.
          (expect workspace--persistence-blocked :to-be t)
          ;; (a) the original no longer holds the corrupt bytes, and a
          ;; corrupt-* sibling preserves them.
          (expect (file-exists-p file) :to-be nil)
          (let ((backups (serialization-robustness-spec--corrupt-siblings)))
            (expect (length backups) :to-equal 1)
            (let ((bytes (with-temp-buffer
                           (insert-file-contents-literally
                            (expand-file-name
                             (car backups)
                             serialization-robustness-spec--state-dir))
                           (buffer-string))))
              (expect (string-match-p "#<killed buffer>" bytes)
                      :to-be-truthy))))
        ;; (d) restore deserializes nothing — the registry stays empty.
        (workspace--restore)
        (expect (hash-table-count workspace--registry) :to-equal 0))))

  (describe "end-to-end clobber-cascade regression"
    (it "a flush after a corrupt load never overwrites the backup with an empty registry"
      ;; The original cascade: corrupt file → read returns nil (looked
      ;; absent) → autosave serialises an empty registry over it. Prove
      ;; it is dead: after restore + a flush, the corrupt bytes survive in
      ;; the backup and NO live state file is written.
      (let ((file (serialization-robustness-spec--state-file)))
        (make-directory serialization-robustness-spec--state-dir t)
        (with-temp-file file
          (insert "(:version 3 :workspaces (#<window 1>))"))
        (workspace--restore)
        (expect workspace--persistence-blocked :to-be t)
        ;; Fire every disk-writing path that the old cascade rode in on.
        ;; All flush synchronously now; the block must still suppress them.
        (workspace--flush-state)
        (workspace--kill-emacs-flush)
        ;; No live state file was (re)written...
        (expect (file-exists-p file) :to-be nil)
        ;; ...and the corrupt original is still preserved verbatim.
        (let ((backups (serialization-robustness-spec--corrupt-siblings)))
          (expect (length backups) :to-equal 1)
          (let ((bytes (with-temp-buffer
                         (insert-file-contents-literally
                          (expand-file-name
                           (car backups)
                           serialization-robustness-spec--state-dir))
                         (buffer-string))))
            (expect bytes :to-equal "(:version 3 :workspaces (#<window 1>))"))))))

  (describe "autosave gate (blocked → no writes)"
    (it "flush and kill-emacs-flush write nothing while blocked"
      (setq workspace--persistence-blocked t)
      (let ((file (serialization-robustness-spec--state-file)))
        (expect (file-exists-p file) :to-be nil)
        (workspace--flush-state)
        (workspace--kill-emacs-flush)
        ;; Neither path created the file as `(:version 3 :workspaces nil)'.
        (expect (file-exists-p file) :to-be nil)
        (expect (serialization-robustness-spec--tmp-siblings) :to-equal nil)))

    (it "warns at most once across many suppressed writes"
      (setq workspace--persistence-blocked t)
      (let ((warn-count 0))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest _) (cl-incf warn-count))))
          (workspace--write-state '(:version 3 :workspaces nil))
          (workspace--write-state '(:version 3 :workspaces nil))
          (workspace--write-state '(:version 3 :workspaces nil)))
        (expect warn-count :to-equal 1))))

  (describe "write-time readable assert"
    (it "refuses a form carrying a raw buffer object and leaves a prior file intact"
      (let ((file (serialization-robustness-spec--state-file)))
        ;; Lay down a known-good prior file via the normal write path.
        (workspace--write-state '(:version 3 :workspaces nil))
        (expect (file-exists-p file) :to-be-truthy)
        (let ((good-bytes (with-temp-buffer
                            (insert-file-contents-literally file)
                            (buffer-string))))
          ;; Now attempt to write an unreadable form (a live buffer).
          (with-temp-buffer
            (let ((warned nil))
              (cl-letf (((symbol-function 'display-warning)
                         (lambda (&rest _) (setq warned t))))
                (workspace--write-state
                 `(:version 3 :workspaces (,(current-buffer)))))
              (expect warned :to-be-truthy)))
          ;; The prior good file is untouched (byte-for-byte).
          (let ((after-bytes (with-temp-buffer
                               (insert-file-contents-literally file)
                               (buffer-string))))
            (expect after-bytes :to-equal good-bytes))
          ;; And the assert is not the same thing as the block: a refused
          ;; write must NOT poison the session flag.
          (expect workspace--persistence-blocked :to-be nil)
          ;; No temp file leaked from the aborted write.
          (expect (serialization-robustness-spec--tmp-siblings) :to-equal nil)))))

  (describe "atomic write happy path"
    (it "writes via temp+rename, leaves no temp sibling, and reads back equal"
      (let ((file (serialization-robustness-spec--state-file))
            (form '(:version 3 :workspaces ((:name "alpha" :home "/tmp/alpha/")))))
        (workspace--write-state form)
        (expect (file-exists-p file) :to-be-truthy)
        ;; No `workspaces.eld.tmp-*' detritus.
        (expect (serialization-robustness-spec--tmp-siblings) :to-equal nil)
        ;; The on-disk form reads back identical (register/shape/
        ;; workspace-plist-v3: temp+rename changes HOW, not WHAT).
        (let ((read-back (with-temp-buffer
                           (insert-file-contents file)
                           (read (current-buffer)))))
          (expect read-back :to-equal form)))))

  (describe "absent → fresh start"
    (it "returns nil, stays unblocked, and a subsequent save writes normally"
      (let ((file (serialization-robustness-spec--state-file)))
        (expect (file-exists-p file) :to-be nil)
        (expect (workspace--read-state) :to-be nil)
        (expect workspace--persistence-blocked :to-be nil)
        ;; A fresh-start save is permitted and writes the file.
        (workspace--write-state '(:version 3 :workspaces nil))
        (expect (file-exists-p file) :to-be-truthy)
        (expect (workspace--read-state) :to-equal '(:version 3 :workspaces nil))))))

(provide 'serialization-robustness-spec)
;;; serialization-robustness-spec.el ends here
