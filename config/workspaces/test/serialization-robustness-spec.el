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
  (load (expand-file-name "../layouts.el"           dir)))

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

(provide 'serialization-robustness-spec)
;;; serialization-robustness-spec.el ends here
