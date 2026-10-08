;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; pdf-tools: render PDFs with a native server
;; ===============================================================================

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :init
  (let ((dir (expand-file-name "packages/pdf-tools/" user-emacs-directory)))
    (make-directory dir t)
    (setq pdf-info-epdfinfo-program (expand-file-name "epdfinfo" dir)))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-use-scaling t)
  :config
  (pdf-tools-install :no-query))

(defun jf/pdf-view-disable-line-numbers ()
  "Turn off line numbers, which `pdf-view-mode' cannot display."
  (display-line-numbers-mode -1))

(add-hook 'pdf-view-mode-hook #'jf/pdf-view-disable-line-numbers)

(with-eval-after-load 'pdf-view
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'pdf)))

(declare-function straight--build-dir "straight")
(defvar straight-byte-compilation-buffer)

(defun jf/org-noter-compile-with-pdf-tools ()
  "Byte-compile org-noter in a subprocess with this session's load path.
Like straight's compile step, but the load path includes pdf-tools,
whose macros `org-noter-pdf' uses without declaring the dependency."
  (let ((dir (straight--build-dir "org-noter")))
    (call-process (expand-file-name invocation-name invocation-directory)
                  nil straight-byte-compilation-buffer nil
                  "-Q" "--batch" "--eval"
                  (format "%S" `(progn (setq load-path ',load-path)
                                       (byte-recompile-directory ,dir 0 'force))))))

;; ===============================================================================
;; org-noter: page-linked notes beside a PDF
;; ===============================================================================

(use-package org-noter
  :straight (org-noter :build (:not compile)
                       :post-build (jf/org-noter-compile-with-pdf-tools))
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-notes-window-location 'horizontal-split)
  (org-noter-auto-save-last-location t)
  (org-noter-separate-notes-from-heading t)
  (org-noter-highlight-selected-text t))

(defvar pdf-view-active-region)
(declare-function pdf-view-deactivate-region "pdf-view")
(declare-function org-noter--session-doc-buffer "org-noter-core")

(defun jf/pdf-view-empty-region-p (region)
  "Return non-nil when REGION, a `pdf-view-active-region', has no edges."
  (and (consp region) (null (cdr region))))

(defun jf/org-noter-drop-empty-region (&rest _)
  "Deactivate a click-only region in the current org-noter session's PDF."
  (when-let* ((session (bound-and-true-p org-noter--session))
              (doc (org-noter--session-doc-buffer session))
              ((buffer-live-p doc)))
    (with-current-buffer doc
      (when (and (derived-mode-p 'pdf-view-mode)
                 (jf/pdf-view-empty-region-p pdf-view-active-region))
        (pdf-view-deactivate-region)))))

(with-eval-after-load 'org-noter
  (dolist (command '(org-noter-insert-note org-noter-insert-precise-note))
    (advice-add command :before #'jf/org-noter-drop-empty-region)))

(with-eval-after-load 'org-noter
  (with-eval-after-load 'evil
    (evil-define-minor-mode-key 'normal 'org-noter-doc-mode
      "i" #'org-noter-insert-note
      "I" #'org-noter-insert-precise-note
      "q" #'org-noter-kill-session)))

(defcustom jf/pdf-books
  '(("ep" . "~/Documents/Effective Python - Third Edition.pdf"))
  "Alist of (KEY . FILE) naming PDF books for `book:' links."
  :type '(alist :key-type string :value-type file)
  :group 'pdf-tools)

(declare-function pdf-info-outline "pdf-info")
(declare-function pdf-view-goto-page "pdf-view")

(defun jf/pdf-outline-page (outline query)
  "Return the page of the first OUTLINE entry whose title starts with QUERY.
The match ignores case, and QUERY must end where a word ends.
OUTLINE is a list of alists as returned by `pdf-info-outline'."
  (let ((case-fold-search t)
        (re (concat "\\`" (regexp-quote query) "\\([^[:alnum:]]\\|\\'\\)")))
    (alist-get 'page
               (seq-find (lambda (entry)
                           (string-match-p re (alist-get 'title entry)))
                         outline))))

(defun jf/pdf-book-parse-link (path)
  "Split book link PATH \"KEY/TARGET\" into (KEY . TARGET).
TARGET is nil when PATH has no slash."
  (if (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" path)
      (cons (match-string 1 path) (match-string 2 path))
    (cons path nil)))

(defun jf/pdf-outline-short-title (title)
  "Return TITLE up to its first colon, e.g. \"Item 21\" for an item title."
  (string-trim (car (split-string title ":"))))

(defun jf/pdf-book-file (key)
  "Return the absolute PDF file for book KEY."
  (let ((file (cdr (assoc key jf/pdf-books))))
    (unless file
      (user-error "No book %S in `jf/pdf-books'" key))
    (expand-file-name file)))

(defun jf/pdf-book-read-key ()
  "Read a book key, without asking when only one book is defined."
  (if (length= jf/pdf-books 1)
      (caar jf/pdf-books)
    (completing-read "Book: " jf/pdf-books nil t)))

(defun jf/pdf-book-outline (key)
  "Return the outline of book KEY, starting pdf-tools if needed."
  (require 'pdf-tools)
  (pdf-info-outline (jf/pdf-book-file key)))

(defun jf/pdf-book-goto (key &optional target)
  "Show book KEY at TARGET, an outline title prefix or a page number."
  (let* ((page (cond ((null target) nil)
                     ((string-match-p "\\`[0-9]+\\'" target)
                      (string-to-number target))
                     ((jf/pdf-outline-page (jf/pdf-book-outline key) target))
                     (t (user-error "No outline entry in %s starts with %S"
                                    key target))))
         (buffer (find-file-noselect (jf/pdf-book-file key)))
         (window (get-buffer-window buffer 'visible)))
    (if window
        (select-frame-set-input-focus (window-frame (select-window window)))
      (pop-to-buffer buffer))
    (when page
      (pdf-view-goto-page page))))

(defun jf/pdf-book-read-entry (key)
  "Read an outline title from book KEY with completion."
  (completing-read (format "%s: " key)
                   (mapcar (lambda (entry) (alist-get 'title entry))
                           (jf/pdf-book-outline key))
                   nil t))

(defun jf/pdf-book-open (key title)
  "Open book KEY at the outline entry TITLE.
Interactively, read both with completion."
  (interactive
   (let ((key (jf/pdf-book-read-key)))
     (list key (jf/pdf-book-read-entry key))))
  (jf/pdf-book-goto key title))

(defun jf/pdf-book-link-follow (path _arg)
  "Follow a book link to PATH, of the form KEY/TARGET."
  (pcase-let ((`(,key . ,target) (jf/pdf-book-parse-link path)))
    (jf/pdf-book-goto key target)))

(defun jf/pdf-book-link-complete (&optional _arg)
  "Return a book link to an outline entry read with completion."
  (let ((key (jf/pdf-book-read-key)))
    (format "book:%s/%s" key
            (jf/pdf-outline-short-title (jf/pdf-book-read-entry key)))))

(defun jf/pdf-book-insert-link ()
  "Insert a book link to an outline entry read with completion."
  (interactive)
  (let ((link (jf/pdf-book-link-complete)))
    (insert (org-link-make-string
             link (cdr (jf/pdf-book-parse-link (string-remove-prefix "book:" link)))))))

(with-eval-after-load 'ol
  (org-link-set-parameters "book"
                           :follow #'jf/pdf-book-link-follow
                           :complete #'jf/pdf-book-link-complete))
