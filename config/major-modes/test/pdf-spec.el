;;; pdf-spec.el --- Buttercup tests for PDF book links -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Exercises the book helpers in config/major-modes/pdf.el: resolving
;; an outline title prefix to a page, parsing `book:' link paths, and
;; routing `jf/pdf-book-goto' to the right page.  pdf-tools itself is
;; mocked at its boundary (`jf/pdf-book-outline', buffer display and
;; `pdf-view-goto-page'), since rendering needs a window system.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(unless (fboundp 'jf/pdf-outline-page)
  (load (expand-file-name
         "../pdf.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(defconst jf/pdf-spec-outline
  '(((depth . 1) (title . "Chapter 1 Pythonic Thinking") (page . 28))
    ((depth . 2) (title . "Item 2: Follow the PEP 8 Style Guide") (page . 30))
    ((depth . 1) (title . "Chapter 3 Loops and Iterators") (page . 104))
    ((depth . 2) (title . "Item 21: Be Defensive when Iterating over Arguments")
     (page . 114)))
  "A slice of the Effective Python outline, as `pdf-info-outline' returns it.")

(describe "jf/pdf-outline-page"
  (it "returns the page of the entry whose title starts with the query"
    (expect (jf/pdf-outline-page jf/pdf-spec-outline "Item 21") :to-equal 114))

  (it "does not let a shorter number match a longer one"
    (expect (jf/pdf-outline-page jf/pdf-spec-outline "Item 2") :to-equal 30))

  (it "ignores case"
    (expect (jf/pdf-outline-page jf/pdf-spec-outline "chapter 3") :to-equal 104))

  (it "matches a whole title"
    (expect (jf/pdf-outline-page jf/pdf-spec-outline
                                 "Item 2: Follow the PEP 8 Style Guide")
            :to-equal 30))

  (it "returns nil when no title starts with the query"
    (expect (jf/pdf-outline-page jf/pdf-spec-outline "Item 9") :to-be nil)))

(describe "jf/pdf-book-parse-link"
  (it "splits KEY/TARGET at the first slash"
    (expect (jf/pdf-book-parse-link "ep/Item 21") :to-equal '("ep" . "Item 21")))

  (it "keeps later slashes in the target"
    (expect (jf/pdf-book-parse-link "ep/a/b") :to-equal '("ep" . "a/b")))

  (it "returns a nil target for a bare key"
    (expect (jf/pdf-book-parse-link "ep") :to-equal '("ep"))))

(describe "jf/pdf-outline-short-title"
  (it "keeps the text before the first colon"
    (expect (jf/pdf-outline-short-title
             "Item 21: Be Defensive when Iterating over Arguments")
            :to-equal "Item 21"))

  (it "keeps a title without a colon whole"
    (expect (jf/pdf-outline-short-title "Chapter 3 Loops and Iterators")
            :to-equal "Chapter 3 Loops and Iterators")))

(describe "jf/pdf-book-goto"
  :var (goto-page)

  (before-each
    (setq goto-page :not-called))

  ;; The book buffer is a fresh one that no window shows, so the real
  ;; `get-buffer-window' returns nil.  (Mocking that primitive would
  ;; make Emacs native-compile a trampoline.)
  (cl-flet ((goto (target)
              (let ((jf/pdf-books '(("ep" . "/books/ep.pdf")))
                    (book (generate-new-buffer " *pdf-spec book*")))
                (unwind-protect
                    (cl-letf (((symbol-function 'jf/pdf-book-outline)
                               (lambda (_key) jf/pdf-spec-outline))
                              ((symbol-function 'find-file-noselect)
                               (lambda (_file) book))
                              ((symbol-function 'pop-to-buffer) #'ignore)
                              ((symbol-function 'pdf-view-goto-page)
                               (lambda (page) (setq goto-page page))))
                      (jf/pdf-book-goto "ep" target))
                  (kill-buffer book)))))

    (it "goes to the page of an outline entry"
      (goto "Item 21")
      (expect goto-page :to-equal 114))

    (it "treats a number as a page"
      (goto "212")
      (expect goto-page :to-equal 212))

    (it "opens the book without moving when there is no target"
      (goto nil)
      (expect goto-page :to-be :not-called))

    (it "signals a user error for an unknown entry"
      (expect (goto "Item 9") :to-throw 'user-error))

    (it "signals a user error for an unknown book"
      (expect (let ((jf/pdf-books nil)) (jf/pdf-book-goto "nope" "1"))
              :to-throw 'user-error))))

(describe "jf/pdf-view-empty-region-p"
  (it "is true for a click-only region, (PAGE)"
    (expect (jf/pdf-view-empty-region-p '(129)) :to-be-truthy))

  (it "is false for a region with edges"
    (expect (jf/pdf-view-empty-region-p '(129 (0.1 0.2 0.3 0.4))) :not :to-be-truthy))

  (it "is false for no region"
    (expect (jf/pdf-view-empty-region-p nil) :not :to-be-truthy)))

(require 'org-noter)

(describe "jf/org-noter-drop-empty-region"
  :var (doc deactivated)

  (before-each
    (setq deactivated nil
          doc (generate-new-buffer " *pdf-spec doc*"))
    (with-current-buffer doc
      (setq-local major-mode 'pdf-view-mode)))

  (after-each
    (kill-buffer doc))

  (cl-flet ((drop (region)
              (with-current-buffer doc
                (setq-local pdf-view-active-region region))
              ;; A real session struct: loading inlines its accessors,
              ;; so they cannot be mocked.
              (with-temp-buffer
                (setq-local org-noter--session
                            (make-org-noter--session :doc-buffer doc))
                (cl-letf (((symbol-function 'pdf-view-deactivate-region)
                           (lambda () (setq deactivated (current-buffer)))))
                  (jf/org-noter-drop-empty-region)))))

    (it "deactivates a click-only region in the session's PDF"
      (drop '(129))
      (expect deactivated :to-be doc))

    (it "keeps a real text selection"
      (drop '(129 (0.1 0.2 0.3 0.4)))
      (expect deactivated :to-be nil))

    (it "does nothing outside an org-noter session"
      (with-current-buffer doc
        (setq-local pdf-view-active-region '(129)))
      (with-temp-buffer
        (jf/org-noter-drop-empty-region))
      (expect deactivated :to-be nil))))

(provide 'pdf-spec)

;;; pdf-spec.el ends here
