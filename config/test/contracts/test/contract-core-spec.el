;;; contract-core-spec.el --- Tests for contract validation primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for contract-core.el validation primitives.

;;; Code:

(require 'buttercup)

;; Add contracts directory to load-path
(let ((contracts-dir (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path contracts-dir))

(require 'contract-core)

;; Register matcher after buttercup is loaded
(contract--register-buttercup-matcher)

(describe "contract--validate-plist"
  (it "returns nil for valid plist with all required keys"
    (let ((data '(:name "test" :count 5 :active t)))
      (expect (contract--validate-plist data
                '((:name stringp) (:count integerp) (:active booleanp)))
              :to-be nil)))

  (it "returns error for missing required key"
    (let ((data '(:name "test")))
      (expect (contract--validate-plist data
                '((:name stringp) (:count integerp)))
              :to-match "Missing required key :count")))

  (it "returns error when value fails predicate"
    (let ((data '(:name 42)))
      (expect (contract--validate-plist data
                '((:name stringp)))
              :to-match "does not satisfy")))

  (it "accepts valid optional keys"
    (let ((data '(:name "test" :extra "bonus")))
      (expect (contract--validate-plist data
                '((:name stringp))
                '((:extra stringp)))
              :to-be nil)))

  (it "ignores absent optional keys"
    (let ((data '(:name "test")))
      (expect (contract--validate-plist data
                '((:name stringp))
                '((:extra stringp)))
              :to-be nil)))

  (it "validates optional key type when present"
    (let ((data '(:name "test" :extra 42)))
      (expect (contract--validate-plist data
                '((:name stringp))
                '((:extra stringp)))
              :to-match "Optional key :extra")))

  (it "returns error for non-list input"
    (expect (contract--validate-plist "not a plist"
              '((:name stringp)))
            :to-match "Expected plist"))

  (it "collects multiple errors"
    (let ((data '(:name 42)))
      (expect (contract--validate-plist data
                '((:name stringp) (:count integerp)))
              :to-match "Missing required key :count"))))

(describe "contract--validate-alist"
  (it "returns nil for valid alist"
    (let ((data '((:a . 1) (:b . 2))))
      (expect (contract--validate-alist data '(:a :b))
              :to-be nil)))

  (it "returns error for unknown key"
    (let ((data '((:a . 1) (:c . 3))))
      (expect (contract--validate-alist data '(:a :b))
              :to-match "Unknown alist key :c")))

  (it "catches plist masquerading as alist"
    (let ((data '(:a 1 :b 2)))  ; This is a plist, not alist
      (expect (contract--validate-alist data '(:a :b))
              :to-match "not a cons cell")))

  (it "validates value predicate"
    (let ((data '((:a . "not a list"))))
      (expect (contract--validate-alist data '(:a) #'listp)
              :to-match "does not satisfy")))

  (it "returns nil for empty alist"
    (expect (contract--validate-alist nil '(:a :b))
            :to-be nil))

  (it "returns error for non-list input"
    (expect (contract--validate-alist "not an alist" '(:a))
            :to-match "Expected alist")))

(describe "contract--validate-plist-in-list"
  (it "returns nil for valid list"
    (expect (contract--validate-plist-in-list
             '(1 2 3)
             (lambda (x) (if (integerp x) nil "not int")))
            :to-be nil))

  (it "returns error for invalid item"
    (expect (contract--validate-plist-in-list
             '(1 "bad" 3)
             (lambda (x) (if (integerp x) nil "not int")))
            :to-match "Item 1: not int"))

  (it "returns nil for empty list"
    (expect (contract--validate-plist-in-list
             nil
             (lambda (_x) "always fails"))
            :to-be nil)))

(describe ":to-satisfy-contract matcher"
  (it "passes when contract is satisfied"
    (let ((data '(:name "test" :count 5)))
      (expect data :to-satisfy-contract
              (lambda (d)
                (contract--validate-plist d
                  '((:name stringp) (:count integerp)))))))

  (it "fails when contract is violated"
    ;; Directly test the validator returns an error string
    (let ((error (contract--validate-plist '(:name 42) '((:name stringp)))))
      (expect error :to-match "does not satisfy"))))

(provide 'contract-core-spec)

;;; contract-core-spec.el ends here
