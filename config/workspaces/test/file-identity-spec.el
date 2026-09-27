;;; file-identity-spec.el --- Tests for the core file-identity predicate -*- lexical-binding: t; -*-

;; `workspace--same-file-p' (data-model.el) is the ONE predicate that
;; decides whether two paths name the same file or directory anywhere in
;; config/workspaces/ (register/invariant/directory-identity-by-device-inode).
;; It compares device and inode numbers of the two truenames.
;;
;; These specs pin:
;;   - its contract: spelling-insensitive, nil for a non-string, a remote
;;     name (with no filesystem call) and a missing path, never signals;
;;   - the reason it exists: a directory written to between the two reads
;;     of one comparison still compares equal to ITSELF.  `file-equal-p'
;;     compares whole attribute lists, timestamps included, and reports it
;;     unequal; each race spec shows that as a control;
;;   - the two core call sites routed to it, each under the same race:
;;     `workspace--registered-for-home-p' (tabs.el) and
;;     `jf/workspace--worktree-children' (git-worktrees.el).

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(defvar file-identity-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(let ((dir file-identity-spec--this-dir))
  (load (expand-file-name "../data-model.el"   dir))
  (load (expand-file-name "../integrations.el" dir))
  (load (expand-file-name "../home-org.el"     dir))
  (load (expand-file-name "../scaffold.el"     dir))
  (load (expand-file-name "../tabs.el"         dir))
  (load (expand-file-name "../git-worktrees.el" dir))
  ;; The one registry-scope construct
  ;; (register/invariant/spec-registry-isolation-via-fixtures).
  (load (expand-file-name "portable-bookmarks-fixtures.el" dir)))

(defvar file-identity-spec--root nil
  "This spec's own temp directory, created and deleted per spec.")

(defun file-identity-spec--make-root ()
  "Create this spec's temp directory, spelled as `make-temp-file' returns it.
On macOS that is a /var/... name whose truename is /private/var/..., so
the returned spelling is deliberately NOT the truename."
  (setq file-identity-spec--root
        (file-name-as-directory (make-temp-file "wsid-" t))))

(defun file-identity-spec--delete-root ()
  "Delete this spec's own temp directory, and only it."
  (when (and file-identity-spec--root
             (file-directory-p file-identity-spec--root))
    (delete-directory file-identity-spec--root t))
  (setq file-identity-spec--root nil))

(defmacro file-identity-spec--with-timestamp-drift (&rest body)
  "Run BODY while every `file-attributes' read reports later times.
A deterministic model of a concurrent writer (or reader: on APFS the
first read of a directory after it changes updates its access time):
each call reports access, modification and status-change times one
second later than the call before.  Device and inode never change,
because the files do not.  Evaluates to BODY's value."
  (declare (indent 0) (debug (&rest form)))
  `(let ((real-file-attributes (symbol-function 'file-attributes))
         (reads 0))
     (cl-letf (((symbol-function 'file-attributes)
                (lambda (file &optional id-format)
                  (let ((attrs (funcall real-file-attributes file id-format)))
                    (when attrs
                      (setq reads (1+ reads)
                            attrs (copy-sequence attrs))
                      (dolist (slot '(4 5 6))
                        (setf (nth slot attrs)
                              (time-add (nth slot attrs) reads))))
                    attrs))))
       ,@body)))

(defmacro file-identity-spec--with-write-between-reads (dir &rest body)
  "Run BODY while every `file-attributes' read is followed by a write into DIR.
The write is real: a new file is created inside DIR after each read, so
DIR's modification time moves between the two reads of one comparison.
This is the form that fails under `file-equal-p'.  Evaluates to BODY's
value."
  (declare (indent 1) (debug (form &rest form)))
  `(let ((real-file-attributes (symbol-function 'file-attributes))
         (target ,dir)
         (writes 0))
     (cl-letf (((symbol-function 'file-attributes)
                (lambda (file &optional id-format)
                  (prog1 (funcall real-file-attributes file id-format)
                    (setq writes (1+ writes))
                    (write-region "" nil
                                  (expand-file-name
                                   (format "wsid-write-%d" writes) target)
                                  nil 'silent)))))
       ,@body)))


;;; The predicate's contract

(describe "workspace--same-file-p"
  (before-each (file-identity-spec--make-root))
  (after-each (file-identity-spec--delete-root))

  (it "is true for one directory under different spellings"
    (let* ((root file-identity-spec--root)
           (dir (expand-file-name "d" root)))
      (make-directory dir)
      (expect (workspace--same-file-p dir dir) :to-be-truthy)
      (expect (workspace--same-file-p dir (file-name-as-directory dir))
              :to-be-truthy)
      (expect (workspace--same-file-p (file-truename dir) dir) :to-be-truthy)
      (expect (workspace--same-file-p
               (concat (file-name-as-directory dir) "../d") dir)
              :to-be-truthy)))

  (it "is true for a symbolic link and its target directory"
    (let* ((root file-identity-spec--root)
           (dir (expand-file-name "d" root))
           (link (expand-file-name "link" root)))
      (make-directory dir)
      (make-symbolic-link dir link)
      (expect (workspace--same-file-p link dir) :to-be-truthy)))

  (it "is nil for two different directories"
    (let* ((root file-identity-spec--root)
           (a (expand-file-name "a" root))
           (b (expand-file-name "b" root)))
      (make-directory a)
      (make-directory b)
      (expect (workspace--same-file-p a b) :to-be nil)))

  (it "is nil when either path does not exist"
    (let* ((root file-identity-spec--root)
           (missing (expand-file-name "missing" root)))
      (expect (workspace--same-file-p missing root) :to-be nil)
      (expect (workspace--same-file-p root missing) :to-be nil)
      (expect (workspace--same-file-p missing missing) :to-be nil)))

  (it "is nil for a non-string"
    (expect (workspace--same-file-p nil file-identity-spec--root) :to-be nil)
    (expect (workspace--same-file-p file-identity-spec--root nil) :to-be nil)
    (expect (workspace--same-file-p 42 42) :to-be nil))

  (it "is nil for a remote name and makes no filesystem call for it"
    (spy-on 'file-attributes :and-call-through)
    (spy-on 'file-truename :and-call-through)
    (let ((remote "/ssh:wsid-nohost.invalid:/tmp/"))
      (expect (workspace--same-file-p remote remote) :to-be nil)
      (expect (workspace--same-file-p remote file-identity-spec--root)
              :to-be nil)
      (expect (workspace--same-file-p file-identity-spec--root remote)
              :to-be nil))
    (expect 'file-attributes :not :to-have-been-called)
    (expect 'file-truename :not :to-have-been-called))

  (it "never signals, answering nil instead"
    (spy-on 'file-attributes :and-call-fake
            (lambda (&rest _) (error "Simulated stat failure")))
    (expect (workspace--same-file-p file-identity-spec--root
                                    file-identity-spec--root)
            :to-be nil))

  (it "is true for a directory whose timestamps change between the two reads"
    (let ((dir file-identity-spec--root))
      (file-identity-spec--with-timestamp-drift
        ;; Control: the drift model reproduces the defect.
        (expect (file-equal-p dir dir) :to-be nil)
        (expect (workspace--same-file-p dir dir) :to-be-truthy)
        (expect reads :to-be-greater-than 2))))

  (it "is true for a directory written to between the two reads"
    (let ((dir file-identity-spec--root))
      (file-identity-spec--with-write-between-reads dir
        (expect (file-equal-p dir dir) :to-be nil)
        (expect (workspace--same-file-p dir dir) :to-be-truthy)
        (expect writes :to-be-greater-than 2))))

  (it "never reports a directory unequal to itself under a real concurrent writer (bounded)"
    ;; A stress companion to the two specs above.  With device and inode it
    ;; cannot fail: no timestamp is compared.  Under `file-equal-p' the
    ;; portable-bookmarks review measured about one wrong answer in four.
    (let* ((dir (directory-file-name file-identity-spec--root))
           (default-directory file-identity-spec--root)
           (writer nil)
           (wrong 0)
           (calls 0)
           (deadline (time-add nil 2)))
      (unwind-protect
          (progn
            ;; An earlier spec file may leave `default-directory' naming a
            ;; directory it has since deleted; the subprocess starts in ours.
            (setq writer
                  (make-process
                   :name "wsid-writer" :buffer nil :noquery t
                   :command
                   (list "sh" "-c"
                         "while :; do : > \"$1/.#wsid-lock\"; rm -f \"$1/.#wsid-lock\"; done"
                         "sh" dir)))
            (while (and (< calls 1000) (time-less-p nil deadline))
              (setq calls (1+ calls))
              (unless (workspace--same-file-p dir (file-name-as-directory dir))
                (setq wrong (1+ wrong)))))
        (when (process-live-p writer)
          (delete-process writer))
        (with-timeout (5)
          (while (process-live-p writer)
            (accept-process-output nil 0.05))))
      (expect calls :to-be-greater-than 100)
      (expect wrong :to-equal 0))))


;;; Core site 1 — tabs.el, the duplicate-home guard

(describe "workspace--registered-for-home-p while the home is being written to"
  :var (home)

  (workspace-bookmark-fixtures-with-empty-registry
    (setq home (workspace-bookmark-fixtures-make-workspace "wsid-home")))

  (it "still finds the registered home under a different spelling"
    (expect (workspace--registered-for-home-p (directory-file-name home))
            :to-be-truthy))

  (it "does not report an unregistered directory"
    (let ((other (workspace-bookmark-fixtures-temp-dir "wsid-other-")))
      (expect (workspace--registered-for-home-p other) :to-be nil)))

  (it "still finds the registered home when it is written to between the two reads"
    (file-identity-spec--with-write-between-reads home
      (expect (workspace--registered-for-home-p (directory-file-name home))
              :to-be-truthy)
      (expect writes :to-be-greater-than 1))))


;;; Core site 2 — git-worktrees.el, a child's own worktree entry

(describe "jf/workspace--worktree-children while the child is being written to"
  (before-each (file-identity-spec--make-root))
  (after-each (file-identity-spec--delete-root))

  (it "still finds the child's own branch when the child is written to between the two reads"
    (let* ((home file-identity-spec--root)
           (source (expand-file-name "source" home))
           (child (expand-file-name "wt" home)))
      (make-directory source)
      (make-directory child)
      ;; A linked worktree's `.git' is a FILE (a gitfile); that is all the
      ;; scan looks at before asking magit.
      (write-region "gitdir: /nonexistent\n" nil
                    (expand-file-name ".git" child) nil 'silent)
      ;; magit reports the child under a different spelling (truename, with
      ;; a trailing slash), as `git worktree list' does on macOS.
      (cl-letf (((symbol-function 'magit-list-worktrees)
                 (lambda ()
                   (list (list (file-truename source) "0000" "main" nil)
                         (list (file-name-as-directory (file-truename child))
                               "1111" "feature" nil)))))
        (file-identity-spec--with-write-between-reads child
          (let ((children (jf/workspace--worktree-children home)))
            (expect (length children) :to-equal 1)
            (expect (plist-get (car children) :branch) :to-equal "feature")
            (expect writes :to-be-greater-than 1)))))))

;;; file-identity-spec.el ends here
