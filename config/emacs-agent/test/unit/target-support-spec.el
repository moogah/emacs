;;; target-support-spec.el --- jf/frame-state and the control helpers -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md D5 and § Seams:
;; seam/target-support.
;;
;; Batch Emacs has one terminal frame with a selected window, like a
;; headless daemon: the helpers are driven here against a buffer shown in
;; that window, and their effect is read back through `jf/frame-state', as
;; a client would read it.  The headless-daemon scenarios run the same
;; functions through the wrapper in behavioral/bare-target-spec.el.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defvar jf/frame-state-max-chars)

(defconst target-support-spec--file
  (expand-file-name "config/emacs-agent/target-support.el" emacs-agent-test-root)
  "The tangled target-support module.")

(defun target-support-spec--command ()
  "Insert how this command was called, for the run-command spec."
  (interactive)
  (insert (if (called-interactively-p 'any) "interactive" "direct")))

(defmacro target-support-spec--in-shown-buffer (contents &rest body)
  "Run BODY with a fresh buffer holding CONTENTS shown in the selected window.
The buffer is in Emacs state, as on a bare -Q target: the test process
runs evil-mode, whose normal state would read the keys `jf/send-keys'
sends as commands."
  (declare (indent 1))
  `(let ((saved (current-window-configuration))
         (buffer (generate-new-buffer "*emacs-agent-test target*")))
     (unwind-protect
         (progn
           (delete-other-windows)
           (switch-to-buffer buffer)
           (when (fboundp 'evil-local-mode)
             (evil-local-mode -1))
           (insert ,contents)
           ,@body)
       (set-window-configuration saved)
       (kill-buffer buffer))))

(defun target-support-spec--state ()
  "`jf/frame-state' parsed into a plist."
  (json-parse-string (jf/frame-state) :object-type 'plist
                     :false-object nil :null-object nil))


(describe "jf/frame-state (seam/target-support)"

  (it "returns JSON with buffer, windows, point, minibuffer and messages (Scenario: frame-state-observation § State is machine-parseable for assertions)"
    (target-support-spec--in-shown-buffer "hello"
      (goto-char 3)
      (message "emacs-agent-test marker")
      (let* ((state (target-support-spec--state))
             (buffer (plist-get state :buffer)))
        (expect (plist-get buffer :name) :to-equal (buffer-name))
        (expect (plist-get buffer :contents) :to-equal "hello")
        (expect (plist-get buffer :point) :to-equal 3)
        (expect (plist-get buffer :truncated) :to-be nil)
        (expect (seq-some (lambda (window) (equal (plist-get window :buffer) (buffer-name)))
                          (plist-get state :windows))
                :to-be t)
        (expect (plist-get (plist-get state :minibuffer) :active) :to-be nil)
        (expect (seq-some (lambda (line) (string-match-p "emacs-agent-test marker" line))
                          (plist-get state :messages))
                :to-be-truthy))))

  (it "truncates contents over jf/frame-state-max-chars and says so (Scenario: frame-state-observation § Large buffer content is truncated with an indicator)"
    (target-support-spec--in-shown-buffer (make-string 1000 ?x)
      (let* ((jf/frame-state-max-chars 100)
             (buffer (plist-get (target-support-spec--state) :buffer)))
        (expect (<= (length (plist-get buffer :contents)) 100) :to-be t)
        (expect (plist-get buffer :truncated) :to-be t))))

  (it "returns a multibyte string carrying non-ASCII buffer text as characters (seam/target-support)"
    ;; `json-parse-string' also accepts unibyte UTF-8, so the reply string
    ;; itself is tested: unibyte, emacsclient prints ‘ as \342\200\230.
    (target-support-spec--in-shown-buffer "é ‘quoted’ →"
      (let ((state (jf/frame-state)))
        (expect (multibyte-string-p state) :to-be t)
        (expect (string-search "é ‘quoted’ →" state) :not :to-be nil)
        (expect (plist-get (plist-get (target-support-spec--state) :buffer) :contents)
                :to-equal "é ‘quoted’ →"))))

  (it "loads into emacs -Q with nothing else and reports state there (seam/target-support)"
    (with-temp-buffer
      (let ((exit (call-process (emacs-agent-test-emacs) nil t nil
                                "-Q" "--batch" "-l" target-support-spec--file
                                "--eval" "(princ (jf/frame-state))")))
        (expect exit :to-equal 0)
        (let ((state (json-parse-string (buffer-string) :object-type 'plist)))
          (expect (plist-member state :buffer) :not :to-be nil))))))


(describe "The control helpers (seam/target-support)"

  (it "inserts a string literally into the current buffer, seen by jf/frame-state (Scenario: emacs-control-tools § Type literal text into the current buffer)"
    (let ((text "a\tb (C-x C-c) \"q\" \\n é"))
      (target-support-spec--in-shown-buffer ""
        (jf/type-text text)
        (expect (plist-get (plist-get (target-support-spec--state) :buffer) :contents)
                :to-equal text))))

  (it "executes a kbd sequence as if typed, seen by jf/frame-state (Scenario: emacs-control-tools § Send a key sequence in kbd notation)"
    (target-support-spec--in-shown-buffer ""
      (jf/send-keys "a b C-a x")
      (expect (plist-get (plist-get (target-support-spec--state) :buffer) :contents)
              :to-equal "xab")))

  (it "runs a named command interactively, seen by jf/frame-state (Scenario: emacs-control-tools § Run a named interactive command)"
    (target-support-spec--in-shown-buffer ""
      (jf/run-command "target-support-spec--command")
      (expect (plist-get (plist-get (target-support-spec--state) :buffer) :contents)
              :to-equal "interactive"))))

;;; target-support-spec.el ends here
