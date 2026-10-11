;;; reply-roundtrip-spec.el --- Long replies through the real emacsclient -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md § Seams:
;; seam/wrapper-cli, seam/dispatcher-args, seam/target-support.
;;
;; server.el sends a reply as `-print' messages of at most
;; `server-msg-size' (1024) characters, and escapes every space, `-', `&'
;; and newline in it as two characters.  emacsclient on macOS reads 1024
;; bytes at a time, so two kinds of message garble a reply ("*ERROR*:
;; Unknown message" in its output, exit 0): one shortened by a byte to keep
;; an escape whole across a cut, and one longer in bytes than in
;; characters, from a decoded (multibyte) reply holding non-ASCII text.
;; bin/emacs-agent asks Emacs for every reply as base64, which holds no
;; escape and no non-ASCII, with a pad character at the lengths where pp's
;; trailing newline would fall on a cut, and decodes it.  These specs drive
;; that path end to end with the real binaries: a bare `emacs -Q' daemon in
;; a private socket directory (`emacs-agent-test-with-socket-dir'), driven
;; with `target NAME eval' and `target NAME state'.  A -Q target has no
;; gptel, so those are the reply paths to drive; `call' and `tools' take
;; the same encoder.  The daemon is started directly, not with `target
;; start'; the scope kills it on exit.  Nothing here can reach the live
;; socket.  The sweeps place an escape, or the reply's length, at every
;; position around the first and second cut.

;;; Code:

(require 'emacs-agent-test-helpers
         (expand-file-name "../helpers-spec.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defconst reply-roundtrip-spec--line-form
  "(concat \"A note \" (string #x2014) \" with \" (string #x201c) \"curly\" (string #x201d) \" quotes, an \" (string #x2018) \"aside\" (string #x2019) \" and caf\" (string #xe9) \".\\n\")"
  "A form, in ASCII, whose value is one line of the buffer text.
The same form builds the text in the target and the expected text
here, so nothing but ASCII crosses into the target.")

(defconst reply-roundtrip-spec--line
  (eval (car (read-from-string reply-roundtrip-spec--line-form)) t)
  "One line of the buffer text: an em-dash, curly quotes and é.")

(defconst reply-roundtrip-spec--text
  (apply #'concat (make-list 100 reply-roundtrip-spec--line))
  "About 5000 characters of non-ASCII text, well past emacsclient's 1024.")


(describe "A long non-ASCII reply through the real emacsclient (seam/dispatcher-args, seam/wrapper-cli)"

  (it "returns ~5000 characters of em-dashes and curly quotes from a bare target's state intact, exit 0 (seam/target-support)"
    (assume (emacs-agent-test-binaries-available-p)
            "no Emacs / emacsclient binary to start a target with")
    (emacs-agent-test-with-socket-dir
      (emacs-agent-test-with-real-binaries
        (emacs-agent-test-start-daemon "roundtrip")
        (let ((set (emacs-agent-test-wrapper-run
                    "target" "roundtrip" "eval"
                    (format "(with-current-buffer (window-buffer (selected-window)) (erase-buffer) (insert (apply #'concat (make-list 100 %s))) (buffer-size))"
                            reply-roundtrip-spec--line-form))))
          (expect (plist-get set :exit) :to-equal 0)
          (expect (string-trim (plist-get set :stdout))
                  :to-equal (number-to-string (length reply-roundtrip-spec--text))))
        (let* ((result (let ((coding-system-for-read 'utf-8))
                         (emacs-agent-test-wrapper-run "target" "roundtrip" "state")))
               (state (ignore-errors
                        (json-parse-string (plist-get result :stdout)
                                           :object-type 'plist
                                           :false-object nil :null-object nil))))
          (expect (cons (plist-get result :exit) (plist-get result :stderr))
                  :to-equal (cons 0 ""))
          (expect (string-search "Unknown message" (plist-get result :stdout)) :to-be nil)
          (expect (plist-get (plist-get state :buffer) :truncated) :to-be nil)
          (expect (plist-get (plist-get state :buffer) :contents)
                  :to-equal reply-roundtrip-spec--text))))))

(defmacro reply-roundtrip-spec--with-target (name &rest body)
  "Run BODY with a bare daemon NAME in a private socket directory."
  (declare (indent 1))
  `(progn
     (assume (emacs-agent-test-binaries-available-p)
             "no Emacs / emacsclient binary to start a target with")
     (emacs-agent-test-with-socket-dir
       (emacs-agent-test-with-real-binaries
         (emacs-agent-test-start-daemon ,name)
         ,@body))))

(defun reply-roundtrip-spec--run (&rest args)
  "Run the wrapper with ARGS, its output read as UTF-8.
Return (EXIT STDOUT STDERR)."
  (let* ((coding-system-for-read 'utf-8)
         (result (apply #'emacs-agent-test-wrapper-run args)))
    (list (plist-get result :exit) (plist-get result :stdout)
          (plist-get result :stderr))))

(defun reply-roundtrip-spec--spaced (offset)
  "A form whose value is OFFSET `a's, a space and 60 more `a's."
  (format "(concat (make-string %d ?a) \" \" (make-string 60 ?a))" offset))

(defun reply-roundtrip-spec--printed (value)
  "VALUE, a string, as `target NAME eval' prints it: pp's text."
  (concat (prin1-to-string value) "\n"))

(defun reply-roundtrip-spec--encoded-length (text)
  "The length of the base64 that carries TEXT, before any pad."
  (length (base64-encode-string (encode-coding-string text 'utf-8) t)))

(defun reply-roundtrip-spec--set-buffer (name form)
  "Make the value of FORM the text of target NAME's selected buffer."
  (expect (reply-roundtrip-spec--run
           "target" name "eval"
           (format "(with-current-buffer (window-buffer (selected-window)) (erase-buffer) (insert %s) nil)"
                   form))
          :to-equal '(0 "nil\n" "")))

(defun reply-roundtrip-spec--state (name)
  "Run `target NAME state'; return (EXIT STDERR TEXT STATE).
TEXT is the JSON printed, without the newline after it, and STATE
it parsed, or nil."
  (pcase-let* ((`(,exit ,stdout ,stderr) (reply-roundtrip-spec--run "target" name "state"))
               (text (string-remove-suffix "\n" stdout)))
    (list exit stderr text
          (ignore-errors
            (json-parse-string text :object-type 'plist
                               :false-object nil :null-object nil)))))

(defun reply-roundtrip-spec--contents (state)
  "The buffer contents STATE reports."
  (plist-get (plist-get state :buffer) :contents))


(describe "Replies that emacsclient would split badly (seam/wrapper-cli)"

  (it "prints a target eval value exactly, exit 0, wherever a space falls around the first and second cut"
    (reply-roundtrip-spec--with-target "split-eval"
      (dolist (offset (append (number-sequence 1000 1030) (number-sequence 2015 2035)))
        (let ((value (concat (make-string offset ?a) " " (make-string 60 ?a))))
          (expect (cons offset (reply-roundtrip-spec--run
                                "target" "split-eval" "eval"
                                (reply-roundtrip-spec--spaced offset)))
                  :to-equal (cons offset (list 0 (reply-roundtrip-spec--printed value) "")))))))

  (it "returns state exactly, exit 0, wherever a space in the buffer falls around the first and second cut (seam/target-support)"
    (reply-roundtrip-spec--with-target "split-state"
      (dolist (offset (append (number-sequence 950 980) (number-sequence 1960 1990)))
        (reply-roundtrip-spec--set-buffer "split-state" (reply-roundtrip-spec--spaced offset))
        (pcase-let ((`(,exit ,stderr ,_ ,state) (reply-roundtrip-spec--state "split-state")))
          (expect (list offset exit stderr (reply-roundtrip-spec--contents state))
                  :to-equal (list offset 0 ""
                                  (concat (make-string offset ?a) " "
                                          (make-string 60 ?a))))))))

  (it "prints a target eval value exactly, exit 0, whose base64 is 2020-2028 or 6064-6072 characters long, the pad's lengths 2024 and 6068 among them"
    (reply-roundtrip-spec--with-target "split-length"
      (let (lengths)
        ;; pp prints the string quoted, with a newline: three bytes more.
        (dolist (size (append (number-sequence 1510 1518) (number-sequence 4543 4551)))
          (let ((printed (reply-roundtrip-spec--printed (make-string size ?a))))
            (push (reply-roundtrip-spec--encoded-length printed) lengths)
            (expect (cons size (reply-roundtrip-spec--run
                                "target" "split-length" "eval"
                                (format "(make-string %d ?a)" size)))
                    :to-equal (cons size (list 0 printed "")))))
        (dolist (length '(2020 2024 2028 6064 6068 6072))
          (expect (cons length (and (memq length lengths) t))
                  :to-equal (cons length t))))))

  (it "returns state exactly, exit 0, whose base64 is 2020-2028 or 6064-6072 characters long, the pad's lengths 2024 and 6068 among them (seam/target-support)"
    (reply-roundtrip-spec--with-target "split-state-length"
      (reply-roundtrip-spec--set-buffer "split-state-length" "\"\"")
      ;; The first state loads target-support.el; the second is the base.
      (reply-roundtrip-spec--state "split-state-length")
      (let* ((base (length (encode-coding-string
                            (nth 2 (reply-roundtrip-spec--state "split-state-length"))
                            'utf-8)))
             lengths)
        ;; The JSON's base64 is 2020-2028 characters for 1513-1521 bytes and
        ;; 6064-6072 for 4546-4554.  Each `a' adds a byte, and point's digits
        ;; up to three more.
        (dolist (range '((1513 . 1521) (4546 . 4554)))
          (dolist (size (number-sequence (- (car range) base 4) (- (cdr range) base)))
            (reply-roundtrip-spec--set-buffer "split-state-length"
                                              (format "(make-string %d ?a)" size))
            (pcase-let ((`(,exit ,stderr ,text ,state)
                         (reply-roundtrip-spec--state "split-state-length")))
              (push (reply-roundtrip-spec--encoded-length text) lengths)
              (expect (list size exit stderr (reply-roundtrip-spec--contents state))
                      :to-equal (list size 0 "" (make-string size ?a))))))
        (dolist (length '(2020 2024 2028 6064 6068 6072))
          (expect (cons length (and (memq length lengths) t))
                  :to-equal (cons length t))))))

  (it "prints a 3000-character non-ASCII target eval value exactly, exit 0"
    (reply-roundtrip-spec--with-target "split-multibyte"
      (expect (reply-roundtrip-spec--run "target" "split-multibyte" "eval"
                                         "(make-string 3000 ?\u00e9)")
              :to-equal (list 0 (reply-roundtrip-spec--printed (make-string 3000 ?\u00e9))
                              "")))))

;;; reply-roundtrip-spec.el ends here
