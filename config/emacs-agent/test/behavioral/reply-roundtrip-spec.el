;;; reply-roundtrip-spec.el --- A long non-ASCII reply through the real emacsclient -*- lexical-binding: t; -*-

;; Change drive-isolated-emacs-via-emacsclient, design.md § Seams:
;; seam/dispatcher-args, seam/target-support, seam/wrapper-cli.
;;
;; On macOS emacsclient garbles a decoded (multibyte) reply over about 1000
;; characters holding non-ASCII text, and still exits 0.  The replies are
;; json-serialize's unibyte UTF-8, not decoded: emacsclient prints their
;; non-ASCII bytes as octal escapes, so a reply crosses the socket as ASCII
;; and bin/emacs-agent turns the escapes back into the bytes.  This spec
;; drives that path end to end with the real binaries: a bare `emacs -Q'
;; daemon in a private socket directory (`emacs-agent-test-with-socket-dir'),
;; whose buffer is set with `target NAME eval' and read back with `target
;; NAME state'.  A -Q target has no gptel, so `state' is the reply path to
;; drive.  The daemon is started directly, not with `target start'; the scope
;; kills it on exit.  Nothing here can reach the live socket.

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

;;; reply-roundtrip-spec.el ends here
