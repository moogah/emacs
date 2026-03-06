;;; bash-parser-glob.el --- Glob pattern matching for bash parser -*- lexical-binding: t; -*-

(defun jf/bash--glob-to-regex (glob-pattern)
  "Convert GLOB-PATTERN to regex pattern.
Handles *, ?, and [abc] character classes.
Escapes regex special characters.

This is a simpler version than `jf/bash--glob-segment-to-regex' that
treats * as matching any character (including /), suitable for simple
filename matching."
  (let ((regex "")
        (i 0)
        (len (length glob-pattern)))
    (while (< i len)
      (let ((ch (aref glob-pattern i)))
        (cond
         ;; Single-char wildcard
         ((eq ch ??)
          (setq regex (concat regex "[^/]"))
          (setq i (1+ i)))

         ;; Any-chars wildcard
         ((eq ch ?*)
          (setq regex (concat regex "[^/]*"))
          (setq i (1+ i)))

         ;; Character class [abc]
         ((eq ch ?\[)
          (let ((class-start i)
                (class-end nil))
            ;; Find matching ]
            (setq i (1+ i))
            (while (and (< i len) (null class-end))
              (when (eq (aref glob-pattern i) ?\])
                (setq class-end i))
              (setq i (1+ i)))
            (if class-end
                ;; Valid character class found
                (setq regex (concat regex
                                   (substring glob-pattern class-start (1+ class-end))))
              ;; No closing ], treat [ as literal
              (setq regex (concat regex "\\["))
              (setq i (1+ class-start)))))

         ;; Escape regex special characters
         ;; Note: In Emacs regex, \( and \) are group markers, not literals.
         ;; Plain ( and ) are literal parentheses, so we DON'T escape them.
         ((memq ch '(?\\ ?. ?+ ?^ ?$ ?{ ?} ?|))
          (setq regex (concat regex "\\" (char-to-string ch)))
          (setq i (1+ i)))

         ;; Regular character
         (t
          (setq regex (concat regex (char-to-string ch)))
          (setq i (1+ i))))))
    regex))

(defun jf/bash--glob-segment-to-regex (glob-segment)
  "Convert GLOB-SEGMENT to regex pattern.
Handles *, ?, and [abc] character classes.
Does not handle ** (that's handled at segment level).
Escapes regex special characters."
  (let ((regex "")
        (i 0)
        (len (length glob-segment)))
    (while (< i len)
      (let ((ch (aref glob-segment i)))
        (cond
         ;; Single-char wildcard (excludes directory separator)
         ((eq ch ??)
          (setq regex (concat regex "[^/]"))
          (setq i (1+ i)))

         ;; Any-chars wildcard (within segment)
         ((eq ch ?*)
          (setq regex (concat regex "[^/]*"))
          (setq i (1+ i)))

         ;; Character class [abc]
         ((eq ch ?\[)
          (let ((class-start i)
                (class-end nil))
            ;; Find matching ]
            (setq i (1+ i))
            (while (and (< i len) (null class-end))
              (when (eq (aref glob-segment i) ?\])
                (setq class-end i))
              (setq i (1+ i)))
            (if class-end
                ;; Valid character class found
                (setq regex (concat regex
                                   (substring glob-segment class-start (1+ class-end))))
              ;; No closing ], treat [ as literal
              (setq regex (concat regex "\\["))
              (setq i (1+ class-start)))))

         ;; Escape regex special characters
         ;; Note: In Emacs regex, \( and \) are group markers, not literals.
         ;; Plain ( and ) are literal parentheses, so we DON'T escape them.
         ((memq ch '(?\\ ?. ?+ ?^ ?$ ?{ ?} ?|))
          (setq regex (concat regex "\\" (char-to-string ch)))
          (setq i (1+ i)))

         ;; Regular character
         (t
          (setq regex (concat regex (char-to-string ch)))
          (setq i (1+ i))))))
    regex))

(defun jf/bash--match-segments (path-segments pattern-segments)
  "Match PATH-SEGMENTS against PATTERN-SEGMENTS recursively.
Handles ** consuming 0-to-N segments. Returns t if match, nil otherwise."
  (cond
   ;; Both empty - successful match
   ((and (null path-segments) (null pattern-segments))
    t)

   ;; Pattern empty but path has segments - no match
   ((null pattern-segments)
    nil)

   ;; Path empty but pattern has non-** segments - no match
   ((null path-segments)
    ;; Only match if remaining patterns are all **
    (seq-every-p (lambda (seg) (string= seg "**")) pattern-segments))

   ;; Handle ** pattern (matches 0-to-N segments)
   ((string= (car pattern-segments) "**")
    (let ((rest-pattern (cdr pattern-segments)))
      (or
       ;; Try matching ** as zero segments (skip it)
       (jf/bash--match-segments path-segments rest-pattern)
       ;; Try matching ** as one or more segments (consume one path segment)
       (jf/bash--match-segments (cdr path-segments) pattern-segments))))

   ;; Handle regular segment matching
   (t
    (let* ((path-seg (car path-segments))
           (pattern-seg (car pattern-segments))
           (regex-pattern (concat "\\`" (jf/bash--glob-segment-to-regex pattern-seg) "\\'"))
           (segment-matches (string-match-p regex-pattern path-seg)))
      ;; Current segment must match, then recurse on rest
      (and segment-matches
           (jf/bash--match-segments (cdr path-segments) (cdr pattern-segments)))))))

(defun jf/bash-glob-match-p (path pattern)
  "Test if PATH matches PATTERN using glob semantics.
No filesystem access - purely string-based matching.

Supported patterns:
  *     - matches any characters within a segment (doesn't cross /)
  **    - matches zero or more complete segments (recursive)
  ?     - matches exactly one character
  [abc] - matches any character in the set

Examples:
  (jf/bash-glob-match-p \"/workspace/file.txt\" \"/workspace/*.txt\")
    => t
  (jf/bash-glob-match-p \"/workspace/src/foo.el\" \"/workspace/**/*.el\")
    => t
  (jf/bash-glob-match-p \"/workspace/file.txt\" \"/workspace/**/file.txt\")
    => t (** matches zero segments)
  (jf/bash-glob-match-p \"/tmp/foo.el\" \"/workspace/**/*.el\")
    => nil

Returns t if PATH matches PATTERN, nil otherwise."
  (let ((path-segments (split-string path "/" t))
        (pattern-segments (split-string pattern "/" t)))
    (jf/bash--match-segments path-segments pattern-segments)))

(provide 'bash-parser-glob)
;;; bash-parser-glob.el ends here
