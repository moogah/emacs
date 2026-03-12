;;; bash-parser-glob.el --- Glob pattern matching for bash parser -*- lexical-binding: t; -*-

;; Author: Jeff Farr
;; Keywords: bash, parser, glob, patterns
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Glob pattern matching without filesystem access.
;; Provides string-based glob pattern matching for security validation.

;;; Code:

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
  "Recursively match PATH-SEGMENTS against PATTERN-SEGMENTS with ** support.

Implements the core matching logic for glob patterns by comparing path segments
against pattern segments. Handles the special ** pattern which can match zero
or more complete directory segments.

PATH-SEGMENTS is a list of strings representing path components split on /.
PATTERN-SEGMENTS is a list of strings representing pattern components split on /.

Returns t if the path matches the pattern, nil otherwise.

Matching rules:
  - Both empty: successful match
  - Pattern empty but path has segments: no match
  - Path empty but pattern has non-** segments: no match
  - ** matches 0-to-N segments (tries both consuming and not consuming)
  - Regular segments must match using jf/bash--glob-segment-to-regex

Recursive pattern behavior (greedy matching):
  - First tries matching ,** as zero segments (skip it, continue with rest)
  - Then tries matching ,** as one or more segments (consume one, try again)
  - This implements O(n) complexity, not exponential backtracking

Examples:
  (jf/bash--match-segments '(\"workspace\" \"file.txt\") '(\"workspace\" \"*.txt\"))
    => t

  (jf/bash--match-segments '(\"a\" \"b\" \"c\" \"file.el\") '(\"a\" \"**\" \"*.el\"))
    => t (** matches \"b/c\")

  (jf/bash--match-segments '(\"workspace\" \"file.txt\") '(\"workspace\" \"file.txt\"))
    => t (exact match)

  (jf/bash--match-segments '(\"tmp\" \"file.txt\") '(\"workspace\" \"*.txt\"))
    => nil (first segment doesn't match)

Internal helper for jf/bash-glob-match-p."
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

Trailing slash behavior:
  Trailing slashes are ignored in both paths and patterns. The matcher
  cannot distinguish between files and directories since it operates on
  strings without filesystem access. Patterns like \"/workspace/dir/\" and
  \"/workspace/dir\" are treated identically.

Examples:
  (jf/bash-glob-match-p \"/workspace/file.txt\" \"/workspace/*.txt\")
    => t
  (jf/bash-glob-match-p \"/workspace/src/foo.el\" \"/workspace/**/*.el\")
    => t
  (jf/bash-glob-match-p \"/workspace/file.txt\" \"/workspace/**/file.txt\")
    => t (** matches zero segments)
  (jf/bash-glob-match-p \"/tmp/foo.el\" \"/workspace/**/*.el\")
    => nil
  (jf/bash-glob-match-p \"/workspace/dir/\" \"/workspace/dir\")
    => t (trailing slashes ignored)

Returns t if PATH matches PATTERN, nil otherwise."
  ;; Validate inputs
  (unless (stringp path)
    (error "jf/bash-glob-match-p: path must be a string, got %S" (type-of path)))
  (unless (stringp pattern)
    (error "jf/bash-glob-match-p: pattern must be a string, got %S" (type-of pattern)))

  (condition-case err
      ;; Split paths on / with OMIT-NULLS=t to ignore trailing slashes.
      ;; This design choice means we cannot distinguish files from directories,
      ;; which is acceptable since we're doing string-based matching without
      ;; filesystem access. The benefit is simpler matching logic.
      (let ((path-segments (split-string path "/" t))
            (pattern-segments (split-string pattern "/" t)))
        (jf/bash--match-segments path-segments pattern-segments))
    (error
     ;; On error, return nil (no match)
     ;; Errors typically indicate malformed patterns or internal bugs
     nil)))

(provide 'bash-parser-glob)
;;; bash-parser-glob.el ends here
