;; -*- lexical-binding: t; -*-

(gptel-make-tool
 :name "create_file"
 :function (lambda (path filename content)
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :description "Create a new file with the specified content"
 :args (list '(:name "path"
               :type string
               :description "The directory where to create the file")
             '(:name "filename"
               :type string
               :description "The name of the file to create")
             '(:name "content"
               :type string
               :description "The content to write to the file"))
 :category "filesystem")

(gptel-make-tool
 :name "read_file"
 :function (lambda (filepath)
             (if (file-exists-p filepath)
                 (with-temp-buffer
                   (insert-file-contents filepath)
                   (buffer-string))
               (format "Error: File not found: %s" filepath)))
 :description "Read the contents of a file"
 :args (list '(:name "filepath"
               :type string
               :description "The full path to the file to read"))
 :category "filesystem")

(gptel-make-tool
 :name "list_directory"
 :function (lambda (path)
             (if (file-directory-p path)
                 (let ((files (directory-files path nil "^[^.]")))
                   (format "Files in %s:\n%s"
                           path
                           (mapconcat #'identity files "\n")))
               (format "Error: Directory not found: %s" path)))
 :description "List files in a directory (excludes hidden files)"
 :args (list '(:name "path"
               :type string
               :description "The directory path to list"))
 :category "filesystem")
