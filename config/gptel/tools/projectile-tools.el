;; -*- lexical-binding: t; -*-
(require 'projectile)
(require 'jf-gptel-scope-core)

(defvar gptel-projectile-result-limit 40000
  "Maximum character count for tool results to prevent context overflow.")

(defun gptel-projectile--result-limit (result)
  "Limit RESULT to gptel-projectile-result-limit characters.
If exceeded, return a warning message instead."
  (if (>= (length (format "%s" result)) gptel-projectile-result-limit)
      (format "Results over %s characters. Use filter_pattern to narrow results or increase limit."
              gptel-projectile-result-limit)
    result))

(gptel-make-tool
 :name "list_known_projects"
 :function (lambda ()
             (let ((projects (projectile-known-projects)))
               (if (null projects)
                   "No known projects found. Open a project directory in Emacs first (C-x C-f to a project file), then projectile will track it."
                 (format "Known projects (%d):\n\n%s\n\nUse these paths with other projectile tools like get_project_info or list_project_files."
                         (length projects)
                         (mapconcat #'identity projects "\n")))))
 :description "List all known projects tracked by projectile.

CRITICAL: Use this tool FIRST when you don't know which project to work with.
This is especially important when running in a gptel buffer that isn't associated
with a file - there's no 'current project' context in that case.

Returns a list of absolute paths to all projects that projectile knows about.
These paths can then be used with other projectile tools:
- get_project_info(directory) - get project details
- list_project_files(directory) - list project files
- list_project_directories(directory) - list project structure
- expand_project_path(relative_path, directory) - resolve paths

Typical workflow:
1. Call list_known_projects() to see available projects
2. Identify the project you want (e.g., 'dotfiles' project)
3. Use that project's path with other tools

No arguments needed - returns all known projects."
 :args (list)
 :category "projectile")

(gptel-make-tool
 :name "get_project_info"
 :function (lambda (directory)
             (let ((project-root (projectile-project-root directory)))
               (if (not project-root)
                   (format "No project found at %s. This directory is not part of a projectile project.\n\nTip: Use list_known_projects first to see available projects." directory)
                 (let ((name (projectile-project-name project-root))
                       (type (projectile-project-type project-root))
                       (vcs (projectile-project-vcs project-root)))
                   (format "Project Info:\n\nName: %s\nRoot: %s\nType: %s\nVCS: %s\n\nUse list_project_files to see files, or list_project_directories to see directory structure."
                           name project-root (or type "generic") (or vcs "none"))))))
 :description "Get high-level overview of a specific project.

Returns project name, root directory, project type (ruby, python, nodejs, etc.),
and VCS system (git, hg, svn, none).

IMPORTANT: You must provide a directory path. Get project paths from list_known_projects first.

Typical workflow:
1. Call list_known_projects() to discover available projects
2. Use a project path from that list with this tool
3. Example: get_project_info('/Users/username/src/dotfiles')

Use this tool when you need to:
- Get details about a specific project
- Understand what type of project you're working with
- Know what version control system is used

After using this tool, use list_project_files or list_project_directories to explore
the project contents."
 :args (list '(:name "directory"
               :type string
               :description "Project directory path. Use list_known_projects to discover available projects."))
 :category "projectile")

(gptel-make-scoped-tool
 "list_project_files"
 "SCOPE-AWARE: Requires directory to match allowed patterns.

List all files in the project (respecting ignore rules).

Returns a list of relative file paths within the project, respecting VCS ignore rules
(.gitignore, .hgignore, etc.) and projectile configuration. Files are listed relative
to the project root.

IMPORTANT: You must provide a directory path. Get project paths from list_known_projects first.

Typical workflow:
1. Call list_known_projects() to discover available projects
2. Use a project path from that list with this tool
3. Example: list_project_files('/Users/username/src/dotfiles', 100, '\\.el$')

Use this tool when you need to:
- See all files in a project
- Find files matching a pattern
- Understand project file structure
- Locate specific file types

Arguments:
- directory: Project directory path (required). Use list_known_projects to discover projects.
- limit: Max files to return (default 100, max 500)
- filter_pattern: Regex pattern to filter results (e.g., '\\.py$' for Python files, '^src/' for src directory)

The tool will warn if results exceed 40,000 characters. Use filter_pattern to narrow
results in large projects.

Returns scope_violation error if directory not in allowed patterns.
Use request_scope_expansion to request access."
 (list '(:name "directory"
         :type string
         :description "Project directory path. Use list_known_projects to discover available projects.")
       '(:name "limit"
         :type integer
         :optional t
         :description "Max files to return (default 100, max 500)")
       '(:name "filter_pattern"
         :type string
         :optional t
         :description "Regex pattern to filter results (e.g., '\\\\.py$', '^src/')"))
 "projectile"
 (let ((project-root (projectile-project-root directory)))
   (if (not project-root)
       (list :success nil
             :error "no_project_found"
             :message (format "No project found at %s. Use list_known_projects to discover available projects." directory))
     (let* ((file-limit (min (or limit 100) 500))
            (all-files (projectile-project-files project-root))
            (filtered-files (if filter-pattern
                                (seq-filter (lambda (f) (string-match-p filter-pattern f)) all-files)
                              all-files))
            (total-count (length filtered-files))
            (display-files (seq-take filtered-files (min file-limit total-count)))
            (message-str (concat
                          (format "Project: %s\nTotal files: %d%s\nShowing: %d\n\n"
                                  project-root
                                  total-count
                                  (if filter-pattern (format " (filtered by: %s)" filter-pattern) "")
                                  (length display-files))
                          (mapconcat #'identity display-files "\n")
                          (when (> total-count file-limit)
                            (format "\n\n[Truncated. Use filter_pattern to narrow results or increase limit (max 500)]")))))
       (list :success t
             :files display-files
             :total_count total-count
             :message (gptel-projectile--result-limit message-str))))))

(gptel-make-scoped-tool
 "list_project_directories"
 "SCOPE-AWARE: Requires directory to match allowed patterns.

List all directories in project structure.

Returns a list of relative directory paths within the project, respecting ignore rules.
This helps understand the overall structure and organization of the project.

IMPORTANT: You must provide a directory path. Get project paths from list_known_projects first.

Typical workflow:
1. Call list_known_projects() to discover available projects
2. Use a project path from that list with this tool
3. Example: list_project_directories('/Users/username/src/dotfiles', 100)

Use this tool when you need to:
- Understand project directory structure
- Find specific directories (src, tests, docs, etc.)
- Navigate project organization
- Locate where to place new files

Arguments:
- directory: Project directory path (required). Use list_known_projects to discover projects.
- limit: Max directories to return (default 50, max 200)

Returns directories relative to project root, sorted alphabetically.

Returns scope_violation error if directory not in allowed patterns.
Use request_scope_expansion to request access."
 (list '(:name "directory"
         :type string
         :description "Project directory path. Use list_known_projects to discover available projects.")
       '(:name "limit"
         :type integer
         :optional t
         :description "Max directories to return (default 50, max 200)"))
 "projectile"
 (let ((project-root (projectile-project-root directory)))
   (if (not project-root)
       (list :success nil
             :error "no_project_found"
             :message (format "No project found at %s. Use list_known_projects to discover available projects." directory))
     (let* ((dir-limit (min (or limit 50) 200))
            (all-dirs (projectile-project-dirs project-root))
            (total-count (length all-dirs))
            (display-dirs (seq-take all-dirs (min dir-limit total-count)))
            (message-str (concat
                          (format "Project: %s\nTotal directories: %d\nShowing: %d\n\n"
                                  project-root
                                  total-count
                                  (length display-dirs))
                          (mapconcat #'identity display-dirs "\n")
                          (when (> total-count dir-limit)
                            (format "\n\n[Truncated. Increase limit (max 200) to see more directories]")))))
       (list :success t
             :directories display-dirs
             :total_count total-count
             :message (gptel-projectile--result-limit message-str))))))

(gptel-make-tool
 :name "expand_project_path"
 :function (lambda (relative-path &optional directory)
             (let* ((dir (or directory default-directory))
                    (project-root (projectile-project-root dir)))
               (if (not project-root)
                   (format "Error: No project found at %s" dir)
                 (let ((expanded-path (projectile-expand-root relative-path project-root)))
                   (if (file-exists-p expanded-path)
                       (format "Expanded path: %s (exists)" expanded-path)
                     (format "Expanded path: %s (does not exist - can be created)" expanded-path))))))
 :description "Convert relative path to absolute within project context.

Takes a path relative to the project root and expands it to an absolute path.
This is useful for resolving paths before file operations.

Use this tool when you need to:
- Convert project-relative paths to absolute paths
- Verify path resolution before file operations
- Understand where a relative path points
- Plan file creation at specific locations

Arguments:
- relative_path: Path relative to project root (required)
- directory: Optional project directory context (defaults to current)

Returns the absolute path and indicates whether it exists. If the path doesn't exist,
it can still be used for file creation operations."
 :args (list '(:name "relative_path"
               :type string
               :description "Path relative to project root")
             '(:name "directory"
               :type string
               :optional t
               :description "Project directory context (defaults to current)"))
 :category "projectile")

(gptel-make-scoped-tool
 "search_project_content"
 "SCOPE-AWARE: Requires directory to match allowed patterns.

Find all files containing a specific string or pattern.

This is the PRIMARY search tool for finding where code is defined or used in a project.
It searches file contents (not just filenames) and returns a list of matching files.

CRITICAL: This is much more efficient than reading every file. Use this tool when you need to:
- Find where a function/class/variable is defined
- Find all usages of a specific symbol
- Search for specific patterns in code
- Locate examples of specific code patterns

Arguments:
- directory: Project directory path (required). Use list_known_projects to discover projects.
- search_term: String to search for (required). Can be a function name, class name, error message, etc.
- file_pattern: Optional glob pattern to filter files (e.g., '*.el' for Elisp, '*.py' for Python, '*.{js,ts}' for JavaScript/TypeScript)

Returns a list of file paths (relative to project root) containing the search term.
If no matches found, returns a clear message.

Example queries:
- search_project_content('/path/to/project', 'defun my-function')
- search_project_content('/path/to/project', 'import pandas', '*.py')
- search_project_content('/path/to/project', 'TODO', '*.{el,org}')

The search uses ripgrep, ag, or grep (fastest available) and respects .gitignore rules.

Returns scope_violation error if directory not in allowed patterns.
Use request_scope_expansion to request access."
 (list '(:name "directory"
         :type string
         :description "Project directory path. Use list_known_projects to discover available projects.")
       '(:name "search_term"
         :type string
         :description "String to search for in file contents")
       '(:name "file_pattern"
         :type string
         :optional t
         :description "Optional glob pattern to filter files (e.g., '*.el', '*.py', '*.{js,ts}')"))
 "projectile"
 (let ((project-root (projectile-project-root directory)))
   (if (not project-root)
       (list :success nil
             :error "no_project_found"
             :message (format "No project found at %s. Use list_known_projects to discover available projects." directory))
     (condition-case err
         (let* ((files (projectile-files-with-string search-term project-root file-pattern))
                (count (length files))
                (message-str (if (zerop count)
                                 (format "No files found containing '%s'%s in project %s"
                                         search-term
                                         (if file-pattern (format " matching pattern '%s'" file-pattern) "")
                                         project-root)
                               (format "Found %d file%s containing '%s'%s:\n\n%s"
                                       count
                                       (if (= count 1) "" "s")
                                       search-term
                                       (if file-pattern (format " matching pattern '%s'" file-pattern) "")
                                       (mapconcat #'identity files "\n")))))
           (list :success t
                 :files files
                 :match_count count
                 :search_term search-term
                 :message (gptel-projectile--result-limit message-str)))
       (error (list :success nil
                    :error "search_error"
                    :message (format "Error searching project: %s" (error-message-string err))))))))

(gptel-make-scoped-tool
 "list_test_files"
 "SCOPE-AWARE: Requires directory to match allowed patterns.

Get all test files in the project.

This tool identifies test files based on project-specific naming conventions and patterns.
It understands different testing frameworks and languages automatically.

Use this tool when you need to:
- Understand test coverage in a project
- Find all tests to review or modify
- Identify testing patterns and structure
- Assess project quality (test count vs implementation files)

How it works:
- Detects project type (Ruby, Python, Java, JavaScript, etc.)
- Uses type-specific test file patterns (e.g., '*_test.py', 'test_*.py', '*Test.java', '*.test.js')
- Checks test directory conventions (e.g., 'test/', 'tests/', '__tests__/')
- Returns files identified as tests

Arguments:
- directory: Project directory path (required). Use list_known_projects to discover projects.
- limit: Max test files to return (default 100). Increase for large test suites.

Returns a list of test file paths relative to project root.
If no tests found, provides guidance on possible reasons.

Common test patterns by language:
- Python: test_*.py, *_test.py in tests/ directory
- Ruby: *_spec.rb, *_test.rb in spec/ or test/ directory
- JavaScript: *.test.js, *.spec.js in __tests__/ or alongside source
- Java: *Test.java in src/test/java/
- Go: *_test.go alongside source files

Returns scope_violation error if directory not in allowed patterns.
Use request_scope_expansion to request access."
 (list '(:name "directory"
         :type string
         :description "Project directory path. Use list_known_projects to discover available projects.")
       '(:name "limit"
         :type integer
         :optional t
         :description "Max test files to return (default 100)"))
 "projectile"
 (let ((project-root (projectile-project-root directory)))
   (if (not project-root)
       (list :success nil
             :error "no_project_found"
             :message (format "No project found at %s. Use list_known_projects to discover available projects." directory))
     (let* ((all-files (projectile-project-files project-root))
            (test-files (projectile-test-files all-files))
            (file-limit (or limit 100))
            (total-count (length test-files))
            (display-files (seq-take test-files (min file-limit total-count))))
       (if (zerop total-count)
           (list :success t
                 :test_files nil
                 :test_count 0
                 :message (format "No test files found in project %s.\n\nThis could mean:\n- Project has no tests yet\n- Test files don't follow naming conventions\n- Project type not detected correctly (check get_project_info)" project-root))
         (let ((message-str (format "Found %d test file%s in project %s:\n\n%s%s"
                                    total-count
                                    (if (= total-count 1) "" "s")
                                    project-root
                                    (mapconcat #'identity display-files "\n")
                                    (if (> total-count file-limit)
                                        (format "\n\n[Showing %d of %d test files. Increase limit to see more.]" file-limit total-count)
                                      ""))))
           (list :success t
                 :test_files display-files
                 :test_count total-count
                 :message (gptel-projectile--result-limit message-str))))))))

(gptel-make-scoped-tool
 "find_related_test"
 "SCOPE-AWARE: Requires directory to match allowed patterns.

Find the test file(s) for a specific implementation file.

This tool finds the corresponding test file for an implementation file based on
project conventions and naming patterns. It's the inverse of finding implementation from test.

Use this tool when you need to:
- Find the test for a specific source file
- Check if a file has test coverage
- Navigate from implementation to its tests
- Understand test organization for a specific module

How it works:
- Takes an implementation file path (relative or absolute)
- Uses project-type-specific rules to find matching test
- Checks multiple possible test locations and naming patterns
- Returns test file path(s) or explains why none found

Arguments:
- directory: Project directory path (required). Use list_known_projects to discover projects.
- file_path: Path to implementation file (required). Can be relative to project root or absolute.

Returns the test file path(s) relative to project root.
If no test found, provides helpful guidance.

Examples:
- find_related_test('/path/to/project', 'src/utils/parser.py') → finds 'tests/utils/test_parser.py'
- find_related_test('/path/to/project', 'lib/api.rb') → finds 'spec/lib/api_spec.rb'
- find_related_test('/path/to/project', 'components/Button.tsx') → finds 'components/Button.test.tsx'

Common patterns:
- Python: src/foo/bar.py → tests/foo/test_bar.py or tests/foo/bar_test.py
- Ruby: lib/foo.rb → spec/lib/foo_spec.rb
- JavaScript: src/Component.js → src/Component.test.js or __tests__/Component.test.js
- Java: src/main/java/Foo.java → src/test/java/FooTest.java

Returns scope_violation error if directory not in allowed patterns.
Use request_scope_expansion to request access."
 (list '(:name "directory"
         :type string
         :description "Project directory path. Use list_known_projects to discover available projects.")
       '(:name "file_path"
         :type string
         :description "Path to implementation file (relative to project root or absolute)"))
 "projectile"
 (let ((project-root (projectile-project-root directory)))
   (if (not project-root)
       (list :success nil
             :error "no_project_found"
             :message (format "No project found at %s. Use list_known_projects to discover available projects." directory))
     (let* ((absolute-path (if (file-name-absolute-p file-path)
                               file-path
                             (expand-file-name file-path project-root)))
            (relative-path (file-relative-name absolute-path project-root)))
       (condition-case err
           (let ((test-files (projectile--related-files absolute-path :test)))
             (if (null test-files)
                 (list :success t
                       :test_files nil
                       :source_file relative-path
                       :message (format "No test file found for %s\n\nPossible reasons:\n- No test exists yet for this file\n- Test naming convention not followed\n- File is already a test file\n- Project type not correctly detected\n\nUse list_test_files to see all tests in the project." relative-path))
               (list :success t
                     :test_files (mapcar (lambda (f) (file-relative-name f project-root)) test-files)
                     :source_file relative-path
                     :message (format "Test file%s for %s:\n\n%s"
                                      (if (> (length test-files) 1) "s" "")
                                      relative-path
                                      (mapconcat (lambda (f) (file-relative-name f project-root)) test-files "\n")))))
         (error (list :success nil
                      :error "find_test_error"
                      :message (format "Error finding test for %s: %s" relative-path (error-message-string err)))))))))

(gptel-make-scoped-tool
 "find_related_files"
 "SCOPE-AWARE: Requires directory to match allowed patterns.

Find all semantically related files for a given file.

This tool discovers files that are logically related to a source file, based on
project conventions and file relationships. It goes beyond just finding tests.

Use this tool when you need to:
- Understand the full context around a file (tests, implementations, headers)
- Navigate between related files (C header ↔ implementation, test ↔ impl)
- Find all files needed to understand a component
- Discover file organization patterns in a project

How it works:
- Analyzes file path and project type
- Identifies relationship kinds: test, impl (implementation), other
- Uses project-specific rules for finding related files
- Returns organized map of all relationships

Relationship types:
- **test**: Test files for this implementation
- **impl**: Implementation files for this test/header
- **other**: Other related files (e.g., C header ↔ source, TypeScript .ts ↔ .d.ts)

Arguments:
- directory: Project directory path (required). Use list_known_projects to discover projects.
- file_path: Path to file (required). Can be relative to project root or absolute.
- relationship_type: Optional filter - 'test', 'impl', 'other', or omit for all relationships

Returns structured information about related files:
- If relationship_type specified: List of files for that relationship
- If omitted: All relationships organized by type

Examples:
- find_related_files('/path/to/project', 'src/parser.c') → finds parser.h (other), test_parser.c (test)
- find_related_files('/path/to/project', 'include/api.h', 'impl') → finds src/api.c
- find_related_files('/path/to/project', 'test/test_foo.py', 'impl') → finds src/foo.py
- find_related_files('/path/to/project', 'Component.tsx') → finds Component.test.tsx, Component.stories.tsx

Common patterns by language:
- C/C++: header ↔ implementation (.h ↔ .c/.cpp)
- Python: implementation ↔ test (foo.py ↔ test_foo.py)
- TypeScript: source ↔ test ↔ types (.ts ↔ .test.ts ↔ .d.ts)
- Java: implementation ↔ test (Foo.java ↔ FooTest.java)

Returns scope_violation error if directory not in allowed patterns.
Use request_scope_expansion to request access."
 (list '(:name "directory"
         :type string
         :description "Project directory path. Use list_known_projects to discover available projects.")
       '(:name "file_path"
         :type string
         :description "Path to file (relative to project root or absolute)")
       '(:name "relationship_type"
         :type string
         :optional t
         :description "Optional: 'test', 'impl', or 'other'. Omit to see all relationships."))
 "projectile"
 (let ((project-root (projectile-project-root directory)))
   (if (not project-root)
       (list :success nil
             :error "no_project_found"
             :message (format "No project found at %s. Use list_known_projects to discover available projects." directory))
     (let* ((absolute-path (if (file-name-absolute-p file-path)
                               file-path
                             (expand-file-name file-path project-root)))
            (relative-path (file-relative-name absolute-path project-root)))
       (condition-case err
           (if relationship-type
               ;; Specific relationship type requested
               (let* ((rel-keyword (intern (concat ":" relationship-type)))
                      (related (projectile--related-files absolute-path rel-keyword)))
                 (if (null related)
                     (list :success t
                           :relationships (list (cons (intern relationship-type) nil))
                           :source_file relative-path
                           :message (format "No %s files found for %s" relationship-type relative-path))
                   (list :success t
                         :relationships (list (cons (intern relationship-type)
                                                    (mapcar (lambda (f) (file-relative-name f project-root)) related)))
                         :source_file relative-path
                         :message (format "%s file%s for %s:\n\n%s"
                                          (capitalize relationship-type)
                                          (if (> (length related) 1) "s" "")
                                          relative-path
                                          (mapconcat (lambda (f) (file-relative-name f project-root)) related "\n")))))
             ;; All relationships
             (let* ((kinds (projectile--related-files-kinds absolute-path))
                    (results '())
                    (relationships '()))
               (dolist (kind kinds)
                 (let ((files (projectile--related-files absolute-path kind)))
                   (when files
                     (let* ((kind-name (substring (symbol-name kind) 1))
                            (rel-files (mapcar (lambda (f) (file-relative-name f project-root)) files)))
                       (push (cons (intern kind-name) rel-files) relationships)
                       (push (format "%s:\n  %s"
                                     (capitalize kind-name)
                                     (mapconcat #'identity rel-files "\n  "))
                             results)))))
               (if (null results)
                   (list :success t
                         :relationships nil
                         :source_file relative-path
                         :message (format "No related files found for %s\n\nThis file may be standalone or relationships not configured for this project type." relative-path))
                 (list :success t
                       :relationships (reverse relationships)
                       :source_file relative-path
                       :message (format "Related files for %s:\n\n%s" relative-path (mapconcat #'identity (reverse results) "\n\n"))))))
         (error (list :success nil
                      :error "find_related_error"
                      :message (format "Error finding related files for %s: %s" relative-path (error-message-string err)))))))))

(provide 'projectile-tools)
;;; projectile-tools.el ends here
