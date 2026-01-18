;; -*- lexical-binding: t; -*-

(defvar jf/postgres-connections nil
  "Alist of PostgreSQL connection definitions.
Each entry has the form (NAME . PLIST) where PLIST contains:
  :host        - Hostname or 'docker for containers (required)
  :port        - Port number (default 5432)
  :database    - Database name (required)
  :user        - Username (required)
  :docker-name - Docker container name (required if :host is 'docker)
  :auth-key    - Host key for auth-source lookup (overrides :host)

Example:
  (setq jf/postgres-connections
        '((local-dev
           :host \"localhost\"
           :port 5432
           :database \"dev_db\"
           :user \"postgres\"
           :auth-key \"localhost-postgres\")
          (docker-test
           :host docker
           :docker-name \"my-postgres-container\"
           :database \"testdb\"
           :user \"postgres\"
           :auth-key \"docker-postgres\")))")

(defun jf/postgres-get-auth-details (auth-key)
  "Retrieve all authentication details for AUTH-KEY from auth-source.
Returns a plist with :user :password :host :port :database, or nil if not found.

Auth-source looks up AUTH-KEY in ~/.authinfo.gpg using the 'machine' field.
Custom netrc properties (server, database) are preserved in the returned plist.
AUTH-KEY can be a symbol or string."
  ;; Convert auth-key to string (auth-source-search requires string for :host)
  (let ((host-key (if (stringp auth-key) auth-key (symbol-name auth-key))))
    (when-let* ((auth-info (car (auth-source-search :host host-key :require '(:secret))))
                (secret (plist-get auth-info :secret))
                (password (if (functionp secret) (funcall secret) secret)))
      (let ((port-value (plist-get auth-info :port)))
        (list :user (plist-get auth-info :user)
              :password password
              :host (or (plist-get auth-info :server) "127.0.0.1")
              :port (if port-value
                        (if (stringp port-value)
                            (string-to-number port-value)
                          port-value)
                      5432)
              :database (plist-get auth-info :database))))))

(defun jf/postgres-get-docker-port (container-name)
  "Get the host port mapped to PostgreSQL (5432) in CONTAINER-NAME.
Returns port number or 5432 if container is not found."
  (let* ((cmd (format "docker port %s 5432 2>/dev/null | cut -d: -f2"
                     (shell-quote-argument container-name)))
         (output (string-trim (shell-command-to-string cmd))))
    (if (string-empty-p output)
        (progn
          (warn "Could not determine port for Docker container: %s. Using default 5432." container-name)
          5432)
      (string-to-number output))))

(defun jf/postgres-get-connection-params (connection-name)
  "Get connection parameters for CONNECTION-NAME.
Returns a plist with :host :port :database :user :password.
Resolves Docker containers to localhost with appropriate port.
Fetches all details from auth-source."
  (let* ((conn-name (if (stringp connection-name)
                        (intern connection-name)
                      connection-name))
         (conn-plist (cdr (assoc conn-name jf/postgres-connections)))
         (auth-key (or (plist-get conn-plist :auth-key) conn-name))
         (docker-name (plist-get conn-plist :docker-name)))

    ;; Retrieve details from auth-source
    (let ((auth-details (jf/postgres-get-auth-details auth-key)))
      (unless auth-details
        (error "No auth-source entry found for: %s. Check ~/.authinfo.gpg" auth-key))

      (let ((host (plist-get auth-details :host))
            (port (plist-get auth-details :port))
            (database (plist-get auth-details :database))
            (user (plist-get auth-details :user))
            (password (plist-get auth-details :password)))

        ;; Handle Docker containers (override host/port from auth-source)
        ;; Use 127.0.0.1 instead of localhost to force TCP connection (not Unix socket)
        (when docker-name
          (setq host "127.0.0.1")
          (setq port (jf/postgres-get-docker-port docker-name)))

        ;; Validate required fields
        (unless (and host database user)
          (error "Incomplete connection details for %s. Required: server, database, login in .authinfo.gpg" auth-key))

        (list :host host
              :port port
              :database database
              :user user
              :password password)))))

(defun jf/postgres-build-sql-connection (connection-name)
  "Build sql-connection-alist entry for CONNECTION-NAME.
Returns a connection specification suitable for sql-connection-alist."
  (let ((params (jf/postgres-get-connection-params connection-name)))
    `(,connection-name
      (sql-product 'postgres)
      (sql-server ,(plist-get params :host))
      (sql-port ,(plist-get params :port))
      (sql-database ,(plist-get params :database))
      (sql-user ,(plist-get params :user))
      (sql-password ,(plist-get params :password)))))

(defun jf/postgres-register-connections ()
  "Register all jf/postgres-connections into sql-connection-alist.
This makes them available to org-babel via :dbconnection parameter."
  (interactive)
  (when jf/postgres-connections
    (dolist (conn jf/postgres-connections)
      (let* ((name (car conn))
             (entry (jf/postgres-build-sql-connection name)))
        ;; Remove existing entry if present
        (setq sql-connection-alist
              (assq-delete-all name sql-connection-alist))
        ;; Add new entry
        (push entry sql-connection-alist)))
    (message "Registered %d PostgreSQL connection%s"
             (length jf/postgres-connections)
             (if (= 1 (length jf/postgres-connections)) "" "s"))))

;; Auto-register connections after loading sql.el
(with-eval-after-load 'sql
  (jf/postgres-register-connections))

(defun jf/postgres-list-connections ()
  "Display all registered PostgreSQL connections."
  (interactive)
  (if (not jf/postgres-connections)
      (message "No PostgreSQL connections defined. Set jf/postgres-connections in your config.")
    (with-output-to-temp-buffer "*PostgreSQL Connections*"
      (princ "Registered PostgreSQL Connections:\n\n")
      (dolist (conn jf/postgres-connections)
        (let* ((name (car conn))
               (plist (cdr conn))
               (host (plist-get plist :host))
               (port (or (plist-get plist :port) 5432))
               (database (plist-get plist :database))
               (user (plist-get plist :user)))
          (princ (format "  %s\n" name))
          (princ (format "    Host: %s:%s\n" host port))
          (princ (format "    Database: %s\n" database))
          (princ (format "    User: %s\n\n" user)))))))

(defun jf/postgres-test-connection (connection-name)
  "Test PostgreSQL connection CONNECTION-NAME.
Executes a simple query to verify connectivity."
  (interactive
   (list (completing-read "Test connection: "
                         (mapcar #'car jf/postgres-connections)
                         nil t)))
  (let* ((params (jf/postgres-get-connection-params connection-name))
         (host (plist-get params :host))
         (port (plist-get params :port))
         (database (plist-get params :database))
         (user (plist-get params :user))
         (password (plist-get params :password)))
    (message "Testing connection %s..." connection-name)
    (let* ((pgpassword-env (if password (format "PGPASSWORD=%s" password) ""))
           (cmd (format "%s psql -h %s -p %d -U %s -d %s -c 'SELECT version();' 2>&1"
                       pgpassword-env host port user database))
           (output (shell-command-to-string cmd)))
      (if (string-match-p "PostgreSQL" output)
          (message "✓ Connection successful: %s" connection-name)
        (message "✗ Connection failed: %s\n%s" connection-name output)))))

(defun jf/postgres-insert-src-block (connection-name)
  "Insert an org-babel SQL source block with CONNECTION-NAME."
  (interactive
   (list (completing-read "Database connection: "
                         (mapcar #'car jf/postgres-connections)
                         nil t)))
  (insert (format "#+begin_src sql :engine postgresql :dbconnection %s\n"
                  connection-name))
  (insert "SELECT current_database(), current_user, version();\n")
  (insert "#+end_src\n")
  (forward-line -2)
  (end-of-line))

(defun jf/postgres-connect (connection-name)
  "Connect to PostgreSQL database using CONNECTION-NAME from jf/postgres-connections.
Resolves connection parameters (Docker ports, auth-source passwords) and opens SQL buffer."
  (interactive
   (list (intern (completing-read "Database connection: "
                                   (mapcar #'car jf/postgres-connections)
                                   nil t))))
  (unless jf/postgres-connections
    (error "No PostgreSQL connections defined. Set jf/postgres-connections in your config."))

  ;; Ensure sql.el is loaded (defines sql-connection-alist)
  (require 'sql)

  ;; Get connection parameters
  (let* ((params (jf/postgres-get-connection-params connection-name))
         (password (plist-get params :password))
         (entry (jf/postgres-build-sql-connection connection-name)))

    ;; Remove old entry if present and add fresh one
    (setq sql-connection-alist
          (assq-delete-all connection-name sql-connection-alist))
    (push entry sql-connection-alist)

    ;; Set PGPASSWORD environment variable for this connection
    (when password
      (setenv "PGPASSWORD" password))

    ;; Connect using built-in sql-connect
    (unwind-protect
        (sql-connect connection-name)
      ;; Clear PGPASSWORD after connection for security
      (setenv "PGPASSWORD" nil))))

(defun jf/postgres-refresh-connections ()
  "Reload machine-specific config and re-register PostgreSQL connections."
  (interactive)
  (if (null jf/machine-role)
      (error "Machine role not set. Please create ~/.machine-role with one of: apploi-mac, personal-mac, personal-mac-air")
    (let ((config-file (expand-file-name
                        (format "local/%s.el" jf/machine-role)
                        jf/emacs-dir)))
      (if (file-exists-p config-file)
          (progn
            (load-file config-file)
            (jf/postgres-register-connections)
            (message "Refreshed PostgreSQL connections from %s" config-file))
        (error "Machine-specific config not found: %s" config-file)))))

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'"
  :config
  (setq csv-separators '("," ";" "|" "\t")))

(defun jf/postgres-check-psql-installed ()
  "Verify psql is installed and accessible.
Returns path to psql or signals an error."
  (let ((psql-path (executable-find "psql")))
    (unless psql-path
      (error "psql not found. Install PostgreSQL client with: brew install libpq"))
    psql-path))

;; Run check on load
(with-eval-after-load 'sql
  (jf/postgres-check-psql-installed))
