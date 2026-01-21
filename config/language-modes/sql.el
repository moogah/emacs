;; -*- lexical-binding: t; -*-

(defvar jf/sql-connections nil
  "Alist of SQL database connection definitions.
Each entry has the form (NAME . PLIST) where PLIST contains:
  :sql-product - Database product (postgres, mysql, sqlite, oracle, sqlserver) (required)
  :host        - Hostname or 'docker for containers (required for most products)
  :port        - Port number (product-specific defaults)
  :database    - Database name (required)
  :user        - Username (required for most products)
  :docker-name - Docker container name (required if :host is 'docker)
  :auth-key    - Host key for auth-source lookup (overrides default lookup)

Example:
  (setq jf/sql-connections
        '((pg-local-dev
           :sql-product postgres
           :host \"localhost\"
           :port 5432
           :database \"dev_db\"
           :user \"postgres\"
           :auth-key \"pg-local-dev\")
          (mysql-prod
           :sql-product mysql
           :host \"prod.example.com\"
           :port 3306
           :database \"app_db\"
           :user \"appuser\"
           :auth-key \"mysql-prod\")
          (sqlite-local
           :sql-product sqlite
           :database \"/path/to/database.db\")
          (docker-postgres
           :sql-product postgres
           :host docker
           :docker-name \"my-postgres-container\"
           :database \"testdb\"
           :user \"postgres\"
           :auth-key \"docker-postgres\")))")

(defun jf/sql-get-auth-details (auth-key sql-product)
  "Retrieve all authentication details for AUTH-KEY from auth-source.
Returns a plist with :user :password :host :port :database, or nil if not found.

SQL-PRODUCT is used to determine default port if not specified in auth-source.
AUTH-KEY can be a symbol or string.

Auth-source looks up AUTH-KEY in ~/.authinfo.gpg using the 'machine' field.
Custom netrc properties (server, database) are preserved in the returned plist."
  ;; Convert auth-key to string (auth-source-search requires string for :host)
  (let ((host-key (if (stringp auth-key) auth-key (symbol-name auth-key))))
    (when-let* ((auth-info (car (auth-source-search :host host-key :require '(:secret))))
                (secret (plist-get auth-info :secret))
                (password (if (functionp secret) (funcall secret) secret)))
      (let* ((port-value (plist-get auth-info :port))
             (default-port (pcase sql-product
                             ('postgres 5432)
                             ('mysql 3306)
                             ('oracle 1521)
                             ('sqlserver 1433)
                             (_ nil))))
        (list :user (plist-get auth-info :user)
              :password password
              :host (or (plist-get auth-info :server) "127.0.0.1")
              :port (if port-value
                        (if (stringp port-value)
                            (string-to-number port-value)
                          port-value)
                      default-port)
              :database (plist-get auth-info :database))))))

(defun jf/sql-get-docker-port (container-name internal-port)
  "Get the host port mapped to INTERNAL-PORT in CONTAINER-NAME.
Returns port number or INTERNAL-PORT if container is not found or port not mapped."
  (let* ((cmd (format "docker port %s %d 2>/dev/null | cut -d: -f2"
                     (shell-quote-argument container-name)
                     internal-port))
         (output (string-trim (shell-command-to-string cmd))))
    (if (string-empty-p output)
        (progn
          (warn "Could not determine port for Docker container: %s (port %d). Using default %d."
                container-name internal-port internal-port)
          internal-port)
      (string-to-number output))))

(defun jf/sql-get-connection-params (connection-name)
  "Get connection parameters for CONNECTION-NAME.
Returns a plist with :sql-product :host :port :database :user :password.
Resolves Docker containers to localhost with appropriate port.
Fetches all details from auth-source.

For SQLite databases, :database contains the file path and :host/:port/:user are nil."
  (let* ((conn-name (if (stringp connection-name)
                        (intern connection-name)
                      connection-name))
         (conn-plist (cdr (assoc conn-name jf/sql-connections)))
         (sql-product (plist-get conn-plist :sql-product))
         (auth-key (or (plist-get conn-plist :auth-key) conn-name))
         (docker-name (plist-get conn-plist :docker-name)))

    (unless sql-product
      (error "Connection %s missing :sql-product. Must be one of: postgres, mysql, sqlite, oracle, sqlserver" conn-name))

    ;; SQLite special case: file-based, no server/auth
    (if (eq sql-product 'sqlite)
        (let ((database (plist-get conn-plist :database)))
          (unless database
            (error "SQLite connection %s missing :database (file path)" conn-name))
          (list :sql-product sql-product
                :database database
                :host nil
                :port nil
                :user nil
                :password nil))

      ;; Server-based databases: retrieve from auth-source
      (let ((auth-details (jf/sql-get-auth-details auth-key sql-product)))
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
            (setq port (jf/sql-get-docker-port docker-name port)))

          ;; Validate required fields
          (unless (and host database user)
            (error "Incomplete connection details for %s. Required: server, database, login in .authinfo.gpg" auth-key))

          (list :sql-product sql-product
                :host host
                :port port
                :database database
                :user user
                :password password))))))

(defun jf/sql-build-connection (connection-name)
  "Build sql-connection-alist entry for CONNECTION-NAME.
Returns a connection specification suitable for sql-connection-alist.
Handles all supported database products (postgres, mysql, sqlite, oracle, sqlserver)."
  (let* ((params (jf/sql-get-connection-params connection-name))
         (sql-product (plist-get params :sql-product)))
    (if (eq sql-product 'sqlite)
        ;; SQLite: only database (file path)
        `(,connection-name
          (sql-product ',sql-product)
          (sql-database ,(plist-get params :database)))
      ;; Server-based databases
      `(,connection-name
        (sql-product ',sql-product)
        (sql-server ,(plist-get params :host))
        (sql-port ,(plist-get params :port))
        (sql-database ,(plist-get params :database))
        (sql-user ,(plist-get params :user))
        (sql-password ,(plist-get params :password))))))

(defun jf/sql-register-connections ()
  "Register all jf/sql-connections into sql-connection-alist.
This makes them available to org-babel via :dbconnection parameter."
  (interactive)
  (when jf/sql-connections
    (dolist (conn jf/sql-connections)
      (let* ((name (car conn))
             (entry (jf/sql-build-connection name)))
        ;; Remove existing entry if present
        (setq sql-connection-alist
              (assq-delete-all name sql-connection-alist))
        ;; Add new entry
        (push entry sql-connection-alist)))
    (message "Registered %d SQL connection%s"
             (length jf/sql-connections)
             (if (= 1 (length jf/sql-connections)) "" "s"))))

;; Auto-register connections after loading sql.el
(with-eval-after-load 'sql
  (jf/sql-register-connections))

(defun jf/sql-list-connections ()
  "Display all registered SQL connections."
  (interactive)
  (if (not jf/sql-connections)
      (message "No SQL connections defined. Set jf/sql-connections in your config.")
    (with-output-to-temp-buffer "*SQL Connections*"
      (princ "Registered SQL Connections:\n\n")
      (dolist (conn jf/sql-connections)
        (let* ((name (car conn))
               (plist (cdr conn))
               (sql-product (plist-get plist :sql-product))
               (host (plist-get plist :host))
               (port (plist-get plist :port))
               (database (plist-get plist :database))
               (user (plist-get plist :user)))
          (princ (format "  %s (%s)\n" name sql-product))
          (if (eq sql-product 'sqlite)
              (princ (format "    Database: %s\n\n" database))
            (princ (format "    Host: %s:%s\n" host (or port "default")))
            (princ (format "    Database: %s\n" database))
            (princ (format "    User: %s\n\n" user))))))))

(defun jf/sql-test-connection (connection-name)
  "Test SQL connection CONNECTION-NAME.
Executes a simple query to verify connectivity."
  (interactive
   (list (completing-read "Test connection: "
                         (mapcar #'car jf/sql-connections)
                         nil t)))
  (let* ((params (jf/sql-get-connection-params connection-name))
         (sql-product (plist-get params :sql-product)))
    (message "Testing connection %s (%s)..." connection-name sql-product)
    (pcase sql-product
      ('postgres (jf/sql--test-postgres params connection-name))
      ('mysql (jf/sql--test-mysql params connection-name))
      ('sqlite (jf/sql--test-sqlite params connection-name))
      (_ (message "✗ Test not implemented for %s" sql-product)))))

(defun jf/sql--test-postgres (params connection-name)
  "Test PostgreSQL connection with PARAMS and CONNECTION-NAME."
  (let* ((host (plist-get params :host))
         (port (plist-get params :port))
         (database (plist-get params :database))
         (user (plist-get params :user))
         (password (plist-get params :password))
         (pgpassword-env (if password (format "PGPASSWORD=%s" password) ""))
         (cmd (format "%s psql -h %s -p %d -U %s -d %s -c 'SELECT version();' 2>&1"
                     pgpassword-env host port user database))
         (output (shell-command-to-string cmd)))
    (if (string-match-p "PostgreSQL" output)
        (message "✓ Connection successful: %s" connection-name)
      (message "✗ Connection failed: %s\n%s" connection-name output))))

(defun jf/sql--test-mysql (params connection-name)
  "Test MySQL connection with PARAMS and CONNECTION-NAME."
  (let* ((host (plist-get params :host))
         (port (plist-get params :port))
         (database (plist-get params :database))
         (user (plist-get params :user))
         (password (plist-get params :password))
         (cmd (format "mysql -h %s -P %d -u %s -p%s -D %s -e 'SELECT version();' 2>&1"
                     host port user password database))
         (output (shell-command-to-string cmd)))
    (if (string-match-p "^[0-9]+\\.[0-9]+" output)
        (message "✓ Connection successful: %s" connection-name)
      (message "✗ Connection failed: %s\n%s" connection-name output))))

(defun jf/sql--test-sqlite (params connection-name)
  "Test SQLite connection with PARAMS and CONNECTION-NAME."
  (let* ((database (plist-get params :database))
         (cmd (format "sqlite3 %s 'SELECT sqlite_version();' 2>&1"
                     (shell-quote-argument database)))
         (output (shell-command-to-string cmd)))
    (if (string-match-p "^[0-9]+\\.[0-9]+" output)
        (message "✓ Connection successful: %s (SQLite %s)" connection-name (string-trim output))
      (message "✗ Connection failed: %s\n%s" connection-name output))))

(defun jf/sql-insert-src-block (connection-name)
  "Insert an org-babel SQL source block with CONNECTION-NAME."
  (interactive
   (list (completing-read "Database connection: "
                         (mapcar #'car jf/sql-connections)
                         nil t)))
  (let* ((conn-plist (cdr (assoc (intern connection-name) jf/sql-connections)))
         (sql-product (plist-get conn-plist :sql-product))
         (engine (pcase sql-product
                   ('postgres "postgresql")
                   ('mysql "mysql")
                   ('sqlite "sqlite")
                   ('oracle "oracle")
                   ('sqlserver "ms")
                   (_ "sql"))))
    (insert (format "#+begin_src sql :engine %s :dbconnection %s\n"
                    engine connection-name))
    (insert "SELECT current_database(), current_user, version();\n")
    (insert "#+end_src\n")
    (forward-line -2)
    (end-of-line)))

(defun jf/sql-connect (connection-name)
  "Connect to SQL database using CONNECTION-NAME from jf/sql-connections.
Resolves connection parameters (Docker ports, auth-source passwords) and opens SQL buffer."
  (interactive
   (list (intern (completing-read "Database connection: "
                                   (mapcar #'car jf/sql-connections)
                                   nil t))))
  (unless jf/sql-connections
    (error "No SQL connections defined. Set jf/sql-connections in your config."))

  ;; Ensure sql.el is loaded (defines sql-connection-alist)
  (require 'sql)

  ;; Get connection parameters
  (let* ((params (jf/sql-get-connection-params connection-name))
         (sql-product (plist-get params :sql-product))
         (password (plist-get params :password))
         (entry (jf/sql-build-connection connection-name)))

    ;; Remove old entry if present and add fresh one
    (setq sql-connection-alist
          (assq-delete-all connection-name sql-connection-alist))
    (push entry sql-connection-alist)

    ;; Set product-specific environment variables for password
    (when password
      (pcase sql-product
        ('postgres (setenv "PGPASSWORD" password))
        ('mysql (setenv "MYSQL_PWD" password))))

    ;; Connect using built-in sql-connect
    (unwind-protect
        (sql-connect connection-name)
      ;; Clear password environment variables after connection for security
      (setenv "PGPASSWORD" nil)
      (setenv "MYSQL_PWD" nil))))

(defun jf/sql-refresh-connections ()
  "Reload machine-specific config and re-register SQL connections."
  (interactive)
  (if (null jf/machine-role)
      (error "Machine role not set. Please create ~/.machine-role with one of: apploi-mac, personal-mac, personal-mac-air")
    (let ((config-file (expand-file-name
                        (format "config/local/%s.el" jf/machine-role)
                        jf/emacs-dir)))
      (if (file-exists-p config-file)
          (progn
            (load-file config-file)
            (jf/sql-register-connections)
            (message "Refreshed SQL connections from %s" config-file))
        (error "Machine-specific config not found: %s" config-file)))))

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'"
  :config
  (setq csv-separators '("," ";" "|" "\t")))

(defun jf/sql-check-cli-tools ()
  "Verify that SQL CLI tools are installed for configured connections.
Reports availability of psql, mysql, sqlite3, sqlplus, sqlcmd."
  (interactive)
  (let ((tools '(("psql" . "PostgreSQL")
                 ("mysql" . "MySQL/MariaDB")
                 ("sqlite3" . "SQLite")
                 ("sqlplus" . "Oracle")
                 ("sqlcmd" . "SQL Server")))
        (results '()))
    (dolist (tool tools)
      (let ((cmd (car tool))
            (product (cdr tool)))
        (if (executable-find cmd)
            (push (format "✓ %s (%s)" product cmd) results)
          (push (format "✗ %s (%s) not found" product cmd) results))))
    (if (called-interactively-p 'any)
        (message "%s" (mapconcat #'identity (nreverse results) "\n"))
      (nreverse results))))

;; Run check on load (only reports errors)
(with-eval-after-load 'sql
  (let ((results (jf/sql-check-cli-tools)))
    (dolist (result results)
      (when (string-prefix-p "✗" result)
        (message "%s" result)))))
