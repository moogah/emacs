(require 'json)
(require 'url)
(require 'url-http)
(require 'auth-source)

(defgroup jira-integration nil
  "Settings for Jira API integration."
  :group 'applications)

(defcustom jira-base-url "https://apploi.atlassian.net"
  "The base URL for your Jira instance.
   For Atlassian Cloud, this is typically https://your-company.atlassian.net"
  :type 'string
  :group 'jira-integration)

(defcustom jira-auth-source-key "id.atlassian.com"
  "The key to use when looking up Jira credentials in the auth-source."
  :type 'string
  :group 'jira-integration)

(defcustom jira-email ""
  "Your email address for Jira authentication."
  :type 'string
  :group 'jira-integration)

(defun jira-get-auth ()
  "Get Jira authentication from auth-source."
  (let ((auth-info (nth 0 (auth-source-search :host jira-auth-source-key))))
    (when auth-info
      (let ((login (plist-get auth-info :user))
            (password (funcall (plist-get auth-info :secret))))
        ;; If login is "apiKey", use jira-email + API key
        (if (string= login "apiKey")
            (cons jira-email password)
          ;; Otherwise use the login (email) and password directly
          (cons login password))))))

(defun jira-api-request (endpoint method &optional params data callback)
  "Make a request to the Jira API.
ENDPOINT is the API endpoint (without base URL).
METHOD is the HTTP method to use.
PARAMS is an alist of URL parameters to include.
DATA is the data to send (for POST/PUT requests).
CALLBACK is a function to call with the response."
  (let* ((auth (jira-get-auth))
         (url-request-method method)
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("User-Agent" . "Emacs-Jira-Client/1.0")))
         (url-request-data (when data
                             (encode-coding-string (json-encode data) 'utf-8)))
         (auth-string (when auth
                        (base64-encode-string
                         (format "%s:%s" (car auth) (cdr auth)))))
         ;; Build the URL with proper param encoding
         (url-params (when params
                       (concat "?" (mapconcat
                                   (lambda (param)
                                     (concat (url-hexify-string (car param))
                                             "="
                                             (url-hexify-string (cdr param))))
                                   params "&"))))
         (full-url (concat jira-base-url endpoint url-params)))
    
    ;; Log request info for debugging
    (message "Making %s request to %s" method full-url)
    (when auth
      (message "Using auth with username: %s" (car auth)))
    
    ;; Add Authorization header if auth is available
    (when auth-string
      (push (cons "Authorization" (format "Basic %s" auth-string))
            url-request-extra-headers))
    
    ;; Make the HTTP request and handle the response
    (url-retrieve full-url
                  (lambda (status callback)
                    (if (plist-get status :error)
                        (let ((error-details (plist-get status :error)))
                          (message "Error: %S" error-details))
                      ;; Request successful, parse JSON response
                      (progn
                        (message "Request succeeded")
                        (goto-char url-http-end-of-headers)
                        (condition-case err
                            (let ((json-response (json-read)))
                              (when callback
                                (funcall callback json-response)))
                          (error
                           (message "Error parsing JSON: %S" err)
                           (let ((raw-content (buffer-substring-no-properties
                                               url-http-end-of-headers (point-max))))
                             (message "Raw response: %s" raw-content)))))))
                  (list callback) t)))

(defun jira-get-my-issues (&optional callback)
  "Get issues assigned to the current user."
  (jira-api-request "/rest/api/2/search" "GET" 
                    '(("jql" . "assignee=currentUser()"))
                    nil
                    (lambda (data)
                      (message "Got issues data with %d issues" 
                               (or (cdr (assoc 'total data)) 0))
                      (when callback
                        (funcall callback data)))))

(defun jira-get-issue (issue-key &optional callback)
  "Get a specific issue by its key."
  (jira-api-request (format "/rest/api/2/issue/%s" issue-key) "GET" nil nil callback))

(defun jira-list-my-issues ()
  "Display issues assigned to the current user."
  (interactive)
  (jira-get-my-issues
   (lambda (data)
     (message "Raw data: %S" data)
     (let ((issues (append (cdr (assoc 'issues data)) nil))
           (total (cdr (assoc 'total data)))
           (buf (get-buffer-create "*Jira Issues*")))
       (with-current-buffer buf
         (erase-buffer)
         (insert (format "Found %d issues\n\n" (or total 0)))
         (if (not issues)
             (insert "No issues assigned to you.\n")
           (dolist (issue issues)
             (let ((key (cdr (assoc 'key issue)))
                   (summary (cdr (assoc 'summary (cdr (assoc 'fields issue))))))
               (insert (format "[%s] %s\n" key summary))))))
       (switch-to-buffer buf)))))

(defun jira-view-issue (issue-key)
  "View a specific Jira issue."
  (interactive "sIssue key: ")
  (jira-get-issue
   issue-key
   (lambda (data)
     (let* ((fields (cdr (assoc 'fields data)))
            (summary (cdr (assoc 'summary fields)))
            (description (cdr (assoc 'description fields)))
            (status (cdr (assoc 'name (cdr (assoc 'status fields)))))
            (buf (get-buffer-create (format "*Jira Issue: %s*" issue-key))))
       (with-current-buffer buf
         (erase-buffer)
         (insert (format "Issue: %s\n" issue-key))
         (insert (format "Summary: %s\n" summary))
         (insert (format "Status: %s\n\n" status))
         (insert "Description:\n")
         (insert (or description "No description provided.")))
       (switch-to-buffer buf)))))

(defun jira-test-auth ()
  "Test Jira authentication and display the result."
  (interactive)
  (let ((auth (jira-get-auth)))
    (if auth
        (message "Auth found. Username: %s, Password length: %d"
                 (car auth) (length (cdr auth)))
      (message "No auth info found. Check your ~/.authinfo.gpg file."))))

(defun jira-test-connection ()
  "Test connection to Jira API."
  (interactive)
  (message "Testing connection to %s..." jira-base-url)
  (jira-api-request "/rest/api/2/myself" "GET" nil nil
                    (lambda (data)
                      (message "Connection successful! User: %s (%s)"
                               (cdr (assoc 'displayName data))
                               (cdr (assoc 'emailAddress data))))))
  
(defun jira-test-search ()
  "Test the search API to find issues."
  (interactive)
  (message "Testing search API with JQL: assignee=currentUser()")
  (jira-api-request 
   "/rest/api/2/search"
   "GET" 
   '(("jql" . "assignee=currentUser()") 
     ("fields" . "summary,status"))
   nil
   (lambda (data)
     (let ((total (cdr (assoc 'total data)))
           (issues (cdr (assoc 'issues data))))
       (message "Search found %d issues" (or total 0))
       (when issues
         (message "First issue: %S" (nth 0 issues)))))))
  
(defun jira-explore-projects ()
  "List all accessible projects."
  (interactive)
  (jira-api-request "/rest/api/2/project" "GET" nil nil
                    (lambda (data)
                      (message "Found %d projects" (length data))
                      (let ((buf (get-buffer-create "*Jira Projects*")))
                        (with-current-buffer buf
                          (erase-buffer)
                          (insert "Available Jira Projects:\n\n")
                          (dolist (project data)
                            (let ((key (cdr (assoc 'key project)))
                                  (name (cdr (assoc 'name project))))
                              (insert (format "[%s] %s\n" key name)))))
                        (switch-to-buffer buf)))))

(provide 'jira)
