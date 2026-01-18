(use-package oauth2
  :straight t)

(defun reddit-oauth2-script-authenticate ()
  "Authenticate with Reddit and return the access token."
  (let* ((client-id "HOzYYXYLapvLvebJUtNo1A")
         (client-secret "8ltNl1OTfr8Ci-lJXex2wTf2ZUD5PA")
         (username "moogah")
         (password "CX:2Rf2d39s9XL8")
         (token-url "https://www.reddit.com/api/v1/access_token")
         (auth (concat "Basic " (base64-encode-string (concat client-id ":" client-secret))))
         (url-request-method "POST")
         (url-request-extra-headers `(("Authorization" . ,auth)
                                      ("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (concat "grant_type=password&username=" (url-hexify-string username) 
                                  "&password=" (url-hexify-string password))))


   (with-current-buffer (url-retrieve-synchronously token-url)
     (goto-char (point-min))
     (re-search-forward "^$")
     (delete-region (point) (point-min))
     (json-read))))

(defun fetch-reddit-upvoted-posts ()
  "Fetches upvoted posts from Reddit."
  (interactive)
  (let* ((auth-data (reddit-oauth2-script-authenticate))
         (access-token (cdr (assoc 'access_token auth-data)))
         (url-request-method "GET")
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " access-token))
                                      ("User-Agent" . "MoogScrape/0.1 by moogah")))
         (url "https://oauth.reddit.com/user/moogah/upvoted")) ;; Replace with your username
   ;; Print URL and Headers for debugging
   (message "URL: %s" url)
   (message "Headers: %s" (format "%s" url-request-extra-headers))
    ;; Make the request
   (with-current-buffer (url-retrieve-synchronously url)
     (buffer-substring-no-properties (point-min) (point-max)))))
     ;;(let ((response (buffer-string)))
     ;;(message "Full Response:\n%s" response)))))
     ;;(print response)))))

(defun parse-reddit-response (response)
  "Parses the response from Reddit into headers and JSON body."
  (let ((header-lines (split-string response "\n\n" t))
        headers json-body)
    ;; Extract and parse headers
    (setq headers (mapcar (lambda (line) (split-string line ": " t))
                          (split-string (car header-lines) "\n" t)))
    ;; Extract JSON body
    (setq json-body (car (cdr header-lines)))
    ;; Parse JSON body
    (setq json-body (json-read-from-string json-body))
    ;; Return parsed headers and JSON body
    (list :headers headers :json json-body)))

(defun get-http-status-code (headers)
  "Prints the HTTP status code from HEADERS."
  (let* ((status-line (car (car headers)))  ; Get the first header
         (status-parts (split-string status-line " "))  ; Split the status line
         (status-code (nth 1 status-parts)))  ; Get the status code
    status-code))
