;; -*- lexical-binding: t; -*-
;; Machine-specific configuration for apploi-mac (Work MacBook Air)

;; Configure machine-specific daily journal directory
(setq org-roam-dailies-directory "dailies/apploi-mac/")

(setq browser-hist-db-paths
      '((chrome . "/Users/jefffarr/Library/Application Support/Google/Chrome/Profile 1/History")))

(setq browser-hist-default-browser 'chrome)

;; PostgreSQL connections - sensitive details stored in ~/.authinfo.gpg
;; Only connection names and Docker-specific metadata are here.
;; After updating connections, run: M-x jf/postgres-register-connections

(setq jf/postgres-connections
      '((pg-local-apollo-container
         :docker-name "postgres")
        (pg-devnew-rw
         :host "oncall-psql-devnew-20241205190430184400000002.cluster-cto8yse48viq.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-devnew-rw")
        (pg-bdrkdev-rw
         :host "oncall-psql-bdrkdev-20240920163715257100000013.cluster-cto8yse48viq.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-bdrkdev-rw")
        (pg-schedule-prod-rw
         :host "oncall-psql-prod-read-write.endpoint.proxy-cdvuqeybm7mr.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-schedule-prod-rw")
        (pg-schedule-prod-ro
         :host "oncall-psql-prod-20240209204443329600000006.cluster-ro-cdvuqeybm7mr.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-schedule-prod-ro")))
