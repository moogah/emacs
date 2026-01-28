;; -*- lexical-binding: t; -*-
;; Machine-specific configuration for apploi-mac (Work MacBook Air)

;; Configure machine-specific daily journal directory
(setq org-roam-dailies-directory "dailies/apploi-mac/")

;; Projectile: Use machine-specific bookmarks file
;; This overrides the default state/projectile/projectile-bookmarks.eld
(setq projectile-known-projects-file
      (expand-file-name "state/projectile/apploi-mac-bookmarks.eld" jf/emacs-dir))

;; Bookmarks: Use machine-specific bookmarks file
;; This overrides the default state/bookmarks/bookmarks
(setq bookmark-default-file
      (expand-file-name "state/bookmarks/apploi-mac-bookmarks" jf/emacs-dir))

;; Activities: Use machine-specific activities file
;; This overrides the default state/activities/activities-activities
(put 'activities-activities 'persist-location
     (expand-file-name "state/activities/apploi-mac-activities" jf/emacs-dir))

(setq browser-hist-db-paths
      '((chrome . "/Users/jefffarr/Library/Application Support/Google/Chrome/Profile 1/History")))

(setq browser-hist-default-browser 'chrome)

;; SQL database connections - sensitive details stored in ~/.authinfo.gpg
;; Only connection names, sql-product, and Docker-specific metadata are here.
;; After updating connections, run: M-x jf/sql-register-connections

(setq jf/sql-connections
      '((pg-local-apollo-container
         :sql-product postgres
         :docker-name "postgres")
        (pg-devnew-rw
         :sql-product postgres
         :host "oncall-psql-devnew-20241205190430184400000002.cluster-cto8yse48viq.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-devnew-rw")
        (pg-bdrkdev-rw
         :sql-product postgres
         :host "oncall-psql-bdrkdev-20240920163715257100000013.cluster-cto8yse48viq.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-bdrkdev-rw")
        (pg-schedule-prod-rw
         :sql-product postgres
         :host "oncall-psql-prod-read-write.endpoint.proxy-cdvuqeybm7mr.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-schedule-prod-rw")
        (pg-schedule-prod-ro
         :sql-product postgres
         :host "oncall-psql-prod-20240209204443329600000006.cluster-ro-cdvuqeybm7mr.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-schedule-prod-ro")))
