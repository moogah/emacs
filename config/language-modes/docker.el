;; -*- lexical-binding: t; -*-

(defvar docker-compose-cli-path
  "/usr/local/bin/docker-compose"
  "Path to the docker compose executable.")

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :straight t)
