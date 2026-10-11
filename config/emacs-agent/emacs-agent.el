;;; emacs-agent.el --- Agent-facing dispatcher, presets and target support -*- lexical-binding: t; -*-

(jf/load-module (expand-file-name "config/emacs-agent/dispatcher.el"          jf/emacs-dir))
(jf/load-module (expand-file-name "config/emacs-agent/presets.el"             jf/emacs-dir))
(jf/load-module (expand-file-name "config/emacs-agent/manifest.el"            jf/emacs-dir))
(jf/load-module (expand-file-name "config/emacs-agent/introspection-tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/emacs-agent/action-log.el"          jf/emacs-dir))
(jf/load-module (expand-file-name "config/emacs-agent/target-support.el"      jf/emacs-dir))

(provide 'emacs-agent)
;;; emacs-agent.el ends here
