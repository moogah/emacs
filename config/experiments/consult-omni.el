(use-package consult-omni
        :straight (consult-omni :type git :host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el"))
        :after consult)
(require 'consult-omni-sources)
(consult-omni-sources-load-modules)
