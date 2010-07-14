(defconst yasnippet-version "yasnippet-0.6.1c")

(add-to-list 'load-path (concat vendor-dir yasnippet-version))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat vendor-dir yasnippet-version "/snippets"))

(provide 'stesla-yasnippet)