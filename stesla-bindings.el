(global-set-key (kbd "C-c l") 'goto-line)

(set-register ?. `(file . ,(concat dotfiles-dir "init.el")))

(provide 'stesla-bindings)