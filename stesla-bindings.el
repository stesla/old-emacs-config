(dolist (b `((,(kbd "C-c l") goto-line)
             (,(kbd "C-c r") compile)))
  (global-set-key (car b) (cadr b)))

(dolist (r `((?. (file . ,(concat dotfiles-dir "init.el")))
             (?b (file . ,(concat dotfiles-dir "stesla-bindings.el")))))
  (set-register (car r) (cadr r)))

(provide 'stesla-bindings)