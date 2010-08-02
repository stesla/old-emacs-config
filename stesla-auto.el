(require 'autoinsert)
(auto-insert-mode)

(setq auto-insert-directory (concat dotfiles-dir "templates/"))
(setq auto-insert-query nil)

(define-auto-insert "\.py" "python.py")