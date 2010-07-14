;;;;
;;;; MISC
;;;;

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      kill-whole-line t
      confirm-kill-emacs 'yes-or-no-p)

;; Trailing whitespace is evil, but evil is relative.
(setq-default show-trailing-whitespace t)

(defun stesla-hide-trailing-whitespace ()
  "Turn off trailing whitespace highlighting in this buffer."
  (interactive)
  (setq show-trailing-whitespace nil))

(mapc (lambda (mode-hook)
        (add-hook mode-hook 'stesla-hide-trailing-whitespace))
      '(Buffer-menu-mode-hook
        custom-mode-hook term-mode-hook Info-mode-hook
        comint-mode-hook buffer-menu-mode-hook apropos-mode-hook
        tooltip-show-hook gnus-article-mode-hook mail-mode-hook
        gnus-summary-mode-hook message-mode-hook gnus-group-mode-hook
        eshell-mode-hook w3-mode-hook w3m-mode-hook help-modeq erc-mode))

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(setq x-select-enable-clipboard t)

(provide 'stesla-misc)