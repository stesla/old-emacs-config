;;; stesla-misc.el --- Miscellaneous config that doesn't fit elsewhere.

;; Copyright (C) 2010  Samuel Tesla

;; Author: Samuel Tesla <samuel@alieniloquent.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

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
      '(Buffer-menu-mode-hook text-mode-hook
        custom-mode-hook term-mode-hook Info-mode-hook
        comint-mode-hook buffer-menu-mode-hook apropos-mode-hook
        tooltip-show-hook gnus-article-mode-hook mail-mode-hook
        gnus-summary-mode-hook message-mode-hook gnus-group-mode-hook
        eshell-mode-hook w3-mode-hook w3m-mode-hook help-modeq erc-mode))

(defun stesla-delete-trailing-whitespace ()
  "Delete all trailing whitespace in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[\t ]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

;; Automatically pick up changes on the filesystem
(global-auto-revert-mode 1)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Show the column number in the modeline
(column-number-mode 1)

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

;; Don't clutter up directories with files~ or #files#
(let ((backup-dir (expand-file-name (concat dotfiles-dir "backups/"))))
  (setq backup-directory-alist `((".*" . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,backup-dir t))))

(setq x-select-enable-clipboard t)

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'stesla-misc)