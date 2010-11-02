;;; stesla-bindings.el --- Key bindings and register bindings.

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

(dolist (b `((,(kbd "C-c l") goto-line)
             (,(kbd "C-c c") compile)
             (,(kbd "C-c #") stesla-word-count)))
  (global-set-key (car b) (cadr b)))

(dolist (r `((?. (file . ,(concat dotfiles-dir "init.el")))
             (?b (file . ,(concat dotfiles-dir "stesla-bindings.el")))))
  (set-register (car r) (cadr r)))

(defun stesla-word-count ()
  (interactive)
  (if current-prefix-arg
      (shell-command "cat session*.tex | wc -w")
    (shell-command-on-region (point-min) (point-max) "wc -w")))

(provide 'stesla-bindings)