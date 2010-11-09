;;; stesla-words.el --- Code for writing prose

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

(defvar stesla-word-count-mode-line " WC:?")
(make-variable-buffer-local 'stesla-word-count-mode-line)

(defvar stesla-word-count-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c #") 'stesla-word-count)
    map))

(defvar stesla-word-count-command "cat * | wc -w")

(defun stesla-word-count ()
  (interactive)
  (let ((command (if current-prefix-arg
                     (setq stesla-word-count-command
                           (read-string "Command: " stesla-word-count-command))
                   stesla-word-count-command)))
    (shell-command stesla-word-count-command)))

(define-minor-mode stesla-word-count-mode
  "A mode for counting words"
  :lighter stesla-word-count-mode-line
  :keymap stesla-word-count-mode-map
  (if stesla-word-count-mode
      (condition-case ()
          (stesla-word-count-mode-on)
        (error (message "Enabling Word Count mode gave an error")))
    (stesla-word-count-mode-off)))

(defun stesla-word-count-mode-on ()
  (add-hook 'after-change-functions 'stesla-word-count-after-change-function t t)
  (stesla-word-count-update-mode-line))

(defun stesla-word-count-mode-off ()
  (remove-hook 'after-change-functions 'stesla-word-count-after-change-function t))

(defun stesla-word-count-after-change-function (start stop len)
  (stesla-word-count-update-mode-line))

(defun stesla-word-count-update-mode-line ()
  (let ((count (stesla-word-count-in-region (point-min) (point-max))))
    (setq stesla-word-count-mode-line (format " WC:%d" count))
    (force-mode-line-update)))

(defun stesla-word-count-in-region (start end)
  (length
   (split-string
    (buffer-substring-no-properties start end))))

(provide 'stesla-words)