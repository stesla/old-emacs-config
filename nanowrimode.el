;;; nanowrimode.el -- I write my NaNovel in Emacs

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

(defvar nanowrimode-line " NaNo")
(make-variable-buffer-local 'nanowrimode-line)

(defvar nanowrimode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c #") 'nanowrimode-count)
    map))

(defvar nanowrimode-count 0)
(make-variable-buffer-local 'nanowrimode-count)
(defvar nanowrimode-count-command-history '("cat * | wc -w"))
(defun nanowrimode-count ()
  (interactive)
  (let* ((command (read-shell-command "Command: "
                                      (car nanowrimode-count-command-history)
                                      '(nanowrimode-count-command-history . 1)))
         (count (string-to-number (shell-command-to-string command))))
    (setq nanowrimode-count count)
    (put 'nanowrimode-count 'currentp t)
    (message (format "%d" count))
    (nanowrimode-update-mode-line)))

(define-minor-mode nanowrimode
  "A mode for counting words"
  :lighter nanowrimode-line
  :keymap nanowrimode-map
  (if nanowrimode
      (condition-case ()
          (nanowrimode-on)
        (error (message "Enabling NaNoWriMode mode gave an error")))
    (nanowrimode-off)))

(defun nanowrimode-on ()
  (add-hook 'after-change-functions 'nanowrimode-after-change-function t t)
  (nanowrimode-update-mode-line))

(defun nanowrimode-off ()
  (remove-hook 'after-change-functions 'nanowrimode-after-change-function t))

(defun nanowrimode-after-change-function (start stop len)
  (put 'nanowrimode-count 'currentp nil)
  (nanowrimode-update-mode-line))

(defun nanowrimode-daily-goal ()
  (let ((day (string-to-number (format-time-string "%d" (current-time)))))
    (- (* day 1667) (/ day 3))))

(defun nanowrimode-update-mode-line ()
  (setq nanowrimode-line
        (format " NaNo(%d:%d%s:%d)"
                (nanowrimode-words-in-region (point-min) (point-max))
                nanowrimode-count
                (if (get 'nanowrimode-count 'currentp) "" "*")
                (nanowrimode-daily-goal)))
  (force-mode-line-update))

(defun nanowrimode-words-in-region (start end)
  (length
   (split-string
    (buffer-substring-no-properties start end))))

(provide 'nanowrimode)