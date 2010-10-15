;;; stesla-python.el --- Python language customizations.

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

;; download and install Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; easy_install rope
;; easy_install ropemode
;; download and install ropemacs
(defun load-ropemacs ()
  "Load ropemacs"
  (interactive)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil))

(eval-after-load 'python
  '(progn
     (defun stesla-python-find-callers ()
       "Using PYTHON-WHICH-FUNC, determine the current function, and then run GREP-FIND to determine callsites of that function"
       (interactive)
       (let ((func (car (last (split-string (python-which-func) "[.]")))))
         (grep-find (concat "find . -type f -print0 | xargs -0 -e grep -nH -e "
                            "'\\b" func "('"))))
     (define-key python-mode-map (kbd "\C-c f") 'stesla-python-find-callers)
     (add-hook 'python-mode-hook 'flymake-mode)))

(eval-after-load 'flymake
  '(progn
     (defun flymake-pylint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "epylint" (list local-file))))
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init))))


(provide 'stesla-python)