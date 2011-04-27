;;; stesla-haskell.el -- Haskell customizations

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

(defconst haskell-dir (concat dotfiles-dir "haskellmode-emacs-2.7.0/"))
(add-to-list 'load-path haskell-dir)

(load (concat haskell-dir "haskell-site-file"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'stesla-haskell)