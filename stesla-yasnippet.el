;;; stesla-yasnippet.el --- YASnippet customizations.

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

(defconst yasnippet-version "yasnippet-0.6.1c")

(add-to-list 'load-path (concat dotfiles-dir yasnippet-version))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir yasnippet-version "/snippets"))

(provide 'stesla-yasnippet)