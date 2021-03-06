;;; init.el -- Emacs Initialization

;; Copyright (C) 2010 Samuel Tesla

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

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))

;; Load commonly used packages
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Load up customizations
(require 'stesla-colors)
(require 'stesla-misc)
(require 'stesla-bindings)
(require 'stesla-python)
(require 'stesla-yasnippet)
(require 'stesla-auto)
(require 'stesla-elisp)
(require 'stesla-slime)
(require 'stesla-clojure)
(require 'stesla-git)
(require 'stesla-objection)
(require 'stesla-go)
(require 'stesla-org)
(require 'nanowrimode)
(require 'stesla-caml)
(require 'stesla-haskell)
(require 'stesla-javascript)
(require 'stesla-erlang)

;; We don't want a FQDN for system-name, just the hostname portion
(setq system-name (car (split-string system-name "[.]")))

;; System- and User-specific customizations
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
