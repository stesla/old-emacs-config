;;; stesla-org.el --- org-mode customization

;; Copyright (C) 2010  Samuel Tesla

;; Author: Samuel Tesla <samuel@Dahlia>
;; Keywords: 

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

;;; Commentary:

;; 

;;; Code:

(require 'remember)

(setq org-directory "~/org/"
      org-default-notes-file "~/.notes"
      remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(global-set-key (kbd "C-c r") 'remember)

(setq org-refile-targets
      '(("general.org" :maxlevel . 1)
        ("home.org"    :maxlevel . 1)
        ("work.org"    :maxlevel . 1)))

(setq org-remember-templates
      '(("Todo" ?t "* TODO %? %^g\n %i\n" "inbox.org" "Tasks")
        ("Journal" ?j "* %^{Topic} %T \n%i%?\n" "journal.org")
        ("Idea" ?i "* %^{Topic} %T \n%i%?\n" "inbox.org" "Ideas")))

(provide 'stesla-org)
;;; stesla-org.el ends here
