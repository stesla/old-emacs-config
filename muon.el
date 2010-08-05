;;; muon.el --- A mode for interacting with MUSHes.

;; Copyright (C) 2010  Samuel Tesla

;; Author: Samuel Tesla <samuel@Inara.local>
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

(defvar muon-worlds
  '((gateway . (:host "connect.mu-gateway.net" :port 6700))))

(defun muon-get-world (world-name)
  (assoc world-name muon-worlds))

(defun muon-world-name (world)
  (car world))

(defun muon-world-host (world)
  (plist-get (cdr world) :host))

(defun muon-world-port (world)
  (plist-get (cdr world) :port))

(provide 'muon)
;;; muon.el ends here
