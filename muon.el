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

(eval-when-compile
  (require 'cl))

(defvar muon-worlds
  '(("gateway" . (:host "connect.mu-gateway.net" :port 6700))))

(defun muon-get-world (world-name)
  (assoc world-name muon-worlds))

(defun muon-world-name (world)
  (car world))

(defun muon-world-host (world)
  (plist-get (cdr world) :host))

(defun muon-world-port (world)
  (plist-get (cdr world) :port))

(defun muon (world-name)
  (interactive "sWorld: ")
  (let ((buffer (generate-new-buffer "*muon*")))
    (muon-open-connection buffer (muon-get-world world-name))
    (set-window-buffer (selected-window) buffer)))

(defun muon-open-connection (buffer world)
  (make-network-process :name (concat "muon " (muon-world-name world))
                        :buffer buffer
                        :filter 'muon-insertion-filter
                        :host (muon-world-host world)
                        :service (muon-world-port world)))

(defun muon-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (loop for x across string
              do (muon-insert proc x))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun muon-insert (proc byte)
  (cond
   ((process-get proc 'iac)
    (if (eq 255 byte)
        (insert 255)
      (process-put proc 'iac t)))
   ((process-get proc 'cr)
    (if (eq ?\n byte)
        (insert ?\n)
      (process-put proc 'cr t)))
   (t
    (insert byte))))

(provide 'muon)
;;; muon.el ends here
