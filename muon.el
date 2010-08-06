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
    (set-buffer buffer)
    (muon-mode)
    (switch-to-buffer buffer)))

(defun muon-open-connection (buffer world)
  (make-network-process :name (concat "muon " (muon-world-name world))
                        :buffer buffer
                        :coding '(no-conversion . no-conversion)
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
              do (muon-telnet-process proc x))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defconst muon-se 240)
(defconst muon-nop 241)
(defconst muon-dm 242)
(defconst muon-brk 243)
(defconst muon-ip 244)
(defconst muon-ao 245)
(defconst muon-ayt 246)
(defconst muon-ec 247)
(defconst muon-el 248)
(defconst muon-ga 249)
(defconst muon-sb 250)
(defconst muon-will 251)
(defconst muon-wont 252)
(defconst muon-do 253)
(defconst muon-dont 254)
(defconst muon-iac 255)

(defun muon-telnet-process (proc byte)
  (cond
   ((process-get proc 'iac)
    (cond
     ((or (eq muon-will byte)
          (eq muon-wont byte)
          (eq muon-do byte)
          (eq muon-dont byte))
      nil)
     ((eq muon-iac byte)
      (muon-insert muon-iac)
      (process-put proc 'iac nil))
     (t
      (process-put proc 'iac nil))))
   ((process-get proc 'cr)
    (unless (eq ?\n byte)
      (muon-insert byte))
    (process-put proc 'cr nil))
   ((eq muon-iac byte)
    (process-put proc 'iac t))
   ((eq ?\r byte)
    (muon-insert ?\n)
    (process-put proc 'cr t))
   (t
    (muon-insert byte))))

(defun muon-insert (byte)
  (insert byte))

(define-derived-mode muon-mode
  text-mode "Muon"
  "Major mode for MUSHing.
\\{muon-mode-map}")

(provide 'muon)
;;; muon.el ends here
