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

(defvar muon-input-data ""
  "A string used to store characters until a newline is encountered.")
(make-variable-buffer-local 'muon-input-buffer)

(defvar muon-input-pos 0
  "The index into MUON-INPUT-DATA where the next character should be placed.")
(make-variable-buffer-local 'muon-input-pos)

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
    (setq muon-input-data (make-empty-input-data))
    (setq muon-input-pos 0)
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
        (let ((inhibit-read-only t))
          (goto-char (process-mark proc))
          (loop for x across string
                do (muon-telnet-process proc x))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

;; (defconst muon-se 240)
;; (defconst muon-nop 241)
;; (defconst muon-dm 242)
;; (defconst muon-brk 243)
;; (defconst muon-ip 244)
;; (defconst muon-ao 245)
;; (defconst muon-ayt 246)
;; (defconst muon-ec 247)
;; (defconst muon-el 248)
;; (defconst muon-ga 249)
;; (defconst muon-sb 250)
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
  (aset muon-input-data muon-input-pos byte)
  (incf muon-input-pos)
  (cond
   ((eq ?\n byte)
    (insert (substring muon-input-data 0 muon-input-pos))
    (setq muon-input-pos 0))
   ((eq muon-input-pos (length muon-input-data))
    (setq muon-input-data (concat muon-input-data (make-empty-input-data))))))

(defun make-empty-input-data ()
  (make-string 1024 0))

(defun muon-send-input-line (line)
  (let ((process (get-buffer-process (current-buffer)))
        (telnet-line line))
    (process-send-string telnet-line)))

(define-derived-mode muon-mode
  nil "Muon"
  "Major mode for MUSHing.
\\{muon-mode-map}"
  (setq buffer-read-only t))

(provide 'muon)
;;; muon.el ends here
