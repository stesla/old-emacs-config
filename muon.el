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

;;;; User Settings

(defvar muon-worlds
  '(("gateway" . (:host "connect.mu-gateway.net" :port 6700))))

(defvar muon-prompt "MU>"
  "The string displayed as the input prompt in Muon buffers")

(defun muon-prompt ()
  (let ((prompt (if (functionp muon-prompt)
                    (funcall muon-prompt)
                  muon-prompt)))
    (if (> (length prompt) 0)
        (concat prompt " ")
      prompt)))

(defgroup muon-faces nil
  "Faces for Muon")

(defface muon-prompt-face
  '((t (:bold t :foreground "Black" :background "lightBlue2")))
  "Muon face used for the prompt."
  :group 'muon-faces)

;;;; Internal Variables

(defvar muon-server-process nil
  "The sever process for this Muon buffer.")
(make-variable-buffer-local 'muon-server-process)

(defvar muon-insert-bufstr nil
  "A string used to store characters until a newline is encountered.")
(make-variable-buffer-local 'muon-insert-bufstr)

(defvar muon-insert-bufstr-pos nil
  "The index into MUON-INSERT-BUFSTR where the next character should be placed.")
(make-variable-buffer-local 'muon-insert-bufstr-pos)

(defvar muon-insert-marker nil
  "A marker to tell Muon where to insert text received from the server.")
(make-variable-buffer-local 'muon-insert-marker)

(defvar muon-input-marker nil
  "A marker to tell Muon where user input starts.")
(make-variable-buffer-local 'muon-input-marker)

;;;; Interactive functions

(define-derived-mode muon-mode
  nil "Muon"
  "Major mode for MUSHing.
\\{muon-mode-map}")

(define-key muon-mode-map "\C-m" 'muon-send-current-line)
(define-key muon-mode-map "\C-a" 'muon-bol)
(define-key muon-mode-map [home] 'muon-bol)
(define-key muon-mode-map "\C-c\C-a" 'muon-bol)

(defun muon (world-name)
  (interactive "sWorld: ")
  (let* ((buffer (generate-new-buffer "*muon*"))
         (process (muon-open-connection buffer (muon-get-world world-name))))
    (set-buffer buffer)

    ;; Set modes
    (muon-mode)
    (visual-line-mode)

    ;; Initialize variables
    (setq muon-server-process process)
    (setq muon-insert-bufstr (make-bufstr))
    (setq muon-insert-bufstr-pos 0)
    (setq muon-insert-marker (make-marker))
    (setq muon-input-marker (make-marker))

    ;; I'm unsure *why* but if I don't put this in, the READ-ONLY
    ;; property on the prompt causes KILL-BUFFER to signal a
    ;; TEXT-READ-ONLY error the first time I call it.
    (goto-char (point-max))
    (forward-line 0)
    (insert "\n")

    (set-marker muon-insert-marker (point))

    (muon-display-prompt)

    (switch-to-buffer buffer)))

(defun muon-bol ()
  (interactive)
  (forward-line 0)
  (when (get-text-property (point) 'muon-prompt)
    (goto-char muon-input-marker))
  (point))

(defun muon-kill-input ()
  (interactive)
  (when (and (muon-bol)
             (/= (point) (point-max)))
    (kill-line)))

;;;; Input/Output

(defun muon-display-line (string)
  (when string
    (save-excursion
      ;; consider setting buffer here
      (let ((insert-position (or (marker-position muon-insert-marker)
                                                  (point-max))))
        (let ((buffer-undo-list t)
              (inhibit-read-only t))
          (save-restriction
            (widen)
            (goto-char insert-position)
            (insert-before-markers string)
            (save-restriction
              (narrow-to-region insert-position (point))
              (ansi-color-apply-on-region (point-min) (point-max))

              ;; Make read only
              (put-text-property (point-min) (point-max) 'read-only t)
              (put-text-property (point-min) (point-max) 'front-sticky t)
              (put-text-property (point-min) (point-max) 'rear-nonsticky t))))))))

(defun muon-send-input-line (line)
  (let ((process (get-buffer-process (current-buffer)))
        (telnet-line (concat line "\r\n")))
    (process-send-string process telnet-line)))

(defun muon-display-prompt (&optional buffer pos)
  (let* ((prompt (muon-prompt))
         (l (length prompt))
         (ob (current-buffer)))

    (save-excursion
      (setq pos (or pos (point)))
      (goto-char pos)
      (when (> l 0)
        (setq prompt (propertize prompt
                                 'rear-nonsticky t
                                 'muon-prompt t
                                 'front-sticky t
                                 'read-only 't))
        (put-text-property 0 (1- (length prompt))
                           'face 'muon-prompt-face
                           prompt)
        (insert prompt))

      (set-marker muon-input-marker (point)))

    (when (or (not pos) (<= (point) pos))
      (forward-char l))

    (setq buffer-undo-list nil)
    (set-buffer ob)))

(defun muon-beg-of-input-line ()
  (marker-position muon-insert-marker))

(defun muon-end-of-input-line ()
  (point-max))

(defun muon-send-current-line ()
  (interactive)
  (save-restriction
    (widen)
    (if (< (point) (muon-beg-of-input-line))
        (muon-error "Point is not in the input area")
      (let ((inhibit-read-only t)
            (str (muon-user-input))
            (old-buf (current-buffer)))

        (delete-region (muon-beg-of-input-line)
                       (muon-end-of-input-line))

        (unwind-protect
            (muon-send-input-line str)
          (with-current-buffer old-buf
            (save-restriction
              (widen)
              (goto-char (point-max))
              (when (processp muon-server-process)
                (set-marker (process-mark muon-server-process) (point)))
              (set-marker muon-insert-marker (point))
              (let ((buffer-modified (buffer-modified-p)))
                (muon-display-prompt)
                (set-buffer-modified-p buffer-modified)))))))))

(defun muon-user-input ()
  (buffer-substring-no-properties
   muon-input-marker
   (muon-end-of-input-line)))

(defun muon-error (&rest args)
  (if debug-on-error
      (apply #'error args)
    (apply #'message args)
    (beep)))

;;;; Networking

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
  (aset muon-insert-bufstr muon-insert-bufstr-pos byte)
  (incf muon-insert-bufstr-pos)
  (cond
   ((eq ?\n byte)
    (muon-display-line (substring muon-insert-bufstr 0 muon-insert-bufstr-pos))
    (setq muon-insert-bufstr-pos 0))
   ((eq muon-insert-bufstr-pos (length muon-insert-bufstr))
    (setq muon-insert-bufstr (concat muon-insert-bufstr (make-bufstr))))))

(defun make-bufstr ()
  (make-string 1024 0))

;;;; World / Profile Functions

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
