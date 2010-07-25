(defvar growlnotify-command (executable-find "growlnotify") "The path to growlnotify")

(defun growl (title message)
  (start-process "growl" " growl"
                 growlnotify-command
                 title
                 "-a" "Emacs")
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))

(defun stesla-erc-growl-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (growl
     (concat "ERC: highlight: " (buffer-name (current-buffer)))
     message
     )))

(add-hook 'erc-text-matched-hook 'stesla-erc-growl-hook)

;;; Erlang
(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.5.1/emacs")
(setq erlang-root-dir "/usr/local/")
(setq exec-path (cons "/usr/local/bin" exec-path))
(require 'erlang-start)

(add-to-list 'load-path "~/.emacs.d/vendor/distel-4.03/elisp")
(require 'distel)
(distel-setup)