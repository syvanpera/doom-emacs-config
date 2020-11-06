;;; custom/config/+funcs.el -*- lexical-binding: t; -*-

(defun ts/clipboard-get ()
  "Returns the system clipboard contents."
  (interactive)
  (shell-command-to-string "xclip -selection clipboard -out"))

(defun ts/switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))))
