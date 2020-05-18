;;; custom/config/+funcs.el -*- lexical-binding: t; -*-

(defun ts/clipboard-get ()
  "Returns the system clipboard contents."
  (interactive)
  (shell-command-to-string "xclip -selection clipboard -out"))
