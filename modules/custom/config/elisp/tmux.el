(defmacro -ensure-dir (&rest body)
  "Ensure BODY runs in home directory."
  `(let ((default-directory "~"))
     ,@body))

(defun -windmove(dir flag)
  "Move focus to window according to DIR and TMUX-CMD."
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                       ; Moving within emacs
    ;; At edges, send command to tmux
    (-ensure-dir (call-process "tmux" nil nil nil "select-pane" flag))))

(defun tmux-window-up ()
  "Switch to the up window of Emacs or tmux."
  (interactive)
  (-windmove "up" "-U"))

(defun tmux-window-down ()
  "Switch to the down window of Emacs or tmux."
  (interactive)
  (-windmove "down" "-D"))

(defun tmux-window-left ()
  "Switch to the left window of Emacs or tmux."
  (interactive)
  (-windmove "left" "-L"))

(defun tmux-window-right ()
  "Switch to the right window of Emacs or tmux."
  (interactive)
  (-windmove "right" "-R"))
