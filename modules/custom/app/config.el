;;; custom/app/config.el -*- lexical-binding: t; -*-

(use-package! slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
    :name "houston-inc"
    :default t
    :token "xoxs-243295913552-1858796528662-1852661758583-e51665d8493d5b345d1fd07f5e2afb03d95e651ee9db63bf1bcb7f0600008391"
    :subscribed-channels '(logisnext)
    :full-and-display-names t))
