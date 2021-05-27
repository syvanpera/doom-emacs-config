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
    :token ""
    :subscribed-channels '(logisnext)
    :full-and-display-names t))
