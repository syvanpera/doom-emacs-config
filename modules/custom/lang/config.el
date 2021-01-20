;;; custom/lang/config.el -*- lexical-binding: t; -*-

(after! go-mode
  (setq gofmt-command "goimports")
  ;; turn off company idle completion, it's annoyingly slow
  (add-hook! 'go-mode-hook #'rainbow-delimiters-mode)
  (add-hook! 'before-save-hook 'gofmt-before-save))

(after! company-go
  (setq company-idle-delay 0
        company-minimum-prefix-length 1))

(use-package! platformio-mode
  :init
  (add-hook 'c++-mode-hook (lambda () (platformio-conditionally-enable))))

(use-package! gdscript-mode
  :init
  (require 'gdscript-mode))

(after! rustic
  (setq lsp-rust-server 'rust-analyzer))
