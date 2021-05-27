;;; custom/lang/config.el -*- lexical-binding: t; -*-

(load! "elisp/protobuf-mode")
(load! "elisp/prisma-mode")
(load! "elisp/jinja2-mode")

(after! go-mode
  (setq gofmt-command "goimports")
  ;; turn off company idle completion, it's annoyingly slow
  (add-hook! 'go-mode-hook #'rainbow-delimiters-mode)
  (add-hook! 'before-save-hook 'gofmt-before-save))

(after! company-go
  (setq company-idle-delay 0
        company-minimum-prefix-length 1))

(use-package! pnpm-mode
  :hook ((js-mode typescript-mode) . pnpm-mode))

(use-package! graphql-mode)
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.prisma\\'" . graphql-mode)))

(use-package! platformio-mode
  :init
  (add-hook 'c-mode-hook (lambda () (platformio-conditionally-enable)))
  (add-hook 'c++-mode-hook (lambda () (platformio-conditionally-enable))))

(use-package! gdscript-mode
  :init
  (require 'gdscript-mode))

(after! rustic
  (setq lsp-rust-server 'rust-analyzer))

(after! dart-mode
  (setq lsp-dart-sdk-dir "/opt/dart-sdk"
        lsp-dart-flutter-sdk-dir "/home/tuomo/apps/flutter"))

(after! lsp-ui
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 50
        lsp-ui-doc-max-width 150
        lsp-ui-doc-include-signature t
        lsp-headerline-breadcrumb-enable t
        lsp-lens-enable t
        ;; lsp-ui-sideline-show-hover t
        ;; lsp-ui-sideline-show-symbol t
        lsp-signature-render-documentation t))
