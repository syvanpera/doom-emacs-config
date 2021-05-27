;;; custom/lang/packages.el -*- lexical-binding: t; -*-

(package! pnpm-mode)

(package! graphql-mode)

(package! platformio-mode)

(package! gdscript-mode
          :recipe (:host github
                   :repo "GDQuest/emacs-gdscript-mode"))
