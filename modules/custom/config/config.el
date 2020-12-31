;;; custom/config/config.el -*- lexical-binding: t; -*-

(load! "elisp/protobuf-mode")

(load! "+funcs")

(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; Don't save selections on system clipboard
(setq select-enable-clipboard nil
      save-interprogram-paste-before-kill t)

;; Week starts from Monday
(setq calendar-week-start-day 1)

;; Switch workspace when switching projects
(setq +workspaces-on-switch-project-behavior t)

;; Don't allow scratch and messages buffers to be killed
(with-current-buffer "*scratch*"  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

(define-key! 'override
  "M-s" #'save-buffer
  "M-e" #'+treemacs/toggle
  "M-c" #'clipboard-kill-ring-save
  "M-v" #'clipboard-yank
  "M-a" #'mark-whole-buffer
  "M-r" #'counsel-imenu
  "M-f" #'counsel-grep-or-swiper
  "M-F" #'+default/search-project
  "M-p" #'counsel-M-x)

(map!
 :gnvime "C-h" #'evil-window-left
 :gnvime "C-j" #'evil-window-down
 :gnvime "C-k" #'evil-window-up
 :gnvime "C-l" #'evil-window-right

 :nv     "go"  #'browse-url
 (:prefix "C-c"
  :gnvime "l"   #'org-store-link
  :gnvime "C-l" #'org-insert-link

  :gnvime "a"   #'org-agenda
  :gnvime "c"   #'org-capture))

;; window management (prefix "C-w")
(map!
 :map evil-window-map
 :gnvime "o" #'doom/window-maximize-buffer
 :gnvime "z" #'doom/window-enlargen)
(map!
 :map vterm-mode-map
 :i "C-h" #'evil-window-left
 :i "C-j" #'evil-window-down
 :i "C-k" #'evil-window-up
 :i "C-l" #'evil-window-right)

(map! :leader
      :desc "Toggle buffers" "TAB" #'evil-switch-to-windows-last-buffer
      :desc "Switch to workspace 1"  :n  "1"   #'(lambda () (interactive) (+workspace/switch-to 0))
      :desc "Switch to workspace 2"  :n  "2"   #'(lambda () (interactive) (+workspace/switch-to 1))
      :desc "Switch to workspace 3"  :n  "3"   #'(lambda () (interactive) (+workspace/switch-to 2))
      :desc "Switch to workspace 4"  :n  "4"   #'(lambda () (interactive) (+workspace/switch-to 3))
      :desc "Switch to workspace 5"  :n  "5"   #'(lambda () (interactive) (+workspace/switch-to 4))
      :desc "Switch to workspace 6"  :n  "6"   #'(lambda () (interactive) (+workspace/switch-to 5))
      :desc "Switch to workspace 7"  :n  "7"   #'(lambda () (interactive) (+workspace/switch-to 6))
      :desc "Switch to workspace 8"  :n  "8"   #'(lambda () (interactive) (+workspace/switch-to 7))
      :desc "Switch to workspace 9"  :n  "9"   #'(lambda () (interactive) (+workspace/switch-to 8))
      ;;; <leader> l --- workspace
      (:when (featurep! :ui workspaces)
       (:prefix-map ("l" . "workspace")
        :desc "Display tab bar"           "l"   #'+workspace/display
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "TAB" #'+workspace/other
        :desc "New workspace"             "n"   #'+workspace/new
        :desc "Load workspace from file"  "L"   #'+workspace/load
        :desc "Save workspace to file"    "s"   #'+workspace/save
        :desc "Delete session"            "x"   #'+workspace/kill-session
        :desc "Delete this workspace"     "d"   #'+workspace/delete
        :desc "Rename workspace"          "r"   #'+workspace/rename
        :desc "Restore last session"      "R"   #'+workspace/restore-last-session
        :desc "Next workspace"            "j"   #'+workspace/switch-right
        :desc "Previous workspace"        "k"   #'+workspace/switch-left
        :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
        :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
        :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
        :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
        :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
        :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
        :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
        :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
        :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
        :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))

      (:prefix ("a" . "applications")
       :desc "News feeds"           :n  "n" #'elfeed
       :desc "Email"                :n  "m" #'mu4e
       :desc "Prodigy"              :n  "p" #'prodigy)
      (:prefix ("f" . "file")
       (:prefix ("o" . "open")
        :desc "Open org folder"     :n  "o" #'(lambda () (interactive)
                                                (counsel-find-file org-directory))
        :desc "Open org inbox"      :n  "i" #'(lambda () (interactive)
                                                (find-file (concat org-directory +org-capture-todo-file)))

        ))
      (:prefix ("e" . "errors")
       :desc "List errors"          :n  "l" #'flycheck-list-errors
       :desc "Next error"           :n  "n" #'flycheck-next-error
       :desc "Previous error"       :n  "p" #'flycheck-previous-error)
      (:prefix ("t" . "toggle")
       :desc "Search highlight"     :n  "h" #'evil-ex-nohighlight)
      (:prefix ("b" . "buffer")
       :desc "Open messages buffer" :n  "m" #'ts/switch-to-messages-buffer)
      )

(after! deft
  (setq deft-directory "~/org/notes"
        deft-use-filename-as-title t)
  (add-hook! 'deft-mode-hook #'hl-line-mode))

;; evil-collection defines some of these and we need to override them. So bind these
;; after evil-collection
(after! evil-collection
  (defun ts/init-eshell-keymap ()
    "Setup eshell keybindings. This must be done in a hook because eshell-mode
    redefines its keys every time `eshell-mode' is enabled."
    (map! :map eshell-mode-map
          :i    "C-p" #'eshell-previous-matching-input-from-input
          :i    "C-n" #'eshell-next-matching-input-from-input
          :i    "C-r" #'+eshell/search-history))

  (add-hook! 'eshell-first-time-mode-hook #'ts/init-eshell-keymap))

(after! prodigy
  (prodigy-define-service
    :name "Hugo Personal Blog"
    :command "/usr/bin/hugo"
    :args '("server" "-D" "--navigateToChanged")
    :cwd "~/projects/personal/tiniblog"
    :tags '(personal)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(after! elfeed-org
  (setq rmh-elfeed-org-files (list (concat org-directory "elfeed.org"))))

(after! elfeed
  (setq elfeed-search-title-max-width 120)
  (map! :map elfeed-search-mode-map
        :ne "U" #'elfeed-update))

(use-package! tmux-pane
  :init
  (tmux-pane-mode))
