;;; custom/config/config.el -*- lexical-binding: t; -*-

;; Switch workspace when switching projects
(setq +workspaces-on-switch-project-behavior t)

;; Make M-x harder to miss
(define-key! 'override
  "M-s" #'save-buffer
  "M-e" #'+treemacs/toggle
  "M-c" #'clipboard-kill-ring-save
  "M-v" #'clipboard-yank
  "M-a" #'mark-whole-buffer
  "M-r" #'counsel-imenu)

(map!
 :gnvime "C-h" #'evil-window-left
 :gnvime "C-j" #'evil-window-down
 :gnvime "C-k" #'evil-window-up
 :gnvime "C-l" #'evil-window-right)

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
      :desc "Switch to alt buffer" "TAB" #'evil-switch-to-windows-last-buffer
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
        :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final)))

(after! org
  (map!
   (:prefix "C-c"
    :gnvime "l" #'org-store-link
    :gnvime "a" #'org-agenda
    :gnvime "c" #'org-caputre)))

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
