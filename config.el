;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Tuomo Syvänperä"
      user-mail-address "tuomo.syvanpera@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
(setq doom-variable-pitch-font (font-spec :family "sans" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-custom)
;; (setq doom-theme 'doom-spaceduck-custom)

(setq doom-themes-treemacs-theme "doom-colors")
(setq doom-themes-neotree-file-icons t)

;; Transparent background
;; (set-frame-parameter (selected-frame)'alpha '(95 . 95))
;; (add-to-list 'default-frame-alist'(alpha . (95 . 95)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! tmux-pane
  :config
  (tmux-pane-mode))
  ;; (map! :leader
  ;;       (:prefix ("v" . "tmux pane")
  ;;         :desc "Open vpane" :nv "o" #'tmux-pane-open-vertical
  ;;         :desc "Open hpane" :nv "h" #'tmux-pane-open-horizontal
  ;;         :desc "Open hpane" :nv "s" #'tmux-pane-open-horizontal
  ;;         :desc "Open vpane" :nv "v" #'tmux-pane-open-vertical
  ;;         :desc "Close pane" :nv "c" #'tmux-pane-close
  ;;         :desc "Rerun last command" :nv "r" #'tmux-pane-rerun))
  ;; (map! :leader
  ;;       (:prefix "t"
  ;;         :desc "vpane" :nv "v" #'tmux-pane-toggle-vertical
  ;;         :desc "hpane" :nv "h" #'tmux-pane-toggle-horizontal)))

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

(after! go-mode
  (setq gofmt-command "goimports")
  ;; (add-hook! 'go-mode-hook #'rainbow-delimiters-mode)
  (add-hook! 'before-save-hook 'gofmt-before-save))

(after! deft
  (setq deft-directory "~/org/roam"
        deft-use-filename-as-title t)
  (add-hook! 'deft-mode-hook #'hl-line-mode))

(define-key! 'override
  "M-s" #'save-buffer
  ;; "M-e" #'+treemacs/toggle
  "M-e" #'+neotree/open
  ;; "M-e" #'ts/treemacs-toggle
  "M-c" #'clipboard-kill-ring-save
  "M-v" #'clipboard-yank
  "M-a" #'mark-whole-buffer
  "M-r" #'imenu
  "M-f" #'counsel-grep-or-swiper
  "M-F" #'+default/search-project)

;; window management (prefix "C-w")
(map!
 :map evil-window-map
 :gnvime "o" #'doom/window-maximize-buffer
 :gnvime "z" #'doom/window-enlargen)

(map!
 ;; :gnvime "C-h" #'evil-window-left
 ;; :gnvime "C-j" #'evil-window-down
 ;; :gnvime "C-k" #'evil-window-up
 ;; :gnvime "C-l" #'evil-window-right
 (:prefix "C-c"
  :gnvime "l"   #'org-store-link
  :gnvime "C-l" #'org-insert-link

  :gnvime "a"   #'org-agenda
  :gnvime "c"   #'org-capture))


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
      (:prefix-map ("e" . "errors")
       :desc "Next error"     "n" #'flycheck-next-error
       :desc "Previous error" "p" #'flycheck-previous-error
       :desc "List errors"    "l" #'flycheck-list-errors))
