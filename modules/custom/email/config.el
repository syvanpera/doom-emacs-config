;;; custom/email/config.el -*- lexical-binding: t; -*-

;; ;; use org structures and tables in message mode
;; (add-hook 'message-mode-hook 'turn-on-orgtbl)
;; (add-hook 'message-mode-hook 'turn-on-orgstruct++)

(after! mu4e
  ;; (require 'org-mu4e)

  ;; Choose account label to feed msmtp -a option based on From header
  ;; in Message buffer; This function must be added to
  ;; message-send-mail-hook for on-the-fly change of From address before
  ;; sending message since message-send-mail-hook is processed right
  ;; before sending message.
  (defun choose-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "tuomo.syvanpera@houston-inc.com" from) "houston")
                 ((string-match "tuomo.syvanpera@gmail.com" from) "gmail"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))

  (setq mu4e-attachment-dir "~/Downloads"

        mu4e-change-filenames-when-moving t
        mu4e-html2text-command "w3m -dump -T text/html"
        mu4e-sent-messages-behavior 'delete
        mu4e-use-fancy-chars nil
        shr-color-visible-luminance-min 50
        mu4e-org-link-query-in-headers-mode nil
        mu4e-view-show-images t

        mu4e-maildir-shortcuts '(("/tuomo.syvanpera@gmail.com/INBOX"         . ?g)
                                 ("/tuomo.syvanpera@houston-inc.com/INBOX"   . ?h))

        mu4e-bookmarks '(("m:/tuomo.syvanpera@gmail.com/INBOX" "Gmail Inbox" ?g)
                         ("m:/tuomo.syvanpera@houston-inc.com/INBOX" "Houston Inbox" ?h)
                         ("m:/tuomo.syvanpera@gmail.com/INBOX or m:/tuomo.syvanpera@houston-inc.com/INBOX" "All Inboxes" ?i)
                         ("flag:unread and not m:tuomo.syvanpera@gmail.com/[Gmail].Trash and not m:tuomo.syvanpera@houston-inc.com/[Gmail].Trash" "Unread messages" ?u)
                         ("date:today..now and not m:tuomo.syvanpera@gmail.com/[Gmail].Trash and not m:tuomo.syvanpera@houston-inc.com/[Gmail].Trash" "Today's messages" ?t)
                         ("date:7d..now and not m:tuomo.syvanpera@gmail.com/[Gmail].Trash and not m:tuomo.syvanpera@houston-inc.com/[Gmail].Trash" "Last 7 days" ?w))

        ;; allow for updating mail using 'U' in the main view:
        mu4e-get-mail-command "mailsync"

        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        user-full-name "Tuomo Syvänperä"

        ;; Use the correct account context when sending mail based on the from header.
        message-sendmail-envelope-from 'header
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")

        mu4e-headers-date-format "%d.%m."
        mu4e-headers-fields '((:account . 12) (:human-date . 12) (:flags . 4) (:from-or-to . 25) (:subject))

        mu4e-contexts
        `(
          ,(make-mu4e-context
            :name "gmail"
            :match-func (lambda (msg) (when msg
                                   (string-match-p "^/tuomo.syvanpera@gmail.com" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-mail-address  . "tuomo.syvanpera@gmail.com")
                    (user-full-name     . "Tuomo Syvänperä")
                    (mu4e-drafts-folder . "/tuomo.syvanpera@gmail.com/[Gmail].Drafts")
                    (mu4e-sent-folder   . "/tuomo.syvanpera@gmail.com/[Gmail].Sent Mail")
                    (mu4e-trash-folder  . "/tuomo.syvanpera@gmail.com/[Gmail].Trash")
                    (mu4e-refile-folder . "/tuomo.syvanpera@gmail.com/[Gmail].All Mail")))
          ,(make-mu4e-context
            :name "houston"
            :match-func (lambda (msg) (when msg
                                   (string-match-p "^/tuomo.syvanpera@houston-inc.com" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-mail-address  . "tuomo.syvanpera@houston-inc.com")
                    (user-full-name     . "Tuomo Syvänperä")
                    (mu4e-compose-signature . "Tuomo Syvänperä\nSenior Consultant\nHouston Inc.\nKonepajankuja 1, 00510 Helsinki\nwww.houston-inc.com")
                    (mu4e-drafts-folder . "/tuomo.syvanpera@houston-inc.com/[Gmail].Drafts")
                    (mu4e-sent-folder   . "/tuomo.syvanpera@houston-inc.com/[Gmail].Sent Mail")
                    (mu4e-trash-folder  . "/tuomo.syvanpera@houston-inc.com/[Gmail].Trash")
                    (mu4e-refile-folder . "/tuomo.syvanpera@houston-inc.com/[Gmail].All Mail")))))
  ;; (add-hook 'message-send-mail-hook 'choose-msmtp-account))
  )
