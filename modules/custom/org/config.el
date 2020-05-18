;;; custom/org/config.el -*- lexical-binding: t; -*-

(defun ts/org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(defun ts/org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post."
  (let* ((title (read-from-minibuffer "Post Title: "))
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "** TODO " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ":END:"
                 "%?\n")
               "\n")))

(defun ts/new-daily-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree (concat org-directory "reviews.org") "Daily reviews")
                                  (file (concat org-directory "dailyreview.org"))))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))

(defun ts/new-weekly-review ()
  (interactive)
  (let ((org-capture-templates '(("w" "Review: Weekly Review" entry (file+olp+datetree (concat org-directory "reviews.org") "Weekly reviews")
                                  (file (concat org-directory "weeklyreview.org"))))))
    (progn
      (org-capture nil "w")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))

(defun ts/new-monthly-review ()
  (interactive)
  (let ((org-capture-templates '(("m" "Review: Monthly Review" entry (file+olp+datetree (concat org-directory "reviews.org") "Monthly reviews")
                                  (file (concat org-directory "monthlyreview.org"))))))
    (progn
      (org-capture nil "m")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))

(after! org
  (add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
        org-tags-column -80
        org-agenda-tags-column -140
        org-log-done 'time
        org-log-reschedule 'note

        ts/org-capture-workflow-file (concat org-directory "workflow.org")
        ts/org-capture-readlater-file (concat org-directory "readlater.org")

        ;; org-agenda-files (mapcar(lambda (s) (concat org-directory s))
        ;;                         '("inbox.org"
        ;;                           "orgzly-inbox.org"
        ;;                           "todo.org"
        ;;                           "tickler.org"
        ;;                           "reviews.org"
        ;;                           "caverion.org"
        ;;                           "houston.org"
        ;;                           "workflow.org"))
        ;; org-refile-use-outline-path 'file
        ;; org-outline-path-complete-in-steps nil
        ;; org-refile-allow-creating-parent-nodes 'confirm
        ;; org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2))
        ;; org-archive-location (concat org-directory "archive.org::* From %s")
        ;; org-startup-folded 'content
        ;; org-pretty-entities t
        ;; org-use-fast-todo-selection t
        ;; org-goto-interface 'outline-path-completion
        ;; org-outline-path-complete-in-steps nil
        ;; org-agenda-dim-blocked-tasks t
        ;; org-agenda-skip-scheduled-if-done t
        ;; org-log-into-drawer t
        ;; org-log-state-notes-insert-after-drawers nil
        ;; org-bullets-bullet-list '("⚙" "✸" "✿" "◉" "○")
        ;; org-superstar-headline-bullets-list '("◉" "○" "✿" "✸")
        org-ellipsis " "
        org-tag-alist '(;; Context
                        ("COMPUTER" . ?c)
                        ("PHONE"    . ?p)
                        ("EMACS"    . ?e)
                        ("WORKFLOW" . ?f)
                        ("WORK"     . ?w)
                        (:startgroup)
                        ("@HOME"     . ?h)
                        ("@SHOPPING" . ?s)
                        ("@HOUSTON"  . ?o)
                        ("@CLIENT"   . ?l)
                        (:endgroup))

        org-capture-templates '(("t" "Personal todo" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* TODO %?" :prepend t :kill-buffer t)
                                ("l" "Personal todo with link" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* TODO %?\n%a" :prepend t :kill-buffer t)
                                ("w" "Workflow todo" entry
                                 (file+headline ts/org-capture-workflow-file "Inbox")
                                 "* TODO %?" :prepend t :kill-buffer t)
                                ("r" "Read later link" item
                                 (file+headline ts/org-capture-readlater-file "Links")
                                 "%(ts/clipboard-get)" :kill-buffer t))
        ;; Doom Emacs defaults
        ;; (("t" "Personal todo" entry
        ;;   (file+headline +org-capture-todo-file "Inbox")
        ;;   "* [ ] %?\n%i\n%a" :prepend t)
        ;; ("n" "Personal notes" entry
        ;;   (file+headline +org-capture-notes-file "Inbox")
        ;;   "* %u %?\n%i\n%a" :prepend t)
        ;; ("j" "Journal" entry
        ;;   (file+olp+datetree +org-capture-journal-file)
        ;;   "* %U %?\n%i\n%a" :prepend t)
        ;; ("p" "Templates for projects")
        ;; ("pt" "Project-local todo" entry
        ;;   (file+headline +org-capture-project-todo-file "Inbox")
        ;;   "* TODO %?\n%i\n%a" :prepend t)
        ;; ("pn" "Project-local notes" entry
        ;;   (file+headline +org-capture-project-notes-file "Inbox")
        ;;   "* %U %?\n%i\n%a" :prepend t)
        ;; ("pc" "Project-local changelog" entry
        ;;   (file+headline +org-capture-project-changelog-file "Unreleased")
        ;;   "* %U %?\n%i\n%a" :prepend t)
        ;; ("o" "Centralized templates for projects")
        ;; ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
        ;; ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
        ;; ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))

        ;; ORG-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        ;;                     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        ;; DRAFT is for blog posts, used in blog org files
        ;; org-todo-keyword-faces '(("TODO" . (:foreground "#61afef" :underline t))
        ;;                          ("NEXT" . (:foreground "#e5c07b" :underline t))
        ;;                          ("WAITING" . (:foreground "#de935f" :underline t))
        ;;                          ("HOLD" . (:foreground "#e06c75" :underline t))
        ;;                          ("DONE" . (:foreground "#98c379" :underline nil))
        ;;                          ("CANCELLED" . (:foreground "#717171" :underline nil))
        ;;                          ("DRAFT" . (:foreground "#fabd2f" :underline t)))
        ;; org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")
        ;;                     ;; (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
        ;;                     (sequence "NEXT(n)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "ABRT(c@/!)"))
        ;; org-todo-keyword-faces '(("PROJ" :inherit (bold default))
        ;;                          ("HOLD" :inherit (warning bold))
        ;;                          ("WAIT" :inherit (bold) :foreground "#da8548")
        ;;                          ("ABRT" :inherit (error bold)))
        ;; org-priority-faces '((65 :foreground "#e06c75")
        ;;                      (66 :foreground "#61afef")
        ;;                      (67 :foreground "#98c379"))
        ;; org-capture-templates '(("t" "TODO" entry
        ;;                          (file+headline +org-capture-todo-file "Tasks")
        ;;                          "* TODO %?" :prepend t :kill-buffer t)
        ;;                         ("l" "TODO with link" entry
        ;;                          (file+headline +org-capture-todo-file "Tasks")
        ;;                          "* TODO %?\n%a" :prepend t :kill-buffer t)
        ;;                         ("T" "Tickler" entry
        ;;                          (file+headline "~/Sync/org/tickler.org" "Tasks")
        ;;                          "* %i%?")
        ;;                         ("n" "Note" entry
        ;;                          (file+headline +org-capture-notes-file "Notes")
        ;;                          "* %u %?\n%i\n%a" :prepend t :kill-buffer t)
        ;;                         ("b" "Blog post" entry
        ;;                          (file+olp "~/projects/personal/tiniblog/content-org/posts.org" "Posts")
        ;;                          (function ts/org-hugo-new-subtree-post-capture-template))
        ;;                         ("ap" "Appointment : Personal" entry
        ;;                          (file  "gcal/gmail.org")
        ;;                          "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ;;                         ("ah" "Appointment : Houston" entry
        ;;                          (file  "gcal/houston.org")
        ;;                          "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

        ;;                         ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
        ;;                         ;; {todo,notes,changelog}.org file is found in a parent directory.
        ;;                         ;; Uses the basename from `+org-capture-todo-file',
        ;;                         ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
        ;;                         ("p" "Templates for projects")
        ;;                         ("pt" "Project: Todo" entry  ; {project-root}/todo.org
        ;;                          (file +org-capture-project-todo-file)
        ;;                          "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
        ;;                         ("pn" "Project: Note" entry  ; {project-root}/notes.org
        ;;                          (file +org-capture-project-notes-file)
        ;;                          "* %u %?\n%i\n%a" :prepend t :kill-buffer t)

        ;;                         ;; ("p" "Templates for reviews")
        ;;                         ;; ("rd" "Review: Daily Review" entry
        ;;                         ;;  (file+olp+datetree "/tmp/reviews.org" "Past reviews")
        ;;                         ;;  (file "templates/dailyreview.org"))
        ;;                         ;; ("rw" "Review: Weekly Review" entry
        ;;                         ;;  (file+olp+datetree "/tmp/reviews.org" "Past reviews")
        ;;                         ;;  (file "templates/weeklyreview.org"))
        ;;                         ;; ("rm" "Review: Monthly Review" entry
        ;;                         ;;  (file+olp+datetree "/tmp/reviews.org" "Past reviews")
        ;;                         ;;  (file "templates/monthlyreview.org"))
        ;;                         )

        ;; org-agenda-custom-commands '(("r" "Refile needed" tags-todo "REFILE")
        ;;                              ("g" . "GTD contexts")
        ;;                              ("gf" "Workflow" tags-todo "@WORKFLOW")
        ;;                              ("gc" "Client" tags-todo "@CLIENT")
        ;;                              ("gh" "Home" tags-todo "@HOME")
        ;;                              ("go" "Houston" tags-todo "@HOUSTON")
        ;;                              ("gw" "Work" tags-todo "WORK")
        ;;                              ("G" "GTD Block Agenda" ((tags-todo "@HOUSTON")
        ;;                                                       (tags-todo "@HOME"))))
        )
  )
