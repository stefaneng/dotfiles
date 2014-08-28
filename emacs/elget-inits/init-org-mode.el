;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Keep track of when a certain TODO was finished
(setq org-log-done 'time)

;; Set agenda files
(setq org-agenda-files (list "~/org/agenda.org"))

;; Default notes file
(setq org-default-notes-file "~/org/notes.org")

;; Save clock history across emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
