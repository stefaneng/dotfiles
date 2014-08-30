;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Keep track of when a certain TODO was finished
(setq org-log-done 'time)

;; Set agenda files
(setq org-agenda-files '())
(add-to-list 'org-agenda-files (expand-file-name "~/org"))
(add-to-list 'org-agenda-files (expand-file-name "~/school"))


;; Default notes file
(setq org-default-notes-file "~/org/notes.org")

;; Save clock history across emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
