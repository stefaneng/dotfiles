;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Keep track of when a certain TODO was finished
(setq org-log-done 'time)

;; See http://orgmode.org/worg/org-faq.html#set-agenda-files-recursively
(defun find-org-files (dir)
  "Returns a list of all org files in directory DIR"
  ;; Uses the find-lisp function
  (load-library "find-lisp")
  (find-lisp-find-files (expand-file-name dir) "\.org$"))

;; Add all of the org files found (recursively) to the agenda-files
(setq org-agenda-files
      (apply 'append
	     (mapcar 'find-org-files
		     ;; List of org mode file locations
		     (list "~/org" "~/school"))))

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

;; Use CDLaTeX as minor mode to insert environments and math templates
;; http://orgmode.org/org.html#CDLaTeX-mode
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; Set TODO Keywords as workflow states
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "POSTPONDED")))
