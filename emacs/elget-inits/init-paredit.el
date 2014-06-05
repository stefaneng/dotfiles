;; (add-hook 'js2-init-hook 'paredit-nonlisp)

;; (defun paredit-nonlisp ()
;;   "Turn on paredit mode for non-lisps."
;;   (interactive)
;;   (set (make-local-variable 'paredit-space-for-delimiter-predicates)
;;        '((lambda (endp delimiter) nil)))
;;   (paredit-mode 1)
;;   ;; Rebind semicolon since it is not a comment in non-lisp
;;   (define-key (kbd ";")
;; 		  'self-insert-command))
