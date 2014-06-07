(require 'smartparens-config)

(smartparens-global-mode)
(show-smartparens-global-mode)

(define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-{") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-}") 'sp-backward-barf-sexp)
