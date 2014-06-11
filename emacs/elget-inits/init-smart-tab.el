(require 'smart-tab)
(global-smart-tab-mode 1)

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))

(define-key read-expression-map [(backtab)] 'hippie-unexpand)
