;;;; Config for simple-httpd.el
;;;; https://github.com/skeeto/emacs-web-server/blob/master/simple-httpd.el

(customize-set-variable httpd-port 8000)

(defun httpd-serve-dir (dir)
  "Serves a directory. DIR defaults to current directory"
  (interactive "DServe directory: ")
  (set 'httpd-root dir)
  (httpd-start))
