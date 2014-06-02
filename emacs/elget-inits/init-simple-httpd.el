;;;; Config for simple-httpd.el
;;;; https://github.com/skeeto/emacs-web-server/blob/master/simple-httpd.el

(defun httpd-serve-dir (dir)
  "Serves a directory. DIR defaults to current directory"
  (interactive "DServe directory: ")
  (set 'httpd-root dir)
  (httpd-start))

(defun httpd-serve-dir-and-open (dir)
  "Serves directory DIR and opens in browser."
  (interactive "DServe directory: ")
  (set 'httpd-root dir)
  (httpd-start)
  (browse-url (concat
	       "http://"
	       (or httpd-host "localhost")
	       ":"
	       (number-to-string httpd-port))))
