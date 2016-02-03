;;; server-autostart --- Daemon mode autostart subsystem.
;;; Commentary:
;;; Code:
(require 'rc)

(defeat server-autostart ()
  (defvar server-autostart-alist nil
    "List of autostart commands to execute.")

  (defun register-daemon-autostarts (&rest autostarts)
    (setq server-autostart-alist
	  (nconc server-autostart-alist
		 (mapcar (lambda (l)
			   (cons
			    (symbol-name (car l))
			    (car (cdr l))))
			 (partition 2 autostarts)))))
  ;; Parse and enfore autostarts.
  (add-hooks (after-init-hook)
	     (dolist (autostart server-autostart-alist)
	       (bind-head-tail (name command) autostart
			       (when (string= name (daemonp))
				   (funcall command))))))
;;; server-autostart.el ends here
