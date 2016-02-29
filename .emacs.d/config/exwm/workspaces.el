;;; workspaces --- Set the number of exwm workspaces.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (exwm)
	   (setq exwm-workspace-number 10)
	   (mapc (lambda (workspace)
		   (lexical-let ((workspace workspace))
		     (exwm-input-set-key
		      (kbd (format "s-%d" (mod workspace 10)))
		      (lambda ()
			(interactive)
			(exwm-workspace-switch (1- workspace))))))
		 (number-sequence 1 10)))
;;; workspaces.el ends here
