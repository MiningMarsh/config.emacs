;;; workspaces --- Set the number of exwm workspaces.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (exwm)
	   ;; The number of workspaces.
	   (setq exwm-workspace-number 10)

	   ;; Bind for every workspace.
	   (dolist (workspace (number-sequence 1 10))

	     ;; Make the binding lexical.
	     (lexical-let ((workspace workspace))

	       ;; Switch to target workspace.
	       (exwm-input-set-key
		(kbd (format "s-%d" (mod workspace 10)))
		(lambda ()
		  (interactive)
		  (exwm-workspace-switch (1- workspace))))

	       ;; Move to target to target workspace.
	       (exwm-input-set-key
		(kbd (format "M-s-%d" (mod workspace 10)))
		(lambda ()
		  (interactive)
		  (exwm-workspace-move-window (1- workspace) (selected-window)))))))
;;; workspaces.el ends here
