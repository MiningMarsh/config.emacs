;;; use-tabs.el --- Use tabs for shell script indention.
;;; Commentary:
;;; Code:
(add-hooks (sh-mode)
	   (setq tab-width 4
		 sh-basic-offset 4
		 indent-tabs-mode t))
;;; use-tabs.el ends here
