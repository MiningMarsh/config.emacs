;;; tabs.el --- Enable tabs in C mode.
;;; Commentary:
;;; Code:

(add-hooks (c-mode-common)
	   (setq-default c-default-style "linux"
			 c-basic-offset 4
			 tab-width 4
			 indent-tabs-mode t))
;;; tabs.el ends here
