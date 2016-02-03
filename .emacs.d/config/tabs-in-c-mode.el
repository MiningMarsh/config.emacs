;;; tabs-in-c-mode --- Enable tabs in C mode.
;;; Commentary:
;;; Code:
(require 'rc)

(add-hooks (c-mode-common-hook)
	   (setq-default c-default-style "linux"
			 c-basic-offset 4
			 tab-width 4
			 indent-tabs-mode t))
;;; tabs-in-c-mode.el ends here
