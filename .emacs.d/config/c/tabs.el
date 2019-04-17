;;; tabs.el --- Set C default coding style.
;;; Commentary:
;;; Code:
(add-hooks (c-mode-hook c++-mode-hook)
	   (setq c-indent-level 8)
	   (setq c-brace-imaginary-offset 0)
	   (setq c-brace-offset -8)
	   (setq c-argdecl-indent 8)
	   (setq c-label-offset -8)
	   (setq c-continued-statement-offset 8)
	   (setq indent-tabs-mode nil)
	   (setq tab-width 8))
;;; tabs.el ends here
