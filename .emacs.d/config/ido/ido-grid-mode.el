;;; ido-grid-mode --- Enable ido-grid-mode.
;;; Commentary:
;;; Code:

(packages/requires (ido-grid-mode)
	   (ido-grid-mode 1)
	   (setq ido-grid-mode-max-rows 5)
	   (setq ido-grid-mode-min-rows 5)
	   (setq ido-grid-mode-start-collapsed nil))
;;; ido-grid-mode.el ends here
