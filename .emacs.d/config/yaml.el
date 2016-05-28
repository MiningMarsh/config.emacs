;;; yaml --- Enable yaml support.
;;; Commentary:
;;; Code:

(packages/requires (yaml-mode)
	   (add-hooks (yaml-mode-hook)
		     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;;; yaml.el ends here
