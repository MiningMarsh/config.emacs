;;; yaml --- Enable yaml support.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (yaml-mode)
	   (add-hooks (yaml-mode-hook)
		     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;;; yaml.el ends here
