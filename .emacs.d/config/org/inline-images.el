;;; inline-images --- Automatically inline images in Org mode.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (org)
	   (setq org-startup-with-inline-images t))
;;; inline-images.el ends here
