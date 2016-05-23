;;; images --- Emable inline images in erc.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc erc-image)
	   (add-to-list 'erc-modules 'image)
	   (erc-update-modules)
	   (setq erc-image-inline-rescale 300))
;;; images.el ends here