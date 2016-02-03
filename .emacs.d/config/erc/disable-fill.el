;;; disable-fill --- Disable fill mode in ERC.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc)
	   (remove-from-list 'erc-modules 'fill))
;;; disable-fill.el ends here
