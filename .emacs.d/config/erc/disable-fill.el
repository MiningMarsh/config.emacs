;;; disable-fill --- Disable fill mode in ERC.
;;; Commentary:
;;; Code:

(packages/requires (erc)
	   (remove-from-list 'erc-modules 'fill)
	   (erc-fill-mode 0)
	   (remove-from-list 'erc-modules 'stamp)
	   (erc-stamp-mode 0))
;;; disable-fill.el ends here
