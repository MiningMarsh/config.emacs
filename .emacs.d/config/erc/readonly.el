;;; readonly --- Enable ERC readonly enforcement.
;;; Commentary:
;;; Code:

(packages/requires (erc)
		   (add-to-list 'erc-modules 'readonly))
;;; readonly.el ends here
