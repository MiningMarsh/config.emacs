;;; readonly --- Enable ERC readonly enforcement.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (erc)
	   (add-to-list 'erc-modules 'readonly))
;;; readonly.el ends here
