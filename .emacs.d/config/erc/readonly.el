;;; readonly --- Enable ERC readonly enforcement.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc)
	   (add-to-list 'erc-modules 'readonly))
;;; readonly.el ends here
