;;; readonly --- Enable ERC readonly enforcement.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (erc)
	   (add-to-list 'erc-modules 'readonly))
;;; readonly.el ends here
