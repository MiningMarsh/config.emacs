;;; notifications --- Enable ERC notification support.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc)
	   (add-to-list 'erc-modules 'notifications))
;;; notifications.el ends here
