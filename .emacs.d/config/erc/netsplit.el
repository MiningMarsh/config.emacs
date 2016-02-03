;;; netsplit --- Detect netsplits in erc.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc)
	   (add-to-list 'erc-modules 'netsplit))
;;; netsplit.el ends here
