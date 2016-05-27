;;; netsplit --- Detect netsplits in erc.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (erc)
	   (add-to-list 'erc-modules 'netsplit))
;;; netsplit.el ends here
