;;; ring --- Input history.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (erc)
	   (add-to-list 'erc-modules 'ring))
;;; ring.el ends here
