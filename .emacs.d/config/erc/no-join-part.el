;;; no-join-part -- Hide join and part messages in erc.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (erc)
	   (setq erc-hide-list '("JOIN" "PART" "QUIT")))
;;; no-join-part.el ends here
