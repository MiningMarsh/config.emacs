;;; bury-buffers --- Bury new ERC buffers instead of focusing them.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (erc)
	   (setq erc-join-buffer 'bury))
;;; bury-buffers.el ends here
