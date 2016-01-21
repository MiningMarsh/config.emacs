;;; leopard --- Replace "keyboard" with "leopard" in erc
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc erc-input-hooks)
	   (erc-add-input-hook "keyboard" "leopard"))
;;; leopard.el ends here
