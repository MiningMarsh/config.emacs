;;; greentext --- Greentext support for ERC.
;;; Commentary:
;;; Code:

(packages/requires (erc erc-input-hooks)
	   (erc-add-input-hook "^>" (format "%c3>" 3)))
;;; greentext.el ends here
