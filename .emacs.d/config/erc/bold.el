;;; bold --- Bold support for ERC.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc erc-input-hooks)
	   (erc-add-input-hook "\\*\\(.*\\)\\*" (format "%c\\1%c" 2 2)))
;;; bold.el ends here
