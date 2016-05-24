;;; bold --- Bold support for ERC.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (erc erc-input-hooks)
	   (erc-add-input-hook "\\[BOLD\\]\\(.*\\)\\[BOLD\\]" (format "%c\\1%c" 2 2)))
;;; bold.el ends here
