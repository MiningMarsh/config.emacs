;;; italics --- Italic support for ERC.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (erc erc-input-hooks)
	   (erc-add-input-hook "\\[ITAL\\]\\(.*\\)\\[ITAL\\]" (format "%c\\1%c" 29 29)))
;;; italics.el ends here
