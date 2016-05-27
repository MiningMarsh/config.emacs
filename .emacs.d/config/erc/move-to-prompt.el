;;; move-to-prompt --- Move erc to input prompt when typing.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (erc)
	   (add-to-list 'erc-modules 'move-to-prompt))
;;; move-to-prompt.el ends here
