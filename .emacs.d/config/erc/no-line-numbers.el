;;; no-line-numbers --- Disable line numbers in ERC.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (erc)
	   (add-hooks (erc-mode-hook)
		      (line-number-mode -1)
		      (linum-mode -1)))
;;; no-line-numbers.el ends here
