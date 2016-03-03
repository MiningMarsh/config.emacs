;;; spelling --- Enable ERC spell checking.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc auto-complete ac-ispell)
	   (add-to-list 'erc-modules 'spelling)
	   (add-hooks (erc-mode)
		      (ac-ispell-ac-setup)))
;;; spelling.el ends here
