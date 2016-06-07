;;; no-line-numbers --- Disable line numbers in org-mode.
;;; Commentary:
;;; Line numbers don't look nice with the different sized headers.
;;; Code:
(packages/requires (linum)
		   (add-hooks (org-mode)
			      (linum-mode -1)
			      (line-number-mode -1)))
;;; no-line-numbers.el ends here
