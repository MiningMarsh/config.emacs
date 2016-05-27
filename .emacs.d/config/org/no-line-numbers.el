;;; no-line-numbers --- Disable line numbers in org-mode.
;;; Commentary:
;;; Line numbers don't look nice with the different sized headers.
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (linum)
	   (add-hooks (org-mode-hook)
		      (linum-mode -1)
		      (line-number-mode -1)))
;;; no-line-numbers.el ends here
