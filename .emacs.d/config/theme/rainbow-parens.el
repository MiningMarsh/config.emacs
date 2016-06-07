;;; rainbow-parens --- Enable rainbow parens.
;;; Commentary:
;;; Code:

(packages/requires (rainbow-delimiters)
	   (add-hooks (prog-mode)
		      rainbow-delimiters-mode))
;;; rainbow-parens.el ends here
