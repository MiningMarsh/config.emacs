;;; stickyfunc --- Sticky function name support.
;;; Commentary:
;;; Code:

(packages/requires (semantic)
		   (add-hooks (c-mode)
			      (semantic-mode)
			      (semantic-stickyfunc-mode)))
;;; stickyfunc.el ends here
