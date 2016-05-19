;;; stickyfunc --- Sticky function name support.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (semantic)
		   (add-hooks (c-mode)
					  (semantic-mode)
					  (semantic-stickyfunc-mode)))
;;; stickyfunc.el ends here
