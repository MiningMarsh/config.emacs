;;; stickyfunc --- Sticky function name support.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (semantic)
		   (add-hooks (c-mode)
					  (semantic-mode)
					  (semantic-stickyfunc-mode)))
;;; stickyfunc.el ends here
