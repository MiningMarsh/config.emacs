;;; string --- String library.
;;; Commentary:
;;; Code:
(packages/define string ()
		 (defun split-once (str seperators)
		   (bind-head-tail (head tail) (split-string str seperators)
				   (cons head (reduce 'concat tail)))))
