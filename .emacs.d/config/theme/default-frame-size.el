;;; default-frame-size --- Set the default frame size.
;;; Commentary:
;;; Code:

(add-hooks (before-make-frame-hook)
	   (add-to-list 'default-frame-alist '(left   . 0))
	   (add-to-list 'default-frame-alist '(top    . 0))
	   (add-to-list 'default-frame-alist '(height . 70))
	   (add-to-list 'default-frame-alist '(width  . 80)))
;;; default-frame-size.el ends here
