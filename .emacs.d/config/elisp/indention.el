;;; indention --- Fix indention for emacs lisp.
;;; Commentary:
;;; We used Common Lisp idention as Emacs Lisp indention is crap.
;;; Code:
(add-hooks (lisp-mode)
	   (set (make-local-variable 'lisp-indent-function)
		'common-lisp-indent-function))
;;; indention.el ends here
