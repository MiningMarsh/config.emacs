;;; paredit -- Enable paredit.
;;; Commentary:
;;; Code:
(packages/requires (paredit)

		   ;; Enable paredir for various modes.
		   (add-hooks (emacs-lisp-mode
			       eval-expression-minibuffer-setup
			       ielm-mode
			       lisp-mode
			       lisp-interactive-mode
			       scheme-mode)
			      paredit-mode))
;;; paredit.el ends here
