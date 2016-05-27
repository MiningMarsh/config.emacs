;;; paredit -- Enable paredit.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (paredit)

	   ;; Enable paredir for various modes.
	   (add-hooks (emacs-lisp-mode-hook
		       eval-expression-minibuffer-setup-hook
		       ielm-mode-hook
		       lisp-mode-hook
		       lisp-interactive-mode-hook
		       scheme-mode-hook)
		      paredit-mode))
;;; paredit.el ends here
