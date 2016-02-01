;;; flycheck --- Flycheck.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (paredit)

	   ;; Enable paredit for various modes.
	   (add-hooks (emacs-lisp-mode-hook
		       eval-expression-minibuffer-setup-hook
		       ielm-mode-hook
		       lisp-mode-hook
		       lisp-interactive-mode-hook
		       scheme-mode-hook)
		      flycheck-mode))
;;; flycheck.el ends here
