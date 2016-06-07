;;; flycheck --- Flycheck.
;;; Commentary:
;;; Code:

(packages/requires (flycheck paredit)
	   ;; Enable paredit for various modes.
	   (add-hooks (emacs-lisp-mode
		       eval-expression-minibuffer-setup
		       ielm-mode
		       lisp-mode
		       lisp-interactive-mode
		       scheme-mode)
		      (unless (string= (buffer-name) "*scratch*")
			(flycheck-mode 1))))
;;; flycheck.el ends here
