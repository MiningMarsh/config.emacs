;;; flycheck --- Flycheck.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (flycheck paredit)
	   ;; Enable paredit for various modes.
	   (add-hooks (emacs-lisp-mode-hook
		       eval-expression-minibuffer-setup-hook
		       ielm-mode-hook
		       lisp-mode-hook
		       lisp-interactive-mode-hook
		       scheme-mode-hook)
		      (unless (string= (buffer-name) "*scratch*")
			(flycheck-mode 1))))
;;; flycheck.el ends here
