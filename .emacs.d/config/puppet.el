;;; puppet --- Enable puppet support.
;;; Commentary:
;;; Code:

(packages/requires (puppet-mode flymake-puppet)

		   ;; Load puppet for puppet files.
		   (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

		   ;; Enable flymake puppet when using puppet mode.
		   (add-hooks (puppet-mode-hook)
			      flymake-puppet-load))
;;; puppet.el ends here
