;;; auto-complete-mode --- Enable auto-complete-mode.
;;; Commentary:
;;; Code:

(packages/requires (auto-complete yasnippet)
		   (ac-config-default)
		   (add-to-list 'ac-sources 'ac-source-yasnippet)
		   (global-auto-complete-mode t)
		   (ac-flyspell-workaround))
;;; auto-complete-mode.el ends here
