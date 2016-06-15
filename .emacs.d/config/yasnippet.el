;;; yasnippet --- Add yasnippet support.
;;; Commentary:
;;; Code:

(packages/requires (yasnippet)
		   (yas-global-mode 1)
		   (packages/requires (auto-complete)
				      (defer-after-init
					(when (auto-complete)
					  (add-to-list 'ac-sources 'ac-source-yasnippet)))))
;;; yasnippet.el ends here
