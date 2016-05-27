;;; yasnippet --- Add yasnippet support.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (yasnippet)
		   (yas-global-mode 1)
		   (packages/requires (auto-complete)
					  (defer-after-init
						(when (auto-complete)
						  (add-to-list 'ac-sources 'ac-source-yasnippet)))))
;;; yasnippet.el ends here
