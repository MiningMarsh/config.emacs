;;; yasnippet --- Add yasnippet support.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (yasnippet)
		   (yas-global-mode 1)
		   (requiring (auto-complete)
					  (defer-after-init
						(when (auto-complete)
						  (add-to-list 'ac-sources 'ac-source-yasnippet)))))
;;; yasnippet.el ends here
