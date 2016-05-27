;;; semantic --- Semantic mode support.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (semantic)
		   (semantic-mode 1)
		   (packages/requires (auto-complete)
					  (defer-after-init
						(when (auto-complete)
						  (add-to-list 'ac-sources 'ac-source-semantic)))))
;;; semantic.el ends here
