;;; semantic --- Semantic mode support.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (semantic)
		   (semantic-mode 1)
		   (requiring (auto-complete)
					  (defer-after-init
						(when (auto-complete)
						  (add-to-list 'ac-sources 'ac-source-semantic)))))
;;; semantic.el ends here
