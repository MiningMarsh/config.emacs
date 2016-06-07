;;; semantic --- Semantic mode support.
;;; Commentary:
;;; Code:

(packages/requires (semantic auto-complete)

		   ;; Turn on semantic mode.
		   (semantic-mode 1)

		   ;; Turn on some semantic nicities.
		   (global-semantic-stickyfunc-mode 1)
		   (global-semantic-decoration-mode 1)
		   (global-semantic-decoration-mode 1)
		   (global-semanticdb-minor-mode 1)
		   (global-semantic-idle-summary-mode 1)
		   (global-semantic-highlight-func-mode 1)
		   (global-semantic-idle-breadcrumbs-mode 1)
		   (global-semantic-highlight-edits-mode 1)

		   ;; Enable semantic completions.
		   (add-hooks (auto-complete-mode)
			      (add-to-list 'ac-sources 'ac-source-semantic)))
;;; semantic.el ends here
