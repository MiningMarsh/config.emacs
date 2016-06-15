;;; keybinds --- Leader bindings.
;;; Commentary:
;;; Code:

(packages/requires (evil evil-leader org)
		   (evil-leader/set-key-for-mode 'org-mode
						 "p" 'org-preview-latex-fragment
						 "e" 'org-export-dispatch))

;;; keybinds.el ends here
