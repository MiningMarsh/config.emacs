;;; enforce-column-mode --- Highlight too long lines in source code.
;;; Commentary:
;;; Code:

(packages/requires (column-enforce-mode column-marker)
	   (add-hooks (prog-mode)
		      (setq column-enforce-column 81)
		      (column-enforce-mode)
		      (column-marker-1 80)))
;;; enforce-column-mode.el ends here
