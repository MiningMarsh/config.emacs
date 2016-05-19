;;; speedbar --- Speedbar extensions.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (sr-speedbar key-tree)
	   (add-hooks (speedbar-mode-hook)
		      (linum-mode -1)
		      (relative-line-numbers-mode -1)
		      (line-number-mode -1))
	   (key-tree/add-bindings
	    "t" ("Tools"
		 "s" "File Bar" 'sr-speedbar-toggle)))
;;; speedbar.el ends here
