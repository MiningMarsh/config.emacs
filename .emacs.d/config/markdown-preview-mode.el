;;; markdown-preview-mode --- Support for markdown previews.
;;; Commentary:
;;; Code:
(eval-when-compile (require 'key-tree))

(packages/requires (key-tree
		    markdown-mode
		    markdown-preview-mode
		    column-enforce-mode
		    flycheck)

  (add-hooks (markdown-mode)
	     (flycheck-mode 't)
	     (column-enforce-mode 't)
	     (flyspell-mode 't)
	     (set-fill-column 80)
	     (refill-mode 't))

  (key-tree/add-mode-bindings
   markdown-mode
   "m" ("Markdown Editting"
	"p" "Open Clause" 'markdown-preview-mode
	"f" "Reformat Paragraph" 'fill-paragraph)))
;;; markdown-preview-mode.el ends here
