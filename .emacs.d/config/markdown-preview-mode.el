;;; markdown-preview-mode --- Support for markdown previews.
;;; Commentary:
;;; Code:
(eval-when-compile (require 'key-tree))

(packages/requires (key-tree
		    markdown-mode
		    markdown-preview-mode
		    flyspell-mode
		    flycheck-mode
		    auto-fill-mode)

  (add-hooks (markdown-mode)
	     (flycheck-mode 't)
	     (column-enforce-mode 't)
	     (flyspell-mode 't)
	     (auto-fill-mode 't))

  (key-tree/add-mode-bindings
   markdown-mode
   "m" ("Markdown Editting"
	"p" "Open Clause" 'markdown-preview-mode
	"f" "Reformat Paragraph" 'fill-paragraph)))
;;; markdown-preview-mode.el ends here
