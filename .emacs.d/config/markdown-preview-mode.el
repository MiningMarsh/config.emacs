;;; markdown-preview-mode --- Support for markdown previews.
;;; Commentary:
;;; Code:
(eval-when-compile (require 'key-tree))

(packages/requires (key-tree markdown-mode markdown-preview-mode)

  (add-hooks (markdown-mode)
	     (flyspell-mode 't))

  (key-tree/add-mode-bindings
   markdown-mode
   "m" ("Markdown Editting"
	"p" "Open Clause" 'markdown-preview-mode)))
;;; markdown-preview-mode.el ends here
