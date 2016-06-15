;;; keybinds --- Magit keybinds.
;;; Commentary:
;;; Code:
(packages/requires (key-tree magit)
		   (key-tree/add-bindings
		    g ("Git"
		       s "Status" 'magit-status)))
;;; keybinds.el ends here
