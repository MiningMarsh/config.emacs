;;; keybinds --- Keybindings for C source code.
;;; Commentary:
;;; Code:

(require 'key-tree)
(packages/requires (key-tree function-args)

		   (key-tree/add-mode-bindings
		    c-mode

		    "c" ("C Code"
			 "n" ("Navigation"
			      "j" "Jump" 'moo-jump-local))))
;;; keybinds.el ends here
