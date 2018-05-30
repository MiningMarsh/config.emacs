;;; os --- OS keybindings.
;;; Commentary:
;;; Code:
(eval-when-compile (require 'key-tree))

(packages/requires (key-tree os)
		   (key-tree/add-bindings

		    "o" ("Operating Systerm"
			 "l" "Lock Screen" (interactively (os/lock-screen)))))
;;; os.el ends here
