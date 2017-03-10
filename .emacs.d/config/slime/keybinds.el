;;; keybinds --- Leader bindings for invoking and using slime.
;;; Commentary:
;;; Code:
(packages/requires (key-tree)
		   (key-tree/add-mode-bindings lisp-mode
		    s ("SLIME"
			 s "Spawn SLIME prompt" 'slime

			 e ("Eval"
			    d "Defun" 'slime-eval-defun
			    b "Buffer" 'slime-eval-buffer))))
;;; keybinds.el ends here
