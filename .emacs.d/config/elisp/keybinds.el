;;; keybinds --- Leader bindings.
;;; Commentary:
;;; Code:
(eval-when-compile (require 'key-tree))
(require 'rc)

(packages/requires (key-tree paredit)
	   (key-tree/add-mode-bindings
	    emacs-lisp-mode
	    "l" ("Lisp Editing"
		 "(" "Open Clause" 'paredit-open-round
		 ")" "Close Clause" 'paredit-close-round

		 "b" ("Barf"
		      "f" "Barf Forward" 'paredit-forward-barf-sexp
		      "b" "Barf Backward" 'paredit-backward-barf-sexp)

		 "d" ("Delete"
		      "f" "Delete Forward" 'paredit-forward-delete
		      "b" "Delete Backward" 'paredit-backward-delete)

		 "j" "Move Forward" 'paredit-forward
		 "k" "Move Backward" 'paredit-backward
		 "J" "Join Expression" 'paredit-join-sexp

		 "s" ("Slurp"
		      "b" "Slurp Backward" 'paredit-backward-slurp-sexp
		      "f" "Slurp Forward" 'paredit-forward-slurp-sexp)

		 "K" ("Kill"
		      "k" "Kill Expression" 'paredit-kill

		      "w" ("Kill Word"
			   "f" "Kill Word Forward" 'paredit-forward-kill-word
			   "b" "Kill Word Backward" 'paredit-backward-kill-word))

		 "S" "Split Expression" 'paredit-split-sexp
		 "r" "Raise Expression" 'paredit-raise-sexp)

	    "b" ("Buffer"
		 "e" "Evaluate Buffer" (interactively
					(with-temp-message "Buffer evaluating..."
					  (eval-buffer))
					(message "Buffer evaluated.")))))
;;; keybinds.el ends here
