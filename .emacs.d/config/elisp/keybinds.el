;;; keybinds --- Leader bindings.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (evil-leader)
	   (evil-leader/set-key-for-mode 'emacs-lisp-mode
	     "(" 'paredit-open-round
	     ")" 'paredit-close-round
	     "B" 'paredit-backward-barf-sexp
	     "D" 'paredit-backward-delete
	     "H" 'paredit-join-sexp
	     "R" 'paredit-kill
	     "S" 'paredit-backward-slurp-sex
	     "X" 'paredit-backward-kill-word
	     "b" 'paredit-forward-barf-sexp
	     "d" 'paredit-forward-delete
	     "e" 'eval-buffer
	     "h" 'paredit-split-sexp
	     "j" 'paredit-forward
	     "k" 'paredit-backward
	     "r" 'paredit-raise-sexp
	     "s" 'paredit-forward-slurp-sexp
	     "x" 'paredit-forward-kill-word))
;;; keybinds.el ends here
