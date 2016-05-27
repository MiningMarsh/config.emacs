;;; slime-options --- Base SLIME option choice.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (slime slime-autoloads)
	   (slime-setup
	    '(slime-fancy
	      slime-repl
	      slime-sbcl-exts
	      slime-autodoc)))
;;; slime-options.el ends here
