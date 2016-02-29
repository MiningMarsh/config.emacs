;;; eval-lisp-input --- Eval lisp input for ERC input.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (erc erc-input-hooks)
	   (erc-add-input-hook
	    "^[(].*[)]$"
	    (lambda (string)
	      (list
	       string
	       (->> string
		    read
		    eval
		    (format "%s"))))))
;;; eval-lisp-input.el ends here
