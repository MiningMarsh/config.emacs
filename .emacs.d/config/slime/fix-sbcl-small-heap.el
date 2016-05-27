;;; fix-sbcl-small-heap --- Increases the heap size of sbcl when using slime.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (slime slime-autoloads)
	   (setq inferior-lisp-program "sbcl --dynamic-space-size 4096"))
;;; fix-sbcl-small-heap.el ends here
