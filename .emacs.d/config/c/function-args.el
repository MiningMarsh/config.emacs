;;; function-args --- Add function args support.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (function-args)
		   (add-hooks (c-mode-hook)
					  (function-args-mode 1)))
;;; function-args.el ends here
