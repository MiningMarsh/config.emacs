;;; window-width --- Set window width for mu4e.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (mu4e)
	   (add-hooks (window-configuration-change-hook)
		      (setq erc-fill-column (- (window-width) 2))))
;;; window-width.el ends here
