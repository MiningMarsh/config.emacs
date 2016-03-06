;;; display-battery-mode --- Display battery percentage.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (fancy-battery)
	   (defer-after-init
	     (fancy-battery-mode 1)
	     (setq fancy-battery-show-percentage t)))
;;; display-battery-mode.el ends here
