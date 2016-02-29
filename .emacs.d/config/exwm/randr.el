;;; randr --- Enable randr support in exwm.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (exwm exwm-randr)
	   (setq exwm-randr-workspace-output-plist '(1 "eDP1"))
	   (add-hooks (exwm-randr-screen-change-hook)
		      (start-process-shell-command
		       "xrandr" nil "xrandr --output VGA1 --same-as eDP1 --output HDMI1 --same-as eDP1 --auto"))
	   (exwm-randr-enable))

;;; randr.el ends here
