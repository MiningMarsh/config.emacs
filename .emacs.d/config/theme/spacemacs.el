;;; spacemacs --- Enable spacemacs theme.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (spacemacs-theme
	    spaceline fancy-battery
	    spaceline-segments
	    spaceline-config)

	   (setq powerline-default-separator 'arrow)
	   (load-theme 'spacemacs-dark t)

	   (setq spaceline-highlight-face-func
		 'spaceline-highlight-face-evil-state

		 spaceline-workspace-numbers-unicode
		 t

		 spaceline-window-numbers-unicode
		 t)

	   (spaceline-spacemacs-theme)

	   (spaceline-toggle-line-column-off)
	   (spaceline-toggle-column-off)
	   (spaceline-toggle-line-off)
	   (spaceline-toggle-minor-modes-off)
	   (spaceline-toggle-major-mode-on)
	   (spaceline-toggle-process-on)
	   (spaceline-toggle-erc-track-on)
	   (spaceline-toggle-battery-on)
	   (spaceline-toggle-hud-off))
;;; spacemacs.el ends here
