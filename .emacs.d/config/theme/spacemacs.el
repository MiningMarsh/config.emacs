;;; spacemacs --- Enable spacemacs theme.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (spacemacs-theme spaceline fancy-battery spaceline-segments spaceline-config)
	   (setq powerline-default-separator 'contour)
	   (load-theme 'spacemacs-dark t)
	   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
		 spaceline-workspace-numbers-unicode t
		 spaceline-window-numbers-unicode t)
	   (spaceline-spacemacs-theme)
	   (spaceline-toggle-line-off)
	   (spaceline-toggle-line-column-off)
	   (spaceline-toggle-minor-modes-off)
	   (spaceline-toggle-major-mode-off)
	   (spaceline-toggle-process-off)
	   (spaceline-toggle-erc-track-on)
	   (spaceline-toggle-battery-on)
	   (spaceline-toggle-nyan-cat-on))
;;; spacemacs.el ends here
