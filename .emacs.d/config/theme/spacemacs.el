;;; spacemacs --- Enable spacemacs theme.
;;; Commentary:
;;; Code:

(packages/requires (spacemacs-theme spaceline fancy-battery spaceline-segments spaceline-config)
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
	     (spaceline-toggle-battery-off)
	     (spaceline-toggle-nyan-cat-on)

	   (when (daemonp)
	       (add-hook 'after-make-frame-functions
			 (lambda (frame)
			   (with-selected-frame frame
			     (load-theme 'spacemacs-dark t))))))
;;; spacemacs.el ends here
