;;; spacemacs --- Enable spacemacs theme.
;;; Commentary:
;;; Code:
(packages/requires (spacemacs-theme
                    spaceline
                    spaceline-segments
                    spaceline-config)
                    ;;solarized-theme
                    ;;color-theme-solarized

  ;; This needs to be set before the theme is loaded for things
  ;; to apply correctly.
  (setq powerline-default-separator 'contour)

  (load-theme 'solarized-dark t)
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

  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (set-frame-parameter frame 'background-mode 'dark)
      (set-terminal-parameter frame 'background-mode 'dark)
      (load-theme 'solarized-dark t)
      )))
;;; spacemacs.el ends here
