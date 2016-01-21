;;; xresources --- Enable xresources based theme on X window system.
;;; Commentary:
;;; Code:
(require 'rc)

(defun apply-color-theme (frame)
  (select-frame frame)
  (when (eq (window-system frame) 'x)
    (load-theme 'xresources t)))

(setq color-theme-is-global nil)
(add-hook 'after-make-frame-functions 'apply-color-theme)
;;; xresources.el ends here
