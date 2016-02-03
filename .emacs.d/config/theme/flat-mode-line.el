;;; flat-mode-line --- Make the mode line flat.
;;; Commentary:
;;; Code:
(require 'rc)

;; TODO: Fix add-hooks so it can handle irregular hooks like these.
(add-hook 'after-make-frame-functions
	   (lambda (frame)
	     (select-frame frame)
	     (when (eq (window-system frame) 'x)
	       (set-face-attribute
		'mode-line nil
		:box '(:color "white" :line-width 1 :style nil))
	       (set-face-attribute
		'mode-line-inactive nil
		:box '(:color "grey" :line-width 1 :style nil)))))
;;; flat-mode-line.el ends here
