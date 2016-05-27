;;; which-key --- Enable which-key navigation.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (which-key)
	   ;; Setup which-key
	   (which-key-setup-minibuffer)
	   (setq which-key-popup-type 'minibuffer)
	   (dolist (key (assoc-map "TAB" "↹"
			       "RET" "⏎"
			       "DEL" "⇤"
			       "SPC" "␣"))
	     (add-to-list 'which-key-key-replacement-alist key))
	   (setq which-key-show-prefix 'top)
	   (which-key-mode 1))

;;; which-key.el ends here
