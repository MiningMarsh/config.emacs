;;; transparency --- Enable window transparency.
;;; Commentary:
;;; Code:
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
;;; transparency.el ends here
