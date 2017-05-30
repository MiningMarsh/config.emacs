;;; transparency --- Enable window transparency.
;;; Commentary:
;;; Code:
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))
;;; transparency.el ends here
