;;; nyan-mode --- Enable nyan mode globally.
;;; Commentary:
;;; Code:
(packages/requires (nyan-mode)
		   (nyan-mode t)
		   (setq nyan-wavy-trail t)
		   (setq nyan-animate-nyancat t)
		   (nyan-start-animation))
;;; nyan-mode.el ends here
