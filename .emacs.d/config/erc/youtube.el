;;; youtube --- Enable inline youtube thumbnails for erc.
;;; Commentary:
;;; Code:

(packages/requires (erc erc-yt)
		   (add-to-list 'erc-modules 'youtube)
		   (erc-update-modules)
		   (setq erc-yt-browse-function #'play-youtube))
;;; youtube.el ends here
