;;; nick-colors --- ERC nick colors support.
;;; Commentary:
;;; Code:

(packages/requires (erc erc-nick-colors)
		   ;; Define the list of colors to use when coloring IRC nicks.
		   (setq erc-nick-colors-list
			 '("blue"
			   "green"
			   "yellow"
			   "gray"
			   "brown"
			   "white"
			   "cyan")))
;;; nick-colors.el ends here
