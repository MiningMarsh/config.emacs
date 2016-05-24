;;; surpress-help-message --- Surpress emacs startup help message.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (message-filter)
	   (message-blacklist
	    "For information about GNU Emacs and the GNU system, type C-h C-a\."))
;;; surpress-help-message.el ends here
