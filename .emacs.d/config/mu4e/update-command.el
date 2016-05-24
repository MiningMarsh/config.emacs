;;; update-command --- Set update command for fetching mail in mu4e.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (mu4e)
	   (setq mu4e-get-mail-command "update-mail"))
;;; update-command.el ends here
