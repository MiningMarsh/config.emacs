;;; maildir --- Sets mu4e mail directory.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (mu4e)
	   (setq mu4e-maildir "/home/miningmarsh/.mail"))
;;; maildir.el ends here
