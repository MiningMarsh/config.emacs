;;; pretty --- Enable printy rendering for mu4e.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (mu4e mu4e-contrib)
	   (setq mu4e-html2text-command "html2text -utf8 -width 72"))
;;; pretty.el ends here
