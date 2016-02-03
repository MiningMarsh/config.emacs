;;; erc-daemon --- Make erc daemon instances autostart erc.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (server-autostart)
	   (register-daemon-autostarts
	    'irc #'erc-tls))
;;; erc-daemon.el ends here
