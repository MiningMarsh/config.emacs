;;; mu4e-tls --- Enable TLS encryption for mu4e.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (mu4e tls)
	   (setq starttls-use-gnutls t)
	   (setq gnutls-min-prime-bits 1024))
;;; mu4e-tls.el ends here
