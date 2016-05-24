;;; accounts --- Specify mu4e accounts.
;;; Commentary:
;;; Code:
(require 'rc)

(packages/requires (mu4e mu4e-fixes mu4e-accounts)
	   
	   (setq mu4e-account-list
		 (list (make-mu4e-account
			:name "Online"
			:drafts-folder "/miningmarsh/[Gmail].Drafts"
			:mail-address "miningmarsh@gmail.com"
			:sent-folder "/miningmarsh/[Gmail].Sent Mail"
			:smtp-port 465
			:smtp-server "smtp.gmail.com"
			:smtp-stream-type 'ssl
			:trash-folder "/miningmarsh/[Gmail].Trash")
		       (make-mu4e-account
			:name "School"
			:drafts-folder "/jsongy3/[Gmail].Drafts"
			:sent-folder "/jsongy3/[Gmail].Sent Mail"
			:mail-address "jsongy3@gmail.com"
			:smtp-port 465
			:smtp-server "smtp.gmail.com"
			:smtp-stream-type 'ssl
			:trash-folder "/jsongy3/[Gmail].Trash")
		       (make-mu4e-account
			:name "Personal"
			:drafts-folder "/joshua.songy/[Gmail].Drafts"
			:mail-address "joshua.songy@gmail.com"
			:sent-folder "/joshua.songy/[Gmail].Sent Mail"
			:smtp-port 465
			:smtp-server "smtp.gmail.com"
			:smtp-stream-type 'ssl
			:trash-folder "/joshua.songy/[Gmail].Trash"))))
;;; accounts.el ends here
