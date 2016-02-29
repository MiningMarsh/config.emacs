;;; keybinds --- mu4e keybindings.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (evil evil-leader mu4e mu4e-fixes mu4e-accounts)
	    ;;(evil-define-key 'normal 'mu4e-headers-mode-map
	      ;;(kbd "RET") 'mu4e-headers-view-message
	      ;;"j" 'mu4e-headers-next
	      ;;"k" 'mu4e-headers-prev
	      ;;"?" 'mu4e-headers-mark-for-unread
	      ;;"!" 'mu4e-headers-mark-for-read
	      ;;"x" 'mu4e-headers-mark-for-trash
	      ;;"d" 'mu4e-headers-mark-for-delete
	      ;;"=" 'mu4e-headers-mark-for-untrash
	      ;;"s" 'mu4e-headers-search
	      ;;"/" 'mu4e-headers-search-narrow
	      ;;"+" 'mu4e-headers-mark-for-flag
	      ;;"-" 'mu4e-headers-mark-for-unflag
	      ;;"u" 'mu4e-headers-mark-for-unmark
	      ;;"]" 'mu4e-headers-next-unread
	      ;;"[" 'mu4e-headers-prev-unread
	      ;;"b" 'mu4e-headers-search-bookmark
	      ;;"a" 'mu4e-headers-action)

	    (evil-leader/set-key-for-mode 'mu4e-headers-mode
	      (kbd "RET") 'mu4e-headers-view-message
	      "s" 'switch-mu4e-account
	      "j" 'mu4e~headers-jump-to-maildir
	      "r" 'mu4e-compose-reply
	      "f" 'mu4e-compose-forward
	      "c" 'mu4e-compose-new
	      "e" 'mu4e-compose-edit
	      "u" 'mu4e-mark-unmark-all
	      "x" 'mu4e-mark-execute-all
	      "q" 'mu4e~headers-quit-buffer)

	    (evil-leader/set-key-for-mode 'mu4e-compose-mode
	      "c" 'message-send-and-exit
	      "e" 'mml-secure-message-sign-encrypt
	      "s" 'mml-secure-message-sign
	      "S" 'switch-mu4e-account))
;;; keybinds.el ends here
