;;; erc-morse-mode --- Add erc-morse mode.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (erc)
	   (defvar erc-unmorse-string "[morse]"
	     "String to be used so we see it's morse")
	   (defun erc-unmorse ()
	     "Decode morse code after point."
	     (interactive)
	     (goto-char (point-min))
	     (let ((erc-unmorse-point nil))
	       (when (re-search-forward "[.-]+\\([.-]+[/ ]\\)+[.-]+" nil t)
		 (setq erc-unmorse-point (match-beginning 0))
		 (unmorse-region (match-beginning 0) (match-end 0))
		 (goto-char erc-unmorse-point)
		 (insert-string erc-unmorse-string)
		 (insert-string " "))))
	   (defun erc-maybe-morse (ignore)
	     "Change the text to Morse code, if `erc-morse-mode' is non-nil."
	     (when erc-morse-mode
	       (with-temp-buffer
		 (insert str)
		 (erc-morse)
		 (setq str (buffer-string)))))
	   (defun erc-morse ()
	     "Transform the buffer into morse code."
	     (morse-region (point-min) (point-max)))
	   (define-erc-module morse nil
	     "This mode decodes one level of incoming morse,
    and it encodes outgoing messages as morse code."
	     ((add-hook 'erc-insert-modify-hook 'erc-unmorse)
	      (add-hook 'erc-send-pre-hook 'erc-maybe-morse))
	     ((remove-hook 'erc-insert-modify-hook 'erc-unmorse)
	      (remove-hook 'erc-send-pre-hook 'erc-maybe-morse))))
;;; erc-morse-mode.el ends here
