;;; smex --- Enable smex.
;;; Commentary:
;;; Smex promotes frequently used results in ido.
;;; Code:

(packages/requires (smex)
	   (smex-initialize)
	   (global-set-key (kbd "M-x") 'smex)
	   (global-set-key (kbd "M-X") 'mex-major-mode-commands)
	   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))
;;; smex.el ends here
