;;; backup-directory --- Set the backup directory.
;;; Commentary:
;;; Code:

(setq backup-directory-alist
      (assoc-map "." "~/.emacs.d/backups"))
;;; backup-directory.el ends here
