;;; frame-title --- Set the frame title to the current file path.
;;; Commentary:
;;; Code:
(setq frame-title-format
  '((:eval (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
             "%b"))))
;;; frame-title.el ends here
