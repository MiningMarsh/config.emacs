;;; buffer --- Buffer library.
;;; Commentary:
;;; Contains extra functions for manipulating buffers.
;;; Code:
(packages/define buffer ()

  (defun buffer/revert ()
      "Revert the current buffer without confirmation."
    (interactive)
    (revert-buffer t t))

  (defun buffer/untabify ()
    (interactive)
    (untabify (point-min) (point-max))))
;;; buffer.el ends here
