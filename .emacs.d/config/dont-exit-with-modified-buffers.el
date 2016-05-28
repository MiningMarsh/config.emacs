;;; dont-exit-with-modified-buffers --- Don't bother me about saving buffers.
;;; Commentary:
;;; This is a dirty hack, make sure to keep the string constants in line with
;;; the current version of Emacs.
;;; Code:

(defwrap yes-or-no-p (prompt)
  (if (string= prompt "Modified buffers exist; exit anyway? ")
      (progn (message "Modified buffers exist; not exitting.") nil)
    (yes-or-no-p prompt)))
;;; dont-exit-with-modified-buffers.el ends here
