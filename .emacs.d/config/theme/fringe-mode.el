;;; fringe-mode --- Set fringe size.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(defun disable-window-fringe ()
  "Disable fringe in current window."
  (set-window-fringes (selected-window) 0 0 nil))

;;(add-hook 'minibuffer-setup-hook 'disable-window-fringe)
;;(add-hook 'echo-area-clear-hook 'disable-window-fringe)

(fringe-mode '(5 . 0))

;;; fringe-mode.el ends here
