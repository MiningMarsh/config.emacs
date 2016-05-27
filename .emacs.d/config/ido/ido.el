;;; ido --- Enable ido everywhere possible.
;;; Commentary:
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (ido ido-ubiquitous ido-yes-or-no)
	   (ido-mode 1)
	   (ido-everywhere 1)
	   (ido-ubiquitous-mode 1)
	   (ido-yes-or-no-mode 1))
;;; ido.el ends here
