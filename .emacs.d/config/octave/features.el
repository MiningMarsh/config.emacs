;;; features --- Enable specific features in octave mode.
;;; Commentary:
;;; Currently enables abbrev-mode, auto-fill-mode, and font-lock-mode
;;; Code:
(require 'rc)

(add-hooks (octave-mode-hook)
	   (abbrev-mode 1)
	   (auto-fill-mode 1)
	   (if (eq window-system 'x)
	       (font-lock-mode 1)))
;;; features.el ends here