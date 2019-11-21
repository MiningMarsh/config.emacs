;;; colored-compilation-buffer --- Enable color output for compilation tools
;;;                                that output colors.
;;; Commentary:
;;; Code:
(packages/requires (ansi-color)
  (add-hooks (compilation-filter-hook colorize-compilation-buffer)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
;;; colored-compilation-buffer.el ends here
