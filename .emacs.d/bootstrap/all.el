;;; all --- Library that is included for every config file.
;;; Commentary:
;;; This file is NOT loaded for libraries in bootstrap by default, they must
;;; still include packages and rc manually if they need it.
;;; Code:

;; Libraries that we always want loaded.
(require 'rc)
(require 'packages)

;; Provide the library so that thinks correctly complete.
(provide 'all)
;;; all.el ends here
