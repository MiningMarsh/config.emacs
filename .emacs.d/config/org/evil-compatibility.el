;;; evil-compatibility --- Adds evil keybindings to org-mode.
;;; Commentary:
;;; Just a shim for loading evil-org-mode.
;;; Code:
(require 'rc)
(require 'packages)

(packages/requires (evil evil-leader org evil-org))
;;; evil-compatibility.el ends here
