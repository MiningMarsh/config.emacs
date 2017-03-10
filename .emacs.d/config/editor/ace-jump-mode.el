;;; ace-jump-mode --- Enable ace-jump commands.
;;; Commentary:
;;; Code:
(packages/requires (ace-jump-mode key-tree)
  (key-tree/add-editor-bindings j "Jump" 'ace-jump-mode))
;;; ace-jump-mode.el ends here
