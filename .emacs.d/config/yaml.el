;;; yaml --- Enable yaml support.
;;; Commentary:
;;; Code:

(eval-when-compile (require 'key-tree))

(packages/requires (yaml-mode key-tree ansible)
  (key-tree/add-mode-bindings yaml-mode
   y ("YAML Editting"
	a "Ansible Mode" 'ansible
	n "Newline and Indent" 'newline-and-indent)))
;;; yaml.el ends here
