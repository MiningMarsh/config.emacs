;;; keybinds --- Add projectile keybinds.
;;; Commentary:
;;; Code:
(packages/requires (key-tree projectile ag)
		   (key-tree/add-bindings 
		    p ("Project"
		       f ("Find"
			  f "File" 'projectile-find-file
			  g "Directory" 'projectile-find-dir
			  t "Tag" 'projectile-find-tag)
		       i "Info" 'projectile-project-info
		       s "Switch Project" 'projectile-switch-project
		       o "Open Project" 'projectile-switch-open-project
		       S ("Search"
			  g "Grep" 'projectile-grep
			  a "Ag" 'projectile-ag)
		       v "Version Control" 'projectile-vc)))
;;; keybinds.el ends here
