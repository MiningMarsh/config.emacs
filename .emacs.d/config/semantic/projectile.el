;;; projectile --- Adds hooks to allow projectile and semantic to communicate.
;;; Commentary:
;;; Currently this file hooks the projectile project root into semantic, so
;;; semantic will automatically scan code in the current project root.
;;; Code:

(packages/requires (semantic projectile)

  ;; Can't figure out how to get projectile to you
  (setq semanticdb-project-root-functions
		(cons (lambda (dir)
				(ignore-errors
				  (with-working-directory "/home/developer/.emacs.d/config"
				    (projectile-project-root))))
		      semanticdb-project-root-functions)))
;;; projectile.el ends here
