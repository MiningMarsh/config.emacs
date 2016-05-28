;;; ac-ispell --- Enable auto-complete spelling completion.
;;; Commentary:
;;; Code:

(packages/requires (auto-complete ac-ispell)
	   (setq ispell-complete-word-dict (format "%s/.dictionary"
						   (getenv "HOME"))
		 ac-ispell-requires 3
		 ac-ispell-fuzzy-limit 5
		 ac-ispell-cache-size 1000)
	   (ac-ispell-setup)
	   (add-hooks (text-mode)
		      ac-ispell-ac-setup))
;;; ac-ispell.el ends here

