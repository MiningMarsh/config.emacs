;;; pretty-symbols.el --- Pretty symbol rendering.
;;; Commentary:
;;; Code:
(packages/define pretty-symbols (cl-lib)
		 (defmacro pretty-symbols/-push-replacement-form (string char)
		   `(push '(,string . ,char)
			  prettify-symbols-alist))

		 (defmacro pretty-symbols/add (mode &rest replacements)
		   `(add-hooks (,(-> (symbol-name mode)
				     (concat "-hook")
				     intern-soft))

			       ,@(->> replacements
				      (partition 2)
				      (mapcar
				       (lambda (replacement)
					 (cl-destructuring-bind
					     (string char-symbol) replacement
					   (pretty-symbols/-push-replacement-form
					    string
					    (-> char-symbol
						symbol-name
						string-to-char)))))))))
;;; pretty-symbols.el ends here
