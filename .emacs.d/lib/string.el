;;; string --- String library.
;;; Commentary:
;;; Code:
(packages/define string ()

  (defun string/split (str seperators)
    "Split STR at the first occurance of any seperator in SEPERATORS."
    (bind-head-tail (head tail) (split-string str seperators)
      (cons head (reduce 'concat tail)))))
