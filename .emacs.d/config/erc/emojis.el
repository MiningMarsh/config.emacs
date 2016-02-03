;;; emojis --- Enable emoji support in erc.
;;; Commentary:
;;; Code:
(require 'rc)

(requiring (smiley erc)
	   (add-to-list 'smiley-regexp-alist '("\\(:-?]\\)\\W" 1 "forced"))
	   (add-to-list 'smiley-regexp-alist '("\\s-\\(:-?/\\)\\W" 1 "wry"))
	   (add-to-list 'smiley-regexp-alist '("\\(:-?(\\)\\W" 1 "sad"))
	   (add-to-list 'smiley-regexp-alist '("\\((-?:\\)\\W" 1 "reverse-smile"))
	   (add-to-list 'smiley-regexp-alist '("\\(:-?D\\)\\W" 1 "grin"))
	   (add-to-list 'smiley-regexp-alist '("\\(:-?P\\)\\W" 1 "poke"))
	   (add-to-list 'erc-modules 'smiley)
	   (erc-update-modules))
;;; emojis.el ends here
