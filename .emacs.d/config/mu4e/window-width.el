(requires mu4e)

(add-hooks (window-configuration-change-hook)
  (setq erc-fill-column (- (window-width) 2)))
