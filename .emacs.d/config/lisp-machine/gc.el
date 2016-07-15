;;; gc --- Enable idle garbage collection.
;;; Commentary:
;;; Code:

(setq gc-cons-threshold 10000000)

(defvar garbage-collection-timer nil
  "Timer that you can cancel, performs garbage collection on idle.")

(unless garbage-collection-timer
  (setq garbage-collection-timer
	(run-with-idle-timer 60 t 'garbage-collect)))
;;; gc.el ends here
