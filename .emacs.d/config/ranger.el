
;;; ranger --- Enable ranger mode.
;;; Commentary:
;;; Code:

(packages/requires (ranger)
	   (setq ranger-override-dired t
		 ranger-cleanup-on-disable t
		 ranger-cleanup-eagerly t
		 ranger-modify-header t
		 ranger-hide-cursor t
		 ranger-preview-file t
		 ranger-show-literal t
		 ranger-width-preview 0.25
		 ranger-max-preview-size 5
		 ranger-dont-show-binary t
		 ranger-show-dotfiles nil
		 ranger-excluded-extensions '("mkv"
					      "iso"
					      "mp4")))
;;; ranger.el ends here
