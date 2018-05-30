;;; os.el --- Support library for detecting OS version.
;;; Commentary:
;;; Code:
(packages/define os ()
		 (defmacro os/when-linux (&rest body)
		   `(when (eq system-type 'gnu/linux)
		      ,@body))

		 (defun os/lock-screen ()
		   (os/when-linux
		    (! "qdbus org.kde.screensaver /ScreenSaver Lock"))))
;;; os.el ends here
