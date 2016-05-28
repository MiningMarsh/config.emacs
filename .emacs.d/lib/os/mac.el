;;; mac --- Library for manipulating Mac OS X.
;;; Commentary:
;;; Code:
(packages/define mac ()
  (defun mac/lock-account ()
    "Lock the current user account."
    (start-process "Lock Account" "*Lock Account*"
		   "/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession"
		   "-suspend"))

  (defun mac/shutdown ()
    "Shutdown the system."
    (shell-command-to-string "osascript -e 'tell app \"System Events\" to shut down'"))

  (defun mac/restart ()
    "Restart the system."
    (shell-command-to-string "osascript -e 'tell app \"System Events\" to restart'"))

  (defun mac/log-out ()
    "Log out of the current user account."
    (shell-command-to-string "osascript -e 'tell app \"System Events\" to log out'"))

  (defun mac/sleep ()
    "Put the system to sleep."
    (shell-command-to-string "osascript -e 'tell app \"System Events\" to sleep'")))
;;; mac.el ends here
