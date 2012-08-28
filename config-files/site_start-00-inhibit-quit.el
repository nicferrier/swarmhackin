;;; Some standard config for Emacs

(setq
 confirm-kill-emacs
 (lambda (prompt)
   (message
    "Quit prevented. Instead, use C-b d to detach.")
   nil))

;; Setup the package archives
(setq
 package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/"))


;;; End
