;;; $Id$
;;; debianapps-menu.scm
;;; (C) 1999 Francesco Tapparo and Greg J. Badros
;;;
;;; For Debian GNU/Linux systems.


(define-module (app scwm debianapps-menu)
  :use-module (app scwm std-menus)
  :use-module (app scwm base)
  :use-module (app scwm optargs))

(define-public default-title "Debian Apps")
(define-public default-menu-file "/etc/X11/scwm/scwm_menus")

(define*-public (make-debianapps-menu #&optional 
				      (debian-title default-title)
				      (debian-menu-file default-menu-file))
"Read the menu-generated file debian-menu-file, and return a menu with title 
 menu title. This should work only on debian systems"
  (load debian-menu-file) 
  (menu		
   (append
    (list (menuitem debian-title #f)
	  menu-separator)
    /Debian-list
    (list 
     menu-separator
     (menuitem "Desks" #:submenu menu-desk)
     menu-separator
     (menuitem "Themes" #:submenu (lambda () (make-menu-global-theme)))
     menu-separator
     (menuitem "Quit" #:submenu menu-quit-verify)))))
