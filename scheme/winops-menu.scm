;;; $Id$
;;; winops-menu.scm
;;; (C) 1999 Greg J. Badros

(define-module (app scwm winops-menu)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm std-menus)
  :use-module (app scwm animation)
  :use-module (app scwm stylist)
  :use-module (app scwm window-selection)
  :use-module (app scwm group)
  :use-module (app scwm winops)
  :use-module (app scwm shove-window)
  :use-module (app scwm window-configuration)
  :use-module (app scwm xprop-extras)
  :use-module (app scwm animated-iconify)
  )

(define-public (make-small-window-ops-menu w)
  (menu
   (list
    (menuitem "&Move" #:image-left "mini-move.xpm" 
	      #:action interactive-move)
    (menuitem "Re&size" #:image-left "mini-resize.xpm" 
	      #:action interactive-resize)
    (menuitem (if (iconified-window? w)
		  "Unmi&nimize"
		  "Mi&nimize") #:image-left "mini-iconify.xpm" 
		  #:action animated-iconify)
    (menuitem (if (maximized? w) 
		  "Unma&ximize" 
		  "Ma&ximize") #:action toggle-maximize-both)
    (menuitem "Set &gravity" #:image-left "small-anchor.xpm"
	      #:action interactive-set-window-gravity!)
    menu-separator
    (menuitem "&Other" 
	      #:submenu
	      (menu 
	       (list
		(menuitem "&Raise" #:action raise-window)
		(menuitem "&Lower" #:action lower-window)
		(menuitem (if (sticky-window? w)
			      "Un&stick"
			      "&Stick") 
			  #:action toggle-stick)
		(menuitem (if (shaded-window? w)
			      "Uns&hade"
			      "S&hade")
			  #:action animated-toggle-window-shade)
		(menuitem (if (kept-on-top? w)
			      "Do not keep on top"
			      "Keep on top") 
			  #:action toggle-on-top))))
    (menuitem "Sho&ve" #:submenu menu-window-shove)
    (menuitem "&Configuration" #:submenu window-configuration-menu)
    (menuitem "&Title"
	      #:submenu
	      (menu
	       (list
		(menuitem "&Copy to X cut buffer"
			  #:action copy-window-title-to-cut-buffer)
		(menuitem "&Paste from X cut buffer"
			  #:action paste-window-title-from-cut-buffer))))
    (menuitem "St&yle"
	      #:submenu
	      (make-window-style-menu w))
    (menuitem "Grou&p"
	      #:submenu
	      (make-window-group-menu w))
    menu-separator
    (menuitem "Close" #:image-left "mini-cross.xpm" 
	      #:action close-window)
    (menuitem "Destroy" #:image-left "mini-bomb.xpm" 
	      #:action destroy-window))))

(define-public menu-window-ops
  (menu
   (list
    (menu-title "Window Ops") menu-separator
    (menuitem "&Move" #:image-left "mini-move.xpm" 
	       #:action interactive-move)
    (menuitem "&Resize" #:image-left "mini-resize.xpm" 
	       #:action interactive-resize)
    (menuitem "R&aise" #:image-left "mini-raise.xpm" 
	       #:action (thunk raise-window))
    (menuitem "&Lower" #:image-left "mini-lower.xpm" 
	       #:action (thunk lower-window))
    (menuitem "&Iconify/Restore" #:image-left "mini-iconify.xpm" 
	       #:action (thunk toggle-iconify))
    (menuitem "&Stick/Unstick" #:image-left "mini-stick.xpm" 
	       #:action (thunk toggle-stick))
    (menuitem "Ma&ximize/Reset" #:action (thunk toggle-maximize-both))
    (menuitem "Ma&ximize &Tall/Reset" #:image-left "mini-maxtall.xpm" 
	       #:action (thunk toggle-maximize-vertical))
    (menuitem "Maximize &Wide/Reset" #:image-left "mini-maxwide.xpm"
	       #:action (thunk toggle-maximize-horizontal))
    (menuitem "S&hade/Reset" 
	       #:action (thunk animated-toggle-window-shade))
    menu-separator
    (menuitem "Set &gravity" #:image-left "small-anchor.xpm"
	      #:action interactive-set-window-gravity!)
    (menuitem "Sho&ve" #:image-left "win-pos-center.xpm"
	      #:action menu-window-shove)
    (menuitem "Keep-&on-top/Reset" #:action (thunk toggle-on-top))
    (if (defined? 'print-window)
	(menuitem "&Print" #:action (thunk print-window))
	#f)
    menu-separator
    (menuitem "Group" #:submenu (lambda () (make-window-group-menu (window-context))))
    (menuitem "Change theme" #:submenu (lambda () (menu-window-theme)))
;;    (menuitem "Reload and change theme" #:submenu (lambda () (menu-window-theme #t)))
    menu-separator
    (menuitem "&Delete" #:action (thunk delete-window))
    (menuitem "&Close" #:image-left "mini-cross.xpm" 
	       #:action (lambda () (close-window (get-window))))
    (menuitem "&Kill" #:image-left "mini-bomb.xpm" 
	       #:action (thunk destroy-window))
    menu-separator
    (menuitem "Switch to..." #:action
	       (lambda () 
		 (show-window-list-menu #f #f #:show-geometry #t)))
    (menuitem "Re&fresh Screen" #:image-left "mini-ray.xpm" 
	       #:action (thunk refresh)))))
