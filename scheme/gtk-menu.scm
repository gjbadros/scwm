;;;; $Id$
;;;; (C) 1999 Greg J. Badros
;;;; Use gtk menus

(define-module (app scwm gtk-menu)
  :use-module (app scwm base)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))

(define*-public (scwm-gtk-menu list-of-menuitems #&key
			       (image-side 'menu-side-image)
			       (image-align 'top)
			       (color-bg-image-side 'menu-side-bg-color)
			       (image-bg 'menu-bg-image)
			       (color-text 'menu-text-color)
			       (color-bg 'menu-bg-color)
			       (color-stipple 'menu-stipple-color)
			       (hl-color-fg 'menu-hl-fg-color)
			       (hl-color-bg 'menu-hl-bg-color)
			       (hl-relief? #t)
			       (font 'menu-font)
			       (look 'menu-look)
			       popup-delay hover-delay
			       (extra #f))
  (let ((menu (gtk-menu-new)))
    (map (lambda (item)
	   (let* ((menu-item (gtk-menu-item-new))
		  (props (menuitem-properties item)))
	     (apply (lambda
			(label action extra-label picture-above picture-left hover-action unhover-action hotkey-prefs forced-submenu?)
		      (let ((mi (if (eq? item menu-separator)
				    (gtk-menu-item-new)
				    (gtk-menu-item-new-with-label label))))
			(cond ((or forced-submenu? (gtk-menu? action) 
				   (and (symbol? action) (gtk-menu? (eval action))))
			       (gtk-menu-item-set-submenu mi (if (symbol? action) (eval action) action)))
			      ((or (procedure? action) (and (symbol? action) (procedure? (eval action))))
			       (gtk-signal-connect mi "activate" (lambda () (if (symbol? action) ((eval action)) (action))))))
			(gtk-menu-append menu mi)
			(gtk-widget-show mi)))
		    props)))
	 list-of-menuitems)
    (gtk-widget-show-all menu)
    menu))

(define*-public (scwm-gtk-popup-menu menu #&optional warp-to-index x-pos y-pos left-side? permit-alt-release-selection?)
  (gtk-menu-popup menu #f #f 0 0))

#!

;; gtk-menu
(define-public gtk-menu-quit-verify
  (scwm-gtk-menu
   (list
    (menu-title "Really quit Scwm?")
    menu-separator
    (menuitem "Restart scwm" #:image-left "mini-turn.xpm" 
	       #:action (lambda () (display "restart scwm") (newline)))
    (menuitem "Restart development scwm" #:image-left "mini-turn.xpm" 
	      #:action (lambda () (display "restart dev scwm") (newline)))
    menu-separator
    (menuitem "&Yes" #:image-left "mini-exclam.xpm" #:action (lambda () (display "yes") (newline)))
    (menuitem "&No"  #:image-left "mini-cross.xpm" #:action noop))))

(scwm-gtk-popup-menu gtk-menu-quit-verify)

(define menu-utilities
  (scwm-gtk-menu (cons
	 (menu-title "Utilities")
	 (make-menuitems-from-menu-information-list
	  utilities-available-program-information)
	 )))

;;; (scwm-gtk-popup-menu menu-utilities)

!#
