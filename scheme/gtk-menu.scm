;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros
;;;; Use gtk menus

(define-module (app scwm gtk-menu)
  :use-module (app scwm base)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (app scwm optargs))

(define*-public (scwm-gtk-menu-title label #:key image-left)
  (if image-left
      (scwm-gtk-menuitem label #:image-left image-left)
      (scwm-gtk-menuitem label)))

(define-public scwm-gtk-menu-separator (gtk-menu-item-new))

(define*-public (scwm-gtk-menuitem label #:key image-above image-left
				   (fg #f) (bg #f) (font #f)
				   extra-label action submenu hover-action unhover-action
				   hotkey-prefs)
  (let ((forced-submenu? #f))
    (if (and action submenu)
	(error "Cannot give both an action and a submenu"))
    (if submenu
	(begin
	  (set! forced-submenu? #t)
	  (set! action submenu)))
    (let ((mi (gtk-menu-item-new))
	  (lab (gtk-label-new label)))
      (gtk-misc-set-alignment lab 0 .5)
      (gtk-widget-show lab)
      (let ((hbox (gtk-hbox-new #f 0))
	    (align (gtk-alignment-new .5 .5 0 0)))
	(gtk-widget-show hbox)
	(gtk-widget-show align)
	(gtk-container-add mi hbox)
	(gtk-widget-set-usize align 22 16)
	(if image-left
	    (let ((pix (image->gtk-pixmap image-left hbox)))
	      (gtk-container-add align pix)
	      (gtk-widget-show pix)))
	(gtk-box-pack-start hbox align #f #f 0)
	(gtk-box-pack-start hbox lab #t #t 4)
	(gtk-widget-show mi)
	(if extra-label
	    (let ((extralab (gtk-label-new extra-label)))
	      (gtk-misc-set-alignment extralab 0 1)
	      (gtk-box-pack-start hbox extralab #f #f 4)))
	(cond ((or forced-submenu? (gtk-menu? action) 
		   (and (symbol? action) (gtk-menu? (eval action (current-module)))))
	       (gtk-menu-item-set-submenu mi (if (symbol? action) (eval action (current-module)) action)))
	      ((or (procedure? action) (and (symbol? action) (procedure? (eval action (current-module)))))
	       (gtk-signal-connect mi "activate" (lambda () (if (symbol? action) ((eval action (current-module))) (action)))))))
      mi)))

;;(scwm-gtk-menuitem "foo" #:image-left pic-lambda-mini)


(define-public (menuitem->scwm-gtk-menuitem mi)
  (apply (lambda
	     (label action extra-label picture-above picture-left hover-action unhover-action hotkey-prefs forced-submenu?)
	   (if (eq? mi menu-separator)
	       (gtk-menu-item-new)
	       (if forced-submenu?
		   (scwm-gtk-menuitem label #:image-left picture-left #:submenu action)
		   (scwm-gtk-menuitem label #:image-left picture-left #:action action))))
	 (menuitem-properties mi)))

(define*-public (scwm-gtk-menu list-of-menuitems #:key
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
	   (let ((mi (if (gtk-menu-item? item)
			 item
			 (menuitem->scwm-gtk-menuitem item))))
	     (gtk-menu-append menu mi)
	     (gtk-widget-show mi)))
	 list-of-menuitems)
    (gtk-widget-show-all menu)
    menu))

(define*-public (scwm-gtk-popup-menu menu #:optional warp-to-index x-pos y-pos left-side? permit-alt-release-selection?)
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

(define-public scwm-gtk-menu-quit-verify
  (scwm-gtk-menu
   (list
    (scwm-gtk-menu-title "Really quit Scwm?")
    scwm-gtk-menu-separator
    (scwm-gtk-menuitem "Restart scwm" #:image-left "mini-turn.xpm"  #:extra-label "foo"
		       #:action (lambda () (display "restart scwm") (newline)))
    (scwm-gtk-menuitem "Restart development scwm" #:image-left "mini-turn.xpm" 
		       #:action (lambda () (display "restart dev scwm") (newline)))
    scwm-gtk-menu-separator
    (scwm-gtk-menuitem "&Yes" #:image-left "mini-exclam.xpm" #:action (lambda () (display "yes") (newline)))
    (scwm-gtk-menuitem "&No"  #:image-left "mini-cross.xpm" #:action noop))))


(scwm-gtk-popup-menu gtk-menu-quit-verify)
(scwm-gtk-popup-menu scwm-gtk-menu-quit-verify)
(bind-key 'all "H-t" (lambda () (scwm-gtk-popup-menu scwm-gtk-menu-quit-verify)))

(define menu-utilities
  (scwm-gtk-menu (cons
	 (menu-title "Utilities")
	 (make-menuitems-from-menu-information-list
	  utilities-available-program-information)
	 )))

!#
