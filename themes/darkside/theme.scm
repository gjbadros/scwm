;; $Id$

(define-module (app scwm theme darkside)
  #:use-module (app scwm theme-impl)
  #:use-module (app scwm base)
  #:use-module (app scwm decor)
  #:use-module (app scwm style)
  #:use-module (app scwm background)
  #:use-module (app scwm desk-background)
  #:use-module (app scwm xlib-drawing)
  #:use-module (app scwm face))

(define the-theme
  (let* ((darkside-decor (make-decor))
	 (helvetica-10-font (make-font "-adobe-helvetica-bold-r-*-*-10-*-*-*-*-*-*-*"))
	 (darkside-style (make-style #:fg "grey60" #:bg "#575757" 
				#:border-width 4
				#:focus 'sloppy
				#:sticky-icon #t
				#:mwm-func-hint #t #:mwm-decor-hint #t
				#:mwm-border #f
				#:hint-override #f #:decorate-transient #f
				#:PPosition-hint #f
				#:button 1 #:button 2
				#:button 3 #:button 4
				#:button 6 #:button 8
				#:no-button 5 #:no-button 7 #:no-button 9
;;				#:no-side-decorations #t
				#:use-decor darkside-decor))
	 (xpm-title (load-theme-image             "slate.xpm"))
;;	 (xpm-maximize (load-theme-image          "ds_max4.xpm"))
;;	 (xpm-maximize-in (load-theme-image       "ds_max4_in.xpm"))
;;	 (xpm-maximize-is (load-theme-image       "ds_max4_is.xpm"))
;;	 (xpm-minimize (load-theme-image          "ds_min4.xpm"))
;;	 (xpm-minimize-in (load-theme-image       "ds_min4_in.xpm"))
;;	 (xpm-minimize-is (load-theme-image       "ds_min4_is.xpm"))
	 (xpm-maximize (load-theme-image          "ds_maximize.xpm"))
	 (xpm-maximize-in (load-theme-image       "ds_maximize_in.xpm"))
	 (xpm-maximize-is (load-theme-image       "ds_maximize_is.xpm"))
	 (xpm-minimize (load-theme-image          "ds_minimize.xpm"))
	 (xpm-minimize-in (load-theme-image       "ds_minimize_in.xpm"))
	 (xpm-minimize-is (load-theme-image       "ds_minimize_is.xpm"))
	 (xpm-close (load-theme-image             "ds_kill.xpm"))
	 (xpm-close-in (load-theme-image          "ds_kill_in.xpm"))
	 (xpm-close-is (load-theme-image          "ds_kill_is.xpm"))
	 (xpm-system (load-theme-image            "ds_menu.xpm"))
	 (xpm-system-in (load-theme-image         "ds_menu_in.xpm"))
	 (xpm-system-is (load-theme-image         "ds_menu_is.xpm"))
	 (xpm-shade (load-theme-image             "ds_shade.xpm"))
	 (xpm-shade-in (load-theme-image          "ds_shade_in.xpm"))
	 (xpm-shade-is (load-theme-image          "ds_shade_is.xpm"))
	 (xpm-sticky (load-theme-image            "ds_stick.xpm"))
	 (xpm-sticky-in (load-theme-image         "ds_stick_in.xpm"))
	 (xpm-sticky-is (load-theme-image         "ds_stick_is.xpm"))

	 (color-mask (logxor (color-property (make-color "black") 'pixel)
			     (color-property (make-color "#600000") 'pixel))))
    (with-decor darkside-decor 
;;		(xlib-set-drawing-mask! color-mask)
;;		(set-rubber-band-mask! color-mask)
		(set-icon-font! helvetica-10-font)
		(set-highlight-foreground! "white")
		(set-highlight-background! "#575757")
;;		(set-rubber-band-mask! 255)
		(title-style #:justify 'left #:font helvetica-10-font
			     #:height 18
			     #:pixmap xpm-title
			     #:relief 'flat
			     #:inactive (list #:relief 'flat #:solid "#575757"))
;;			     #:h-gradient (list 10 (list "gray0" 20)
;;						"gray0" "gray70"))
		(set-shadow-factor! .8)  ;; GJB:FIXME:: want default
		(set-highlight-factor! 1.2)  ;; GJB:FIXME:: want default
		
		(button-style 1 #:pixmap xpm-system #:relief 'flat
			      #:active-down (list #:pixmap xpm-system-in)
			      #:inactive (list #:pixmap xpm-system-is))
		(button-style 3 #:pixmap xpm-sticky #:relief 'flat
			      #:active-down (list #:pixmap xpm-sticky-in)
			      #:inactive (list #:pixmap xpm-sticky-is))
		(button-style 2 #:pixmap xpm-close #:relief 'flat
			      #:active-down (list #:pixmap xpm-close-in)
			      #:inactive (list #:pixmap xpm-close-is))
		(button-style 4 #:pixmap xpm-maximize #:relief 'flat
			      #:active-down (list #:pixmap xpm-maximize-in)
			      #:inactive (list #:pixmap xpm-maximize-is))
		(button-style 6 #:pixmap xpm-minimize #:relief 'flat
			      #:active-down (list #:pixmap xpm-minimize-in)
			      #:inactive (list #:pixmap xpm-minimize-is))
		(button-style 8 #:pixmap xpm-shade #:relief 'flat
			      #:active-down (list #:pixmap xpm-shade-in)
			      #:inactive (list #:pixmap xpm-shade-is))
		(border-style #:no-inset #t #:hidden-handles #f))
    (make-theme "darkside" #:window-style darkside-style
		#:background-style 
		(lambda () 
		  (set-background-color! "black")
		  (desk-background #t #:color "black")))))

