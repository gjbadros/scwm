;; $Id$

(define-module (app scwm theme gjb)
  #:use-module (app scwm theme-impl)
  #:use-module (app scwm base)
  #:use-module (app scwm decor)
  #:use-module (app scwm style)
  #:use-module (app scwm background)
  #:use-module (app scwm desk-background)
  #:use-module (app scwm xlib-drawing)
  #:use-module (app scwm face))

(define the-theme
  (let* ((gjb-decor (make-decor))
	 (helvetica-12-font (make-font "-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*"))
	 (gjb-style (make-style #:fg "black" #:bg "grey76" 
				#:icon #f ;; this turns off icons, but leaves icon titles
				#:show-icon #f ;; this turns off both icon titles and icons
				#:icon-box (list (x- 70) 1 69 (y- 141))
				#:border-width 4
				#:focus 'mouse
				#:sticky-icon #t
				#:mwm-func-hint #t #:mwm-decor-hint #t
				#:mwm-border #t
				#:hint-override #t #:decorate-transient #f
				#:PPosition-hint #f
				#:mini-icon  (load-theme-image "mini-term.xpm")
				#:button 3 #:button 4
				#:no-button 5 #:no-button 7
				#:no-button 6 #:no-button 8
				#:no-side-decorations #f
				#:use-decor gjb-decor))
	 (xpm-exp-win (load-theme-image          "mini-exp-windows-full.xpm"))
	 (xpm-hbars (load-theme-image            "mini-hbars-full.xpm"))
	 (xpm-lightbolt (load-theme-image        "mini-lightbolt-full.xpm"))
	 (xpm-shrink-windows (load-theme-image   "mini-shrink-windows-full.xpm"))
	 (xpm-stylized-x (load-theme-image       "mini-stylized-x-full.xpm"))
	 (xpm-sys3d (load-theme-image            "mini-sys3d-full.xpm"))
	 (xpm-term (load-theme-image             "mini-term.xpm"))
	 (color-mask (logxor (color-property (make-color "black") 'pixel)
			     (color-property (make-color "#600000") 'pixel))))
    (with-decor gjb-decor 
		(xlib-set-drawing-mask! color-mask)
		(set-rubber-band-mask! color-mask)
		(set-icon-font! helvetica-12-font)
		(set-highlight-foreground! "white")
		(set-highlight-background! "navyblue")
		(set-rubber-band-mask! 255)
		(title-style #:justify 'left #:font helvetica-12-font
			     #:relief 'raised)
		(set-shadow-factor! .7)  ;; GJB:FIXME:: want default
		(set-highlight-factor! 1.2)  ;; GJB:FIXME:: want default
		
		(button-style 1 #:relief 'flat
			      #:pixmap 'mini-program-icon)
		(button-style 3 #:relief 'flat
			      #:pixmap xpm-hbars)
		(button-style 5 #:relief 'flat
			      #:pixmap xpm-lightbolt)
		(button-style 2 #:relief 'flat
			      #:pixmap xpm-exp-win)
		(button-style 4 #:relief 'flat
			      #:pixmap xpm-shrink-windows)
		(border-style #:no-inset #t #:hidden-handles #t))
    (make-theme "gjb" #:window-style gjb-style
		#:background-style 
		(lambda () 
		  (set-background-color! "black")
		  (desk-background #t #:color "black")))))
