;; $Id$
;; Pretty inaccurate right now, but I don't have
;; afterstep/nextstep/windowmaker here to compare against

(define-module (app scwm theme afterstep)
  #:use-module (app scwm theme-impl)
  #:use-module (app scwm base)
  #:use-module (app scwm decor)
  #:use-module (app scwm style)
  #:use-module (app scwm background)
  #:use-module (app scwm desk-background)
  #:use-module (app scwm xlib-drawing)
  #:use-module (app scwm face))

(define the-theme
  (let* ((afterstep-decor (make-decor))
	 (helvetica-12-font (make-font "-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*"))
	 (afterstep-style (make-style #:fg "grey90" #:bg "grey30" 
				#:icon #f ;; this turns off icons, but leaves icon titles
				#:show-icon #f ;; this turns off both icon titles and icons
				#:icon-box (list (x- 70) 1 69 (y- 141))
				#:border-width 6
				#:focus 'mouse
				#:sticky-icon #t
				#:mwm-func-hint #t #:mwm-decor-hint #t
				#:mwm-border #t
				#:hint-override #t #:decorate-transient #f
				#:PPosition-hint #f
				#:mini-icon  (load-theme-image "mini-term.xpm")
				#:button 1 #:button 3
				#:button 2 #:button 4
				#:no-button 5 #:no-button 7 #:no-button 9
				#:no-button 6 #:no-button 8
				#:no-side-decorations #t
				#:use-decor afterstep-decor))
	 (xpm-maximize (load-theme-image          "as-maximize-a.xpm"))
	 (xpm-minimize (load-theme-image          "as-minimize-a.xpm"))
	 (xpm-close (load-theme-image             "as-kill.xpm"))
	 (xpm-system (load-theme-image            "as-shade-a.xpm"))
	 (xpm-sticky (load-theme-image            "as-switchwindow-a.xpm"))
	 (xpm-extra-left (load-theme-image        "as-menu-a.xpm"))
	 (color-mask (logxor (color-property (make-color "black") 'pixel)
			     (color-property (make-color "#600000") 'pixel))))
    (with-decor afterstep-decor 
		(xlib-set-drawing-mask! color-mask)
		(set-rubber-band-mask! color-mask)
		(set-icon-font! helvetica-12-font)
		(set-highlight-foreground! "white")
		(set-highlight-background! "gray30")
		(set-rubber-band-mask! 255)
		(title-style #:justify 'left #:font helvetica-12-font
			     #:relief 'flat)
;;			     #:h-gradient (list 10 (list "gray0" 20)
;;						"gray0" "gray70"))
		(set-shadow-factor! .7)  ;; GJB:FIXME:: want default
		(set-highlight-factor! 1.2)  ;; GJB:FIXME:: want default
		
		(button-style 1 #:relief 'flat
			      #:pixmap xpm-system) ;;'mini-program-icon
		(button-style 3 #:relief 'flat
			      #:pixmap xpm-sticky)
		(button-style 5 #:relief 'flat
			      #:pixmap xpm-extra-left)
		(button-style 2 #:relief 'flat
			      #:pixmap xpm-maximize)
		(button-style 4 #:relief 'flat
			      #:pixmap xpm-minimize)
		(border-style #:no-inset #t #:hidden-handles #f))
    (make-theme "afterstep" #:window-style afterstep-style
		#:background-style 
		(lambda () 
		  (set-background-color! "black")
		  (desk-background #t #:color "black")))))
