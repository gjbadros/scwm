;; $Id$

(define-module (app scwm theme twm)
  #:use-module (app scwm theme-impl)
  #:use-module (app scwm base)
  #:use-module (app scwm decor)
  #:use-module (app scwm style)
  #:use-module (app scwm background)
  #:use-module (app scwm desk-background)
  #:use-module (app scwm xlib-drawing)
  #:use-module (app scwm face))

(define the-theme
  (let* ((twm-decor (make-decor))
	 (helvetica-12-font (make-font "-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*"))
	 (twm-style (make-style #:fg "light gray" #:bg "black" 
				#:icon #f ;; this turns off icons, but leaves icon titles
				#:show-icon #f ;; this turns off both icon titles and icons
				#:border-width 1
				#:focus 'mouse
				#:sticky-icon #t
				#:mwm-func-hint #t #:mwm-decor-hint #t
				#:mwm-border #t
				#:hint-override #t #:decorate-transient #f
				#:PPosition-hint #f
				#:mini-icon (load-theme-image "mini-term.xpm")
				#:no-side-decorations #f
				#:no-button 3 #:no-button 5 #:no-button 7
				#:no-button 4 #:no-button 6 #:no-button 8
				#:use-decor twm-decor))
	 (xpm-x      (load-theme-image          "twm-x.xpm"))
	 (xpm-boxes  (load-theme-image          "twm-boxes.xpm"))
	 (xpm-dot    (load-theme-image          "twm-dot.xpm"))
	 (color-mask (logxor (color-property (make-color "black") 'pixel)
			     (color-property (make-color "#600000") 'pixel))))
    (with-decor twm-decor 
		(xlib-set-drawing-mask! color-mask)
		(set-rubber-band-mask! color-mask)
		(set-icon-font! helvetica-12-font)
		(set-highlight-foreground! "light gray")
		(set-highlight-background! "maroon")
		(set-rubber-band-mask! 255)
		(title-style #:justify 'left #:font helvetica-12-font
			     #:relief 'flat #:height 20)
		(button-style 1 #:relief 'flat
			      #:pixmap xpm-x)
		(button-style 2 #:relief 'flat
			      #:pixmap xpm-boxes)
		(button-style 4 #:relief 'flat
			      #:pixmap xpm-dot)
		(border-style #:hidden-handles #t))
    (make-theme "twm" #:window-style twm-style
		#:background-style 
		(lambda () 
		  (set-background-color! "black")
		  (desk-background #t #:color "black")))))
