(define-module (app scwm theme woven)
  #:use-module (app scwm theme-impl)
  #:use-module (app scwm base)
  #:use-module (app scwm decor)
  #:use-module (app scwm style)
  #:use-module (app scwm background)
  #:use-module (app scwm desk-background)
  #:use-module (app scwm xlib-drawing)
  #:use-module (app scwm face))

(define the-theme
  (let* ((woven-decor (make-decor))
	 (helvetica-12-font (make-font "-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*"))
	 (woven-style (make-style #:fg "AntiqueWhite" #:bg "#300000" 
				  #:mwm-border #t
				  #:icon #f #:force-icon #t #:show-icon #t ;; icon titles only
				  #:no-side-decorations #f
				  #:button 3
				  #:button 6
				  #:border-width 5 
				  #:use-decor woven-decor))
	 (ghost-x (load-theme-image         "mini-x-ghost.xpm"))
	 (antique-x (load-theme-image       "mini-x-antique.xpm"))
	 (ghost-s-weave (load-theme-image   "mini-s-weave-ghost.xpm"))
	 (antique-s-weave (load-theme-image "mini-s-weave-antique.xpm"))
	 (ghost-l-weave (load-theme-image   "mini-l-weave-ghost.xpm"))
	 (antique-l-weave (load-theme-image "mini-l-weave-antique.xpm"))
	 (ghost-t-weave (load-theme-image   "mini-t-weave-ghost.xpm"))
	 (antique-t-weave (load-theme-image "mini-t-weave-antique.xpm"))
	 (ghost-w-weave (load-theme-image   "mini-w-weave-ghost.xpm"))
	 (antique-w-weave (load-theme-image "mini-w-weave-antique.xpm"))
	 (color-mask (logxor (color-property (make-color "black") 'pixel)
			     (color-property (make-color "#600000") 'pixel))))
    (with-decor woven-decor 
		(xlib-set-drawing-mask! color-mask)
		(set-rubber-band-mask! color-mask)
		(set-icon-font! helvetica-12-font)
		(set-hilight-foreground! "ghost white")
		(set-hilight-background! "#600000")
		(set-rubber-band-mask! 127)
		(title-style #:justify 'left #:font helvetica-12-font
			     #:relief 'flat #:height 20)
		(set-shadow-factor! .7)
		(set-hilight-factor! 1.2)
		
		(button-style 1 #:relief 'flat
			      #:solid "#600000" #:pixmap ghost-x
                              #:active-down (list #:relief 'raised
                                                  #:solid "#600000" 
                                                  #:pixmap ghost-x)
			      #:inactive (list #:relief 'flat
                                               #:solid "#300000"
					       #:pixmap antique-x))
		(button-style 3 #:relief 'flat
			      #:solid "#600000" #:pixmap ghost-t-weave
                              #:active-down (list #:relief 'raised
                                                  #:solid "#600000" 
                                                  #:pixmap ghost-t-weave)
			      #:inactive (list  #:relief 'raised
                                                #:solid "#300000"
                                                #:pixmap antique-t-weave))
		(button-style 2 #:relief 'flat
			      #:solid "#600000" #:pixmap ghost-l-weave
                              #:active-down (list #:relief 'raised
                                                  #:solid "#600000" 
                                                  #:pixmap ghost-l-weave)
			      #:inactive (list #:relief 'flat
                                               #:solid "#300000"
					       #:pixmap antique-l-weave))
		(button-style 4 #:relief 'flat #:solid "#600000"
			      #:pixmap ghost-s-weave
                              #:active-down (list #:relief 'raised
                                                  #:solid "#600000" 
                                                  #:pixmap ghost-s-weave)
		      #:inactive (list #:relief 'flat
                                               #:solid "#300000"
					       #:pixmap antique-s-weave))
  		(button-style 6 #:relief 'flat #:solid "#600000"
  			      #:pixmap ghost-w-weave
                              #:active-down (list #:relief 'raised
                                                  #:solid "#600000" 
                                                  #:pixmap ghost-w-weave)
  			      #:inactive (list #:relief 'flat
                                               #:solid "#300000"
  					       #:pixmap antique-w-weave))
		(border-style #:no-inset #t #:hidden-handles #t))
    
    (make-theme "woven" #:window-style woven-style
		#:background-style 
		(lambda () 
		  (set-background-color! "black")
		  (desk-background #t #:color "black")))))


