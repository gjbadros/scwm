(define-module (app scwm theme win95)
  #:use-module (app scwm theme-impl)
  #:use-module (app scwm decor)
  #:use-module (app scwm style)
  #:use-module (app scwm background)
  #:use-module (app scwm face))

(define the-theme
  (let* ((win95-decor (make-decor))
	 (helvetica-12-font "-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*")

	 (win95-style (make-style #:fg "grey76" #:bg "grey76" #:show-icon #f
				  #:border-width 4 #:mwm-border #f
				  #:mwm-buttons #f
				  #:no-side-decorations #f
				  #:mini-icon (load-theme-image "mini-x.xpm")
				  #:use-decor win95-decor)))
    (with-decor win95-decor 
		(title-style #:justify 'left #:font helvetica-12-font
			     #:relief 'flat
			     #:solid "navy blue" 
			     #:inactive (list #:solid "grey50"))
		(border-style #:no-inset #t #:hidden-handles #t)
		(button-style 1 #:relief 'flat 
			      #:solid "navy blue" #:pixmap 'mini-program-icon
			      #:inactive 
			      (list #:solid "grey50" #:pixmap 'mini-program-icon))
		(button-style 2 #:relief 'flat
			      #:solid "navy blue"
			      #:pixmap (load-theme-image "win95-maximize-full.xpm")
			      #:inactive 
			      (list #:solid "grey50" 
				    #:pixmap (load-theme-image
					      "win95-maximize-full.xpm")))
		(button-style 4 #:relief 'flat
			      #:solid "navy blue"
			      #:pixmap (load-theme-image
					"win95-minimize-full.xpm")
			      #:inactive 
			      (list #:solid "grey50" 
				    #:pixmap (load-theme-image
					      "win95-minimize-full.xpm")))
		(set-hilight-foreground! "white")
		(set-hilight-background! "grey76"))
    
    (make-theme "win95" #:window-style win95-style
		#:background-style 
		(lambda () (set-background-color! "cyan4")))))

