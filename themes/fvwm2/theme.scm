
(define-module (app scwm theme fvwm2)
  #:use-module (app scwm theme-impl)
  #:use-module (app scwm decor)
  #:use-module (app scwm style)
  #:use-module (app scwm face)
  #:use-module (app scwm background))

(define the-theme
  (let* ((fvwm2-decor (make-decor))
	 (times-14-font "-adobe-times-bold-r-*-*-14-*-*-*-*-*-*-*")
	 (fvwm2-style (make-style #:fg "black" #:bg "tan" #:show-icon #t
				  #:border-width 4 #:mwm-border #f
				  #:mwm-buttons #f
				  #:no-side-decorations #f
				  #:use-decor fvwm2-decor)))
    (with-decor fvwm2-decor
		(title-style #:justify 'center #:relief 'raised
			     #:font times-14-font)
		(border-style #:no-inset #f #:hidden-handles #f)
;;; Big Lighting Bolt (Fat at top, comes to a point)
		(button-style 1 #:relief-pattern
			      '((35 15 #t) (20 50 #t) (40 50 #f) (30 80 #t) 
					   (75 40 #f) (50 40 #t) (75 15 #f) 
					   (35 15 #t)))
;;; large up triangle
		(button-style 2 #:relief-pattern
			      '((50 25 #t) (75 75 #f) (25 75 #f) (50 25 #t)))
;;; large down triangle
		(button-style 4 #:relief-pattern
			      '((50 75 #t) (25 25 #t) (75 25 #t) (50 75 #f)))
		(set-hilight-foreground! "black")
		(set-hilight-background! "salmon"))
    (make-theme "fvwm2"	#:window-style fvwm2-style
		#:background-style 
		(lambda () (set-background-color! "gray50")))))
		
