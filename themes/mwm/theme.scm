
(define-module (app scwm theme mwm)
  #:use-module (app scwm theme-impl)
  #:use-module (app scwm decor)
  #:use-module (app scwm style)
  #:use-module (app scwm face)
  #:use-module (app scwm background))

(define the-theme
  (let* ((mwm-decor (make-decor))
	 (times-14-font "-adobe-times-bold-r-*-*-14-*-*-*-*-*-*-*")
	 (mwm-style (make-style #:fg "black" #:bg "gray" #:show-icon #t
				#:border-width 6 #:mwm-border #t
				#:no-side-decorations #f
				#:mwm-buttons #t
				#:use-decor mwm-decor)))
    (with-decor mwm-decor
		(title-style #:justify 'center #:relief 'raised
			     #:font times-14-font)
		(border-style #:no-inset #f #:hidden-handles #f)
		(set-hilight-foreground! "black")
		(set-hilight-background! "cadet blue"))
    (make-theme "mwm" #:window-style mwm-style
		#:background-style
		(lambda () (reset-background!)))))




