;; $Id$
;; (scwm-path-prefix)
;; (documentation "resize-to")
;;

(resize-to 500 400 (select-window-interactively "Resize me"))

(define w (select-window-interactively))

;; (X-server-set-synchronize! #t)
(normal-border w)
(plain-border w)

(define doc-files        ; '("/usr/src/scwm/doc/scwm-procedures.txt")
  (list (string-append (scwm-path-prefix) "/share/scwm/scwm-procedures.txt")
	(string-append (scwm-path-prefix) "/share/scwm/cassowary-scm-procedures.txt")))

(normal-border (current-window-with-focus))
(plain-border (current-window-with-focus))

;; from gjb.scwmrc
(window-style "*" 
	      #:fg "black" #:bg "grey76" 
	      #:icon #f ;; this turns off icons, but leaves icon titles
	      #:show-icon #f ;; this turns off both icon titles and icons
	      #:icon-box (list (x- 70) 1 69 (y- 141))
	      #:border-width 4 ;; MS borderwidth of 4 looks slightly better
	                       ;; to my eye than 3.
	      #:focus 'mouse

;	      #:plain-border #t ;; replaces handle-width
				;; MS no, it replaces no-handles, plus
	                        ;; it doesn't work right now.
	      #:sticky-icon #t
	      #:random-placement #t #:smart-placement #t
	      #:mwm-func-hint #t #:mwm-decor-hint #t
	      #:int-override #t #:decorate-transient #f
	      #:PPosition-hint #f
	      #:mini-icon pic-xterm-mini)

;; from system.scwmrc
(window-style "*" 
	      #:fg "white" #:bg "purple" 
	      #:icon "unknown1.xpm" 
	      #:icon-box (list (x- 70) 1 69 (y- 141))
	      #:border-width 6 
	      #:focus 'mouse
	      #:random-placement #t #:smart-placement #t
	      #:mwm-func-hint #t #:mwm-decor-hint #t
	      #:int-override #t #:decorate-transient #t
	      #:PPosition-hint #f
	      #:lenience #t
	      )


(define fvwm-decor (make-decor))

(begin 
  (with-decor fvwm-decor
	      (title-style #:justify 'center #:relief 'raised
			   #:font times-14)
	      (border-style #:no-inset #f #:hidden-handles #f)
;;; Big Lighting Bolt (Fat at top, comes to a point)
;;	      (button-style 1 #:relief-pattern
;;			    '((35 15 #t) (20 50 #t) (40 50 #f) (30 80 #t) 
;;					 (75 40 #f) (50 40 #t) (75 15 #f) 
;;					 (35 15 #t)))
;;; large up triangle
	      (button-style 2 #:relief-pattern
			    '((50 25 #t) (75 75 #f) (25 75 #f) (50 25 #t)))
;;; large down triangle
	    (button-style 4 #:relief-pattern
			  '((50 75 #t) (25 25 #t) (75 25 #t) (50 75 #f)))
	    (set-hilight-foreground! "black")
	    (set-hilight-background! "salmon"))

  (define fvwm-style 
    (make-style #:fg "black" #:bg "tan" #:show-icon #t
		#:border-width 10 #:mwm-border #f
		#:mwm-buttons #f
		#:use-decor fvwm-decor))
 
  
  (window-style "*" #:use-style fvwm-style)
  )

(refresh)
