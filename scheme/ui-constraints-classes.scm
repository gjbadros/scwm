;; $Id$
;; Copyright (C) 1999 Greg J. Badros and Jeff Nichols

(define-module (app scwm ui-constraints-classes)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm message-window)
  :use-module (app scwm window-locations)
  :use-module (app scwm nonants)
  :use-module (app scwm window-selection)
  :use-module (app scwm winops)
  :use-module (app scwm nonants)
  :use-module (cassowary constraints)
  :use-module (app scwm xlib-drawing))

;; (reset-constraints)

;;; GJB:FIXME:: we'd like to be able to use color, mode
;;; but cannot w/o using an overlay plane;  Also,
;;; the draw functions should get passed semantic parameters
;;; e.g., is-enabled? and is-in-focus? instead of color, width

(define-public ui-constraint-prompter-msgwin (make-message-window ""))

(message-window-style ui-constraint-prompter-msgwin
		      #:font (make-font "*helvetica*bold-r*24*")
		      #:fg "yellow" #:bg "black")

;; (select-window-interactively "Select one:" ui-constraint-prompter-msgwin)

(xlib-set-drawing-mask! 2147483647)  ;; 2^31-1, nice for 32 bit displays

;; public drawing variables

;;; (set! ui-constraint-out-focus-width 3)
;;; (draw-all-constraints)
(define-public ui-constraint-in-focus-width 4)
(define-public ui-constraint-out-focus-width 2)

;; alias for internal use
(define msgwin ui-constraint-prompter-msgwin)

;; generic menuname-proc
(define (menuname-as-class-name ui-constraint)
  (ui-constraint-class-name (ui-constraint-class ui-constraint)))

;; generic menuname-proc for >2 window constraints
(define (menuname-as-win-num ui-constraint)
  (string-append (menuname-as-class-name ui-constraint) ": " 
		 (number->string (length (ui-constraint-windows ui-constraint)))
		 " windows"))

;; helpful draw utilities
(define (draw-window-line-anchor point radius)
  "Draw the circles that anchor a line to a window."
  (let ((diameter (* 2 radius)))
    (xlib-draw-arc! (cons (- (car point) radius) (- (cdr point) radius))
		    diameter diameter 0 360)))

(define (draw-arrow base height point dir)
  "Draw a hollow triangular arrowhead with BASE & HEIGHT with the tip at POINT.
DIR specified the direction the arrow is pointing.  'top, 'left, 'right, and 
'bottom and possible options.  LINE-WIDTH is the width of lines that will be
drawn.  POINT is of the form (X . Y)."
  (let ((px (car point))
	(py (cdr point))
	(halfbase (/ base 2))
	(p1x 0) ;; p1 is one base point
	(p1y 0)
	(p2x 0) ;; p2 is the other base point
	(p2y 0))
    (cond ((eq? dir 'top)
	   (set! p1x (- px halfbase))
	   (set! p1y (+ py height))
	   (set! p2x (+ p1x base))
	   (set! p2y p1y))
	  ((eq? dir 'bottom)
	   (set! p1x (- px halfbase))
	   (set! p1y (- py height))
	   (set! p2x (+ p1x base))
	   (set! p2y p1y))
	  ((eq? dir 'left)
	   (set! p1x (+ px height))
	   (set! p1y (- py halfbase))
	   (set! p2x p1x)
	   (set! p2y (+ p1y base)))
	  ((eq? dir 'right)
	   (set! p1x (- px height))
	   (set! p1y (- py halfbase))
	   (set! p2x p1x)
	   (set! p2y (+ p1y base))))
    (xlib-draw-line! (cons px py)   (cons p1x p1y))
    (xlib-draw-line! (cons px py)   (cons p2x p2y))
    (xlib-draw-line! (cons p1x p1y) (cons p2x p2y))))


;; helpful prompter for two-window constraints
(define-public (two-window-prompter name p1 p2)
  (let ((winlist (selected-windows-list)))
    (if (eqv? (length winlist) 2)
	(begin
	  (unselect-all-windows)
	  (list winlist))
	(let ((w1 (select-window-interactively (string-append name ": " p1) msgwin)))
	  (if w1
	      (let ((w2 (select-window-interactively (string-append name ": " p2) msgwin)))
		(if w2
		    (list (list w1 w2))
		    #f))
	      #f)))))

;; helpful prompter for two-window constraints
;; NOTE: Will either return all windows in selection list
;; (if size of list greater than 1) or prompts the user to
;; select two windows.
(define-public (two-window-or-more-prompter name p1 p2)
  (let ((winlist (selected-windows-list)))
    (if (>= (length winlist) 2)
	(begin
	  (unselect-all-windows)
	  (list winlist))
	(let ((w1 (select-window-interactively (string-append name ": " p1) msgwin)))
	  (if w1
	      (let ((w2 (select-window-interactively (string-append name ": " p2) msgwin)))
		(if w2
		    (list (list w1 w2))
		    #f))
	      #f)))))


(define*-public (one-window-prompter name #&optional (p1 "select window"))
  (let ((winlist (selected-windows-list)))
    (if (eqv? (length winlist) 1)
	(begin 
	  (unselect-all-windows)
	  winlist)
	(let ((win (select-window-interactively 
		    (string-append name ": " p1) msgwin)))
	  (if win (list win) #f)))))


(define*-public (two-window-or-more-nonant-prompter name #&optional (p1 "first window") (p2 "second window"))
  (let ((winlist (selected-windows-list)))
    (if (>= (length winlist) 2)
	(begin
	  (unselect-all-windows)
	  (list winlist (map (lambda (w) (object-property w 'nonant)) winlist)))
	(let ((win1 #f)
	      (win2 #f)
	      (nonant1 #f)
	      (nonant2 #f))
	  (message-window-set-message! msgwin (string-append name ": " p1))
	  (with-message-window-shown
	   msgwin
	   (set! win1 (get-window-with-nonant-interactively))
	   (set! nonant1 (object-property win1 'nonant))
	   (message-window-set-message! msgwin (string-append name ": " p2))
	   (set! win2 (get-window-with-nonant-interactively))
	   (set! nonant2 (object-property win2 'nonant))
	   (list (list win1 win2) (list nonant1 nonant2)))))))
;; (define-public ui-constraint-prompter-msgwin (make-message-window ""))
;; (define msgwin ui-constraint-prompter-msgwin)
;; (define name "foo")
;; (define p1 "p1")
;; (define p2 "p2")
;; nonants
;; (get-window-with-nonant-interactively)
;; ui-constraints-classes
;; (two-window-or-more-nonant-prompter "foo")

;; Perhaps should move this elsewhere
;; Useful drawing utilities function
;; TRANSLATE POINT
;; takes a point in the (x . y) and translates it by dx, dy.
(define (translate-point pt dx dy)
  (let ((x (car pt))
	(y (cdr pt)))
    (cons (+ x dx) (+ y dy))))


;;-------------------------------------------------
;; Anchor Constraint
;;-------------------------------------------------

;; translates a nonant vector into a string for display
(define (dirvector->string vec)
  (string-append 
   (if (vector-ref vec 0) "N" "")
   (if (vector-ref vec 3) "S" "")
   (if (vector-ref vec 1) "W" "")
   (if (vector-ref vec 2) "E" "")))

;; the anchor constraint stores the clv in the first element of OPTS and 
;; constraint nonant in the second element
(define (menuname-anchor ui-constraint)
  (let* ((opts (ui-constraint-opts ui-constraint))
	 (name (ui-constraint-class-name (ui-constraint-class ui-constraint)))
	 (nonant (car opts))
	 (val (dirvector->string nonant)))
    (string-append name ": " val)))

(define (draw-anchor-symbol pt size)
  (let* ((dbsize (* 2 size))
	 (hlfsize (quotient size 2))
	 (qtrsize (quotient hlfsize 2))
	 (topleft (translate-point pt (- size) (- (+ size hlfsize))))
	 (top1    (translate-point pt (- qtrsize) (- hlfsize)))
	 (top2    (translate-point pt (+ qtrsize) (- hlfsize)))
	 (top3    (translate-point pt 0 (- hlfsize)))
	 (bot1    (translate-point pt 0 (+ hlfsize))))
    (xlib-draw-line! top1 top2)
    (xlib-draw-line! top3 bot1)
    (xlib-draw-arc!  topleft dbsize dbsize 230 80)))


(define-public (nonant->dirvector nonant)
  (let ((vec (make-vector 4 #f)))
    (case nonant
      ((0 1 2 4) (vector-set! vec 0 #t))) ;; constrain top
    (case nonant
      ((0 3 4 6) (vector-set! vec 1 #t))) ;; constrain left
    (case nonant
      ((2 4 5 8) (vector-set! vec 2 #t))) ;; constrain right
    (case nonant
      ((4 6 7 8) (vector-set! vec 3 #t))) ;; constrain bottom
    vec))

;; ui constructor
(define (ui-cnctr-anchor)
  (let ((winlist (selected-windows-list)))
    (if (eqv? (length winlist) 1)
	(begin 
	  (unselect-all-windows)
	  (list (car winlist) 
		(nonant->dirvector 
		 (object-property (car winlist) 'nonant)))) ;; give the default nonant as the middle
	(begin
	  (message-window-set-message! msgwin "Window nonant to anchor?")
	  (with-message-window-shown 
	   msgwin
	   (let* ((win (get-window-with-nonant-interactively))
		  (nonant (object-property win 'nonant)))
	     (list win (nonant->dirvector nonant))))))))

;; (object-property #f 'nonant)

;; (nonant->dirvector (get-window-nonant (select-viewport-position)))
;; (cnctr-anchor (select-window-interactively) #(#t #f #f #f) #t)
;; constructor
(define* (cnctr-anchor w1 nonant #&optional (enable? #f))
  (if (not (window? w1))
      (error "No window selected"))
  (let ((top (cdr (window-center-top w1)))
	(bot (cdr (window-center-bottom w1)))
	(rgt (car (window-right-middle w1)))
	(lft (car (window-left-middle w1)))
	(cn-list ())
	(sc-list ())
	(clv-list ()))

    (if (vector-ref nonant 0)
	(let* ((w1-yt (window-clv-yt w1))
	       (clv (make-cl-variable "clvt" top))
	       (cn (make-cl-constraint w1-yt = clv))
	       (sc (make-cl-stay-constraint clv cls-required 10)))
	  (cl-add-constraint (scwm-master-solver) cn)
	  (and enable? (cl-add-constraint (scwm-master-solver) sc))
	  (set! cn-list (cons cn cn-list))
	  (set! sc-list (cons sc sc-list))
	  (set! clv-list (cons clv clv-list))))

    (if (vector-ref nonant 1)
	(let* ((w1-xl (window-clv-xl w1))
	       (clv (make-cl-variable "clvl" lft))
	       (cn (make-cl-constraint w1-xl = clv))
	       (sc (make-cl-stay-constraint clv cls-required 10)))
	  (cl-add-constraint (scwm-master-solver) cn)
	  (and enable? (cl-add-constraint (scwm-master-solver) sc))
	  (set! cn-list (cons cn cn-list))
	  (set! sc-list (cons sc sc-list))
	  (set! clv-list (cons clv clv-list))))

    (if (vector-ref nonant 2)
	(let* ((w1-xr (window-clv-xr w1))
	       (clv (make-cl-variable "clvr" rgt))
	       (cn (make-cl-constraint w1-xr = clv))
	       (sc (make-cl-stay-constraint clv cls-required 10)))
	  (cl-add-constraint (scwm-master-solver) cn)
	  (and enable? (cl-add-constraint (scwm-master-solver) sc))
	  (set! cn-list (cons cn cn-list))
	  (set! sc-list (cons sc sc-list))
	  (set! clv-list (cons clv clv-list))))

    (if (vector-ref nonant 3)
	(let* ((w1-yb (window-clv-yb w1))
	       (clv (make-cl-variable "clvb" bot))
	       (cn (make-cl-constraint w1-yb = clv))
	       (sc (make-cl-stay-constraint clv cls-required 10)))
	  (cl-add-constraint (scwm-master-solver) cn)
	  (and enable? (cl-add-constraint (scwm-master-solver) sc))
	  (set! cn-list (cons cn cn-list))
	  (set! sc-list (cons sc sc-list))
	  (set! clv-list (cons clv clv-list))))

    (list sc-list (list w1) nonant cn-list clv-list)))

;; draw-proc
(define (draw-cn-anchor ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(opts (ui-constraint-opts ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 1))
	(error "Expected only one window in win-list of cn for anchor"))
    (or opts (error "Expected some optional information for an anchor."))
    (let* ((w (car win-list))
	   (wt (window-center-top w))
	   (wb (window-center-bottom w))
	   (wl (window-left-middle w))
	   (wr (window-right-middle w))
	   (nonant (car opts)))
      (if (not (vector? nonant))
	  (error "Nonant value in opts list should be a vector."))
      (xlib-set-line-attributes! width)
      (if (vector-ref nonant 0)
	  (draw-anchor-symbol wt 20))
      (if (vector-ref nonant 1)
	  (draw-anchor-symbol wl 20))
      (if (vector-ref nonant 2)
	  (draw-anchor-symbol wr 20))
      (if (vector-ref nonant 3)
	  (draw-anchor-symbol wb 20)))))

;; define the anchor type constraint
(define-public uicc-anchor
  (make-ui-constraint-class 
   "Anchor"
   "Anchor a window nonant.
Keeps a window nonant in its current location. \
You can anchor any of the 9 nonants of a window, \
e.g., click in the middle and center of a window \
to make it resize around its center." 
   '(1 1) cnctr-anchor 
   ui-cnctr-anchor draw-cn-anchor 
   cl-is-constraint-satisfied? 
   "anchor.xpm" #f menuname-anchor))

;;-----------------------------------------------------------------------
;; alignment constraint
;;
;; forces the alignment of some windows based upon
;; where the user clicks in the window
;;-----------------------------------------------------------------------

;; horizontal alignment

;; ui-constructor
(define (make-ui-cnctr-align horiz-or-vert)
  (lambda ()
    (two-window-or-more-nonant-prompter (string-append horiz-or-vert " alignment"))))

;; get the proper constraint var for alignment
(define (get-hcl-from-nonant win nonant)
  (case nonant
    ((0 1 2) (window-clv-yt win))
    ((3 4 5) (cl-plus (window-clv-yt win) (cl-divide (window-clv-height win) 2)))
    ((6 7 8) (window-clv-yb win))
    (else (error "Bad nonant"))))

;; constructor
(define* (cnctr-halign wlist qlist #&optional (enable? #f))
  (let* ((var1 (get-hcl-from-nonant (car wlist) (car qlist)))
	 (varlist (map (lambda (w q) (get-hcl-from-nonant w q)) (cdr wlist) (cdr qlist)))
	 (cnlist (map (lambda (v) (make-cl-constraint var1 = v)) varlist)))
    (and enable? (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist))
    (list cnlist wlist qlist)))

;; get proper position for drawing
(define (get-hpos-from-nonant win nonant)
  (case nonant
    ((0 1 2) (window-center-top win))
    ((3 4 5) (window-center-middle win))
    ((6 7 8) (window-center-bottom win))
    (else (error "Bad nonant"))))

;; draw the alignment constraint
(define (draw-cn-halign ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(opts (car (ui-constraint-opts ui-constraint)))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (or opts (error "Expected some optional information for an alignment constraint."))
    (let* ((w1 (car win-list))
	   (q1 (car opts))
	   (w1pos (get-hpos-from-nonant w1 q1)))
      (xlib-set-line-attributes! width)
      (draw-window-line-anchor w1pos 5)
      (for-each (lambda (w q)
		  (let ((wpos (get-hpos-from-nonant w q)))
		    (xlib-draw-line! w1pos wpos)
		    (draw-window-line-anchor wpos 5)))
		(cdr win-list) (cdr opts)))))

;; halign-nonant->string
;; converts a normal nonant value to a string
(define (halign-nonant->string nonant)
  (case nonant
    ((0 1 2) "top")
    ((3 4 5) "middle")
    ((6 7 8) "bottom")))

;; menuname code for alignment
(define (menuname-halign ui-constraint)
  (let ((numwin (length (ui-constraint-windows ui-constraint))))
    (if (> numwin 2)
	(menuname-as-win-num ui-constraint)
	(let* ((opts (car (ui-constraint-opts ui-constraint)))
	       (name (ui-constraint-class-name (ui-constraint-class ui-constraint)))
	       (q1 (car opts))
	       (q2 (cadr opts))
	       (val (string-append (halign-nonant->string q1) "<->" (halign-nonant->string q2))))
	  (string-append name ": " val)))))

;; define the horiz. alignment constraint
(define-public uicc-halign
  (make-ui-constraint-class 
   "Horizontal alignment"
   "Horizontal alignment.  \n\
Keep windows together using a horizontal connecting bar. \
Also can be used to glue top and bottom edges of windows together."
   '(2 '+) cnctr-halign 
   (make-ui-cnctr-align "Horizontal") draw-cn-halign 
   cl-is-constraint-satisfied? 
   "cn-keep-tops-even.xpm" "cn-keep-top-bottom-even.xpm" menuname-halign))

;; (ui-constraint-class-pixmap-name uicc-halign)
;; (ui-constraint-class-pixmap2-name uicc-halign)
;; (ui-constraint-class-pixmap-name uicc-valign)
;; (ui-constraint-class-pixmap2-name uicc-valign)


;; vertical alignment

;; get the proper constraint var for alignment
(define (get-vcl-from-nonant win nonant)
  (case nonant
    ((0 3 6) (window-clv-xl win))
    ((1 4 7) (cl-plus (window-clv-xl win) (cl-divide (window-clv-width win) 2)))
    ((2 5 8) (window-clv-xr win))
    (else (error "Bad nonant"))))

;; constructor
(define* (cnctr-valign wlist qlist #&optional (enable? #f))
  (let* ((var1 (get-vcl-from-nonant (car wlist) (car qlist)))
	 (varlist (map (lambda (w q) (get-vcl-from-nonant w q)) (cdr wlist) (cdr qlist)))
	 (cnlist (map (lambda (v) (make-cl-constraint var1 = v)) varlist)))
    (and enable? (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist))
    (list cnlist wlist qlist)))

;; get proper position for drawing 
(define (get-vpos-from-nonant win nonant)
  (case nonant
    ((0 3 6) (window-left-middle win))
    ((1 4 7) (window-center-middle win))
    ((2 5 8) (window-right-middle win))
    (else (error "Bad nonant"))))

;; draw the alignment constraint
(define (draw-cn-valign ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(opts (car (ui-constraint-opts ui-constraint)))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (or opts (error "Expected some optional information for an alignment constraint."))
    (let* ((w1 (car win-list))
	   (q1 (car opts))
	   (w1pos (get-vpos-from-nonant w1 q1)))
      (xlib-set-line-attributes! width)
      (draw-window-line-anchor w1pos 5)
      (for-each (lambda (w q)
		  (let ((wpos (get-vpos-from-nonant w q)))
		    (xlib-draw-line! w1pos wpos)
		    (draw-window-line-anchor wpos 5)))
		(cdr win-list) (cdr opts)))))

;; valign-nonant->string
;; converts a normal nonant value to a string
(define (valign-nonant->string nonant)
  (case nonant
    ((0 3 6) "left")
    ((1 4 7) "center")
    ((2 5 8) "right")))

;; menuname code for alignment
(define (menuname-valign ui-constraint)
  (let ((numwin (length (ui-constraint-windows ui-constraint))))
    (if (> numwin 2)
	(menuname-as-win-num ui-constraint)
	(let* ((opts (car (ui-constraint-opts ui-constraint)))
	       (name (ui-constraint-class-name (ui-constraint-class ui-constraint)))
	       (q1 (car opts))
	       (q2 (cadr opts))
	       (val (string-append (valign-nonant->string q1) "<->" (valign-nonant->string q2))))
	  (string-append name ": " val)))))

;; define the vert. alignment constraint
(define-public uicc-valign
  (make-ui-constraint-class 
   "Vertical alignment"
   "Vertical alignment.    \n\
Keep windows together using a vertical connecting bar. \
Also can be used to glue left and right edges of windows together." 
   '(2 '+) cnctr-valign 
   (make-ui-cnctr-align "Vertical") draw-cn-valign 
   cl-is-constraint-satisfied? 
   "cn-keep-lefts-even.xpm" "cn-right-left-even.xpm" menuname-valign))


;;----------------------------------------------------------------------
;; Relative size constraints
;;----------------------------------------------------------------------

;; Horizontal Size

;; ui-constructor
(define (ui-cnctr-hsize)
  (two-window-or-more-prompter
   "Relative width"
   "First window?"
   "Second window?"))

;; constructor
(define* (cnctr-hsize winlist #&optional (enable? #f))
  (let* ((width1 (car (window-frame-size (car winlist))))
	 (widthlist (map (lambda (w) (car (window-frame-size w))) (cdr winlist)))
	 (clvlist (map (lambda (w) (make-cl-variable "clvhwdiff" (- width1 w))) widthlist))
	 (cleqlist (map (lambda (w) (cl-minus (window-clv-width (car winlist)) (window-clv-width w))) (cdr winlist)))
	 (cnlist (map (lambda (cleq clv) (make-cl-constraint cleq = clv)) cleqlist clvlist))
	 (sclist (map (lambda (clv) (make-cl-stay-constraint clv cls-required 10)) clvlist)))
    (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist)
    (and enable? (for-each (lambda (sc) (cl-add-constraint (scwm-master-solver) sc)) sclist))
    (list sclist winlist cnlist)))

;; draw-proc
(define (draw-cn-hsize ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (for-each (lambda (n)
		(let* ((pos (window-viewport-position n))
		       (sz  (window-frame-size n))
		       (px  (+ (car pos) (car sz)))
		       (p2y (+ (cadr pos) (quotient (cadr sz) 2))))
		  (xlib-set-line-attributes! width)
		  (xlib-draw-rectangle! (cons px (cadr pos)) 30 (cadr sz))
		  (xlib-draw-line! (cons px p2y) (cons (+ px 10) p2y))
		  (draw-arrow 10 10 (cons (+ px 20 (quotient width 2)) p2y) 'right)))
	      win-list)))

;;    (let* ((w1ctr (translate-point (window-center-middle (car win-list)) width 0))
;;	   (w1lm (window-left-middle (car win-list)))
;;	   (w1rm (window-right-middle (car win-list)))
;;	   (wctrlist (map (lambda (w) (translate-point (window-center-middle w) width 0)) (cdr win-list))) 
;;	   (wlmlist (map (lambda (w) (window-left-middle w)) (cdr win-list)))
;;	   (wrmlist (map (lambda (w) (window-right-middle w)) (cdr win-list))))
;;      (xlib-set-line-attributes! width)
;;      (xlib-draw-line! w1lm w1rm)
;;      (draw-window-line-anchor w1ctr 5)
;;      (for-each (lambda (wctr wrm wlm)
;;		  (xlib-draw-line! wlm wrm)
;;		  (xlib-draw-line! w1ctr wctr)
;;		  (draw-window-line-anchor wctr 5))
;;		wctrlist wrmlist wlmlist))))

;; declare horiz. relative size constraint
(define-public uicc-hsize
  (make-ui-constraint-class
   "Relative width"
   "Relative width.        \n\
Resize the widths of windows together."
   '(2 '+) cnctr-hsize
   ui-cnctr-hsize draw-cn-hsize
   cl-is-constraint-satisfied?
   "cn-relative-hsize.xpm" #f menuname-as-win-num))

;; Vertical Size

;; ui-constructor
(define (ui-cnctr-vsize)
  (two-window-or-more-prompter
   "Relative height" 
   "First window?"
   "Second window?"))

;; constructor
(define* (cnctr-vsize winlist #&optional (enable? #f))
  (let* ((h1 (cadr (window-frame-size (car winlist))))
	 (hlist (map (lambda (w) (cadr (window-frame-size w))) (cdr winlist)))
	 (clvlist (map (lambda (h) (make-cl-variable "clvvwdiff" (- h1 h))) hlist))
	 (cleqlist (map (lambda (w) (cl-minus (window-clv-height (car winlist)) (window-clv-height w))) (cdr winlist)))
	 (cnlist (map (lambda (cleq clv) (make-cl-constraint cleq = clv)) cleqlist clvlist))
	 (sclist (map (lambda (clv) (make-cl-stay-constraint clv cls-required 10)) clvlist)))
    (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist)
    (and enable? (for-each (lambda (sc) (cl-add-constraint (scwm-master-solver) sc)) sclist))
    (list sclist winlist cnlist)))

;; draw-proc
(define (draw-cn-vsize ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (for-each (lambda (n)
		(let* ((pos (window-viewport-position n))
		       (sz  (window-frame-size n))
		       (py  (- (cadr pos) 30))
		       (p2x (+ (car pos) (quotient (car sz) 2))))
		  (xlib-set-line-attributes! width)
		  (xlib-draw-rectangle! (cons (car pos) py) (car sz) 30)
		  (xlib-draw-line! (cons p2x (cadr pos)) (cons p2x (- (cadr pos) 10)))
		  (draw-arrow 10 10 (cons p2x (- (cadr pos) 20 (quotient width 2))) 'top)))
	      win-list)))

;;    (let* ((w1ctr (translate-point (window-center-middle (car win-list)) 0 width))
;;	   (w1ct (window-center-top (car win-list)))
;;	   (w1cb (window-center-bottom (car win-list)))
;;	   (wctrlist (map (lambda (w) (translate-point (window-center-middle w) 0 width)) (cdr win-list)))
;;	   (wctlist (map (lambda (w) (window-center-top w)) (cdr win-list)))
;;	   (wcblist (map (lambda (w) (window-center-bottom w)) (cdr win-list))))
;;     (xlib-set-line-attributes! width)
;;      (xlib-draw-line! w1ct w1cb)
;;      (draw-window-line-anchor w1ctr 5)
;;      (for-each (lambda (w2ctr w2ct w2cb)
;;		  (xlib-draw-line! w2ct w2cb)
;;		  (xlib-draw-line! w1ctr w2ctr)
;;		  (draw-window-line-anchor w2ctr 5))
;;		wctrlist wctlist wcblist))))

;; declare the vert. relative size constraint
(define-public uicc-vsize
  (make-ui-constraint-class
   "Relative height"
   "Relative height        \n\
Resize the heights of windows together." 
   '(2 '+) cnctr-vsize
   ui-cnctr-vsize draw-cn-vsize
   cl-is-constraint-satisfied?
   "cn-relative-vsize.xpm" #f menuname-as-win-num))


;;------------------------------------------------------------------
;; Minimum Window Size
;;------------------------------------------------------------------

;; horizontal

;; ui-constructor
(define (ui-cnctr-minhsize)
  (one-window-prompter "Minimum width"))

;; constructor
(define* (cnctr-minhsize win #&optional (enable? #f))
  (let* ((clv (make-cl-variable "clvminwidth" (car (window-frame-size win))))
	 (cleq (window-clv-width win))
	 (cn (make-cl-constraint cleq >= clv))
	 (sc (make-cl-stay-constraint clv cls-required 10)))
    (cl-add-constraint (scwm-master-solver) cn)
    (and enable? (cl-add-constraint (scwm-master-solver) sc))
    (list (list sc) (list win) clv cn)))

;; draw-proc
(define (draw-cn-minhsize ui-constraint enable focus mode)
  (let* ((win      (car (ui-constraint-windows ui-constraint)))
	 (width    (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width))
	 (halfw    (quotient width 2))
	 (clv-val  (cl-int-value (car (ui-constraint-opts ui-constraint))))
	 (wpos     (window-viewport-position win))
	 (wsz      (window-frame-size win))
	 (offset   (quotient (- clv-val (car wsz)) 2))
	 (loff     (- (car wpos) offset))
	 (roff     (+ offset (car wpos) (car wsz)))
	 (be       (+ (cadr wpos) (cadr wsz)))
	 (ctr      (+ (cadr wpos) (quotient (cadr wsz) 2))))
    (xlib-set-line-attributes! width)
    (xlib-draw-line! (cons loff (cadr wpos)) (cons loff be))
    (xlib-draw-line! (cons roff (cadr wpos)) (cons roff be))
    (xlib-draw-line! (cons (- loff halfw) ctr) (cons (- loff 10) ctr))
    (xlib-draw-line! (cons (+ roff halfw) ctr) (cons (+ roff 10) ctr))
    (draw-arrow 10 10 (cons (- loff 20) ctr) 'left)
    (draw-arrow 10 10 (cons (+ roff 20) ctr) 'right)))

;; declare the min. horiz. size constraint
(define-public uicc-minhsize
  (make-ui-constraint-class
   "Minimum width"
   "Minimum width.         \n\
Do not let window get narrower than it is."
   1 cnctr-minhsize
   ui-cnctr-minhsize draw-cn-minhsize
   cl-is-constraint-satisfied?
   "cn-min-hsize.xpm" #f menuname-as-class-name))

;; vertical

;; ui-constructor
(define (ui-cnctr-minvsize)
  (one-window-prompter "Minimum height"))

;; constructor
(define* (cnctr-minvsize win #&optional (enable? #f))
  (let* ((clv (make-cl-variable "clvminheight" (cadr (window-frame-size win))))
	 (cleq (window-clv-height win))
	 (cn (make-cl-constraint cleq >= clv))
	 (sc (make-cl-stay-constraint clv cls-required 10)))
    (cl-add-constraint (scwm-master-solver) cn)
    (and enable? (cl-add-constraint (scwm-master-solver) sc))
    (list (list sc) (list win) clv cn)))

;; draw-proc
(define (draw-cn-minvsize ui-constraint enable focus mode)
  (let* ((win      (car (ui-constraint-windows ui-constraint)))
	 (width    (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width))
	 (halfw    (quotient width 2))
	 (clv-val  (cl-int-value (car (ui-constraint-opts ui-constraint))))
	 (wpos     (window-viewport-position win))
	 (wsz      (window-frame-size win))
	 (offset   (quotient (- clv-val (cadr wsz)) 2))
	 (toff     (- (cadr wpos) offset))
	 (boff     (+ offset (cadr wpos) (cadr wsz)))
	 (le       (+ (car wpos) (car wsz)))
	 (ctr      (+ (car wpos) (quotient (car wsz) 2))))
    (xlib-set-line-attributes! width)
    (xlib-draw-line! (cons (car wpos) toff) (cons le toff))
    (xlib-draw-line! (cons (car wpos) boff) (cons le boff))
    (xlib-draw-line! (cons ctr (- toff halfw)) (cons ctr (- toff 10)))
    (xlib-draw-line! (cons ctr (+ boff halfw)) (cons ctr (+ boff 10)))
    (draw-arrow 10 10 (cons ctr (- toff 20 halfw)) 'top)
    (draw-arrow 10 10 (cons ctr (+ boff 20 halfw)) 'bottom)))

;; declare the min. vert.  size constraint
(define-public uicc-minvsize
  (make-ui-constraint-class
   "Minimum height"
   "Minimum height.        \n\
Do not let window get shorter than it is." 
   1 cnctr-minvsize
   ui-cnctr-minvsize draw-cn-minvsize
   cl-is-constraint-satisfied?
   "cn-min-vsize.xpm" #f menuname-as-class-name))


;;--------------------------------------------------------------------
;; Maximum Window Size
;;--------------------------------------------------------------------

;; horizontal

;; ui-constructor
(define (ui-cnctr-maxhsize)
  (one-window-prompter "Maximum width"))

;; constructor
(define* (cnctr-maxhsize win #&optional (enable? #f))
  (let* ((clv (make-cl-variable "clvmaxwidth" (car (window-frame-size win))))
	 (cleq (window-clv-width win))
	 (cn (make-cl-constraint cleq <= clv))
	 (sc (make-cl-stay-constraint clv cls-required 10)))
    (cl-add-constraint (scwm-master-solver) cn)
    (and enable? (cl-add-constraint (scwm-master-solver) sc))
    (list (list sc) (list win) clv cn)))

;; draw-proc
(define (draw-cn-maxhsize ui-constraint enable focus mode)
  (let* ((win      (car (ui-constraint-windows ui-constraint)))
	 (width    (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width))
	 (halfw    (quotient width 2))
	 (clv-val  (cl-int-value (car (ui-constraint-opts ui-constraint))))
	 (wpos     (window-viewport-position win))
	 (wsz      (window-frame-size win))
	 (offset   (quotient (- clv-val (car wsz)) 2))
	 (loff     (- (car wpos) offset))
	 (roff     (+ offset (car wpos) (car wsz)))
	 (be       (+ (cadr wpos) (cadr wsz)))
	 (ctr      (+ (cadr wpos) (quotient (cadr wsz) 2))))
    (xlib-set-line-attributes! width)
    (xlib-draw-line! (cons loff (cadr wpos)) (cons loff be))
    (xlib-draw-line! (cons roff (cadr wpos)) (cons roff be))
    (xlib-draw-line! (cons (+ loff 10 width) ctr) (cons (car wpos) ctr))
    (xlib-draw-line! (cons (- roff 10 width) ctr) (cons (+ (car wpos) (car wsz)) ctr))
    (draw-arrow 10 10 (cons (+ loff halfw) ctr) 'left)
    (draw-arrow 10 10 (cons (- roff halfw) ctr) 'right)))

;; declare the max. horiz. size constraint
(define-public uicc-maxhsize
  (make-ui-constraint-class
   "Maximum width"
   "Maximum width.         \n\
Do not let window get wider than it is." 
   1 cnctr-maxhsize
   ui-cnctr-maxhsize draw-cn-maxhsize
   cl-is-constraint-satisfied?
   "cn-max-hsize.xpm" #f menuname-as-class-name))

;; vertical

;; ui-constructor
(define (ui-cnctr-maxvsize)
  (one-window-prompter "Maximum height"))

;; constructor
(define* (cnctr-maxvsize win #&optional (enable? #f))
  (let* ((clv (make-cl-variable "clvmaxheight" (cadr (window-frame-size win))))
	 (cleq (window-clv-height win))
	 (cn (make-cl-constraint cleq <= clv))
	 (sc (make-cl-stay-constraint clv cls-required 10)))
    (cl-add-constraint (scwm-master-solver) cn)
    (and enable? (cl-add-constraint (scwm-master-solver) sc))
    (list (list sc) (list win) clv cn)))

;; draw-proc
(define (draw-cn-maxvsize ui-constraint enable focus mode)
  (let* ((win      (car (ui-constraint-windows ui-constraint)))
	 (width    (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width))
	 (halfw    (quotient width 2))
	 (clv-val  (cl-int-value (car (ui-constraint-opts ui-constraint))))
	 (wpos     (window-viewport-position win))
	 (wsz      (window-frame-size win))
	 (offset   (quotient (- clv-val (cadr wsz)) 2))
	 (toff     (- (cadr wpos) offset))
	 (boff     (+ offset (cadr wpos) (cadr wsz)))
	 (le       (+ (car wpos) (car wsz)))
	 (ctr      (+ (car wpos) (quotient (car wsz) 2))))
    (xlib-set-line-attributes! width)
    (xlib-draw-line! (cons (car wpos) toff) (cons le toff))
    (xlib-draw-line! (cons (car wpos) boff) (cons le boff))
    (xlib-draw-line! (cons ctr (+ toff 10 width)) (cons ctr (cadr wpos)))
    (xlib-draw-line! (cons ctr (- boff 10 width)) (cons ctr (+ (cadr wpos) (cadr wsz))))
    (draw-arrow 10 10 (cons ctr (+ toff halfw)) 'top)
    (draw-arrow 10 10 (cons ctr (- boff halfw)) 'bottom)))

;; declare the max. vert. size constraint
(define-public uicc-maxvsize
  (make-ui-constraint-class
   "Maximum height"
   "Maximum height.        \n\
Do not let window get taller than it is." 
   1 cnctr-maxvsize
   ui-cnctr-maxvsize draw-cn-maxvsize
   cl-is-constraint-satisfied?
   "cn-max-vsize.xpm" #f menuname-as-class-name))


;;-----------------------------------------------------------
;; strict relative position
;;-----------------------------------------------------------

;;  ui-constructor
(define (ui-cnctr-strict-relpos)
  (two-window-or-more-nonant-prompter "Strict relative position"))

;; utilities functions to create some cl-variables
(define (window-clv-cx win) 
  (cl-plus (window-clv-xl win) (cl-divide (window-clv-width win) 2)))

(define (window-clv-my win)
  (cl-plus (window-clv-yt win) (cl-divide (window-clv-height win) 2)))


;; constructor
(define* (cnctr-strict-relpos winlist nonantlist #&optional (enable? #f))
  (let* ((clvxposlist (map get-vcl-from-nonant winlist nonantlist))
	 (clvyposlist (map get-hcl-from-nonant winlist nonantlist))
	 (xposlist (map (lambda (w n) (car (get-vpos-from-nonant w n))) winlist nonantlist))
	 (yposlist (map (lambda (w n) (car (get-hpos-from-nonant w n))) winlist nonantlist))
	 (clvxlist (map (lambda (xp) (make-cl-variable "clv-xdiff" (- (car xposlist) xp))) (cdr xposlist)))
	 (clvylist (map (lambda (yp) (make-cl-variable "clv-ydiff" (- (car yposlist) yp))) (cdr yposlist)))
	 (scxlist (map (lambda (clvx) (make-cl-stay-constraint clvx cls-required 10)) clvxlist))
	 (scylist (map (lambda (clvy) (make-cl-stay-constraint clvy cls-required 10)) clvylist))
	 (sclist (append scxlist scylist))
	 (cleqxlist (map (lambda (clvp) (cl-minus (car clvxposlist) clvp)) (cdr clvxposlist)))
	 (cleqylist (map (lambda (clvp) (cl-minus (car clvyposlist) clvp)) (cdr clvyposlist)))
	 (cnxlist (map (lambda (clv1 cleqx) (make-cl-constraint clv1 = cleqx )) clvxlist cleqxlist))
	 (cnylist (map (lambda (clv2 cleqy) (make-cl-constraint clv2 = cleqy )) clvylist cleqylist))
	 (cnlist (append cnxlist cnylist)))
    (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist)
    (and enable? (for-each (lambda (sc) (cl-add-constraint (scwm-master-solver) sc)) sclist))
    (list sclist winlist nonantlist cnlist)))

;; utility function (for getting positions from nonants)

(define (get-pos-from-nonant win nonant)
  (case nonant
    ((0) (window-left-top win))
    ((1) (window-center-top win))
    ((2) (window-right-top win))
    ((3) (window-left-middle win))
    ((4) (window-center-middle win))
    ((5) (window-right-middle win))
    ((6) (window-left-bottom win))
    ((7) (window-center-bottom win))
    ((8) (window-right-bottom win))
    (else (error "Bad nonant"))))

;; draw-proc
(define (draw-cn-strict-relpos ui-constraint enable focus mode)
  (let* ((win-list (ui-constraint-windows ui-constraint))
	 (width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width))
	 (nonant-list (car (ui-constraint-opts ui-constraint)))
	 (w1pos (get-pos-from-nonant (car win-list) (car nonant-list)))
	 (w1top (translate-point w1pos 0 5))
	 (w1bot (translate-point w1pos 0 -5))
	 (wposlist (map (lambda (w n) (get-pos-from-nonant w n)) (cdr win-list) (cdr nonant-list)))
	 (wtoplist (map (lambda (wpos) (translate-point wpos 0 5)) wposlist))
	 (wbotlist (map (lambda (wpos) (translate-point wpos 0 -5)) wposlist)))
    (xlib-set-line-attributes! width)
    (draw-window-line-anchor w1pos 5)
    (for-each (lambda (wpos wtop wbot)
		(xlib-draw-line! w1pos wpos)
		(xlib-draw-line! w1top wtop)
		(xlib-draw-line! w1bot wbot)
		(draw-window-line-anchor wpos 5))
	      wposlist wtoplist wbotlist)))

;; declare the strict relative position constraint
(define-public uicc-strict-relpos
  (make-ui-constraint-class
   "Strict relative position"
   "Strict relative position. \n\
Move windows around together.  Where you click in the windows \
determines what part of each window is connected to the other." 
   '(2 '+) cnctr-strict-relpos
   ui-cnctr-strict-relpos draw-cn-strict-relpos
   cl-is-constraint-satisfied?
   "cn-strict-relative-pos.xpm" #f menuname-as-win-num))


;;---------------------------------------------------------
;; weak relative position constraints  
;;---------------------------------------------------------

;; keep-above
  
;; ui-constructor
(define (ui-cnctr-keep-above)
  (two-window-or-more-prompter 
   "Vertical separation"
   "Window on top?" "Window below?"))

;; draw-proc
(define (draw-cn-keep-above ui-constraint enable focus mode)
  (let* ((winlist (ui-constraint-windows ui-constraint))
	 (short-winlist (drop-last-element winlist))
	 (width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (for-each (lambda (wt wb)
		(if (not (null? wt))
		    (let* ((halfw  (quotient width 2))
			   (wtpos  (window-viewport-position wt))
			   (wtsz   (window-frame-size wt))
			   (wtctr  (+ (car wtpos) (quotient (car wtsz) 2)))
			   (wbpos  (window-viewport-position wb))
			   (wbsz   (window-frame-size wb))
			   (wbctr  (+ (car wbpos) (quotient (car wbsz) 2)))
			   (midleft (min (car wtpos) (car wbpos)))
			   (midrght (max (+ (car wtpos) (car wtsz)) (+ (car wbpos) (car wbsz))))
			   (botwt   (+ (cadr wtpos) (cadr wtsz)))
			   (ctr    (+ botwt (quotient (- (cadr wbpos) botwt) 2))))
		      (xlib-set-line-attributes! width 'on-off-dash)
		      (xlib-draw-line! (cons midleft ctr) (cons midrght ctr))
		      (xlib-set-line-attributes! width)
		      (xlib-draw-line! (cons wtctr (- ctr halfw)) (cons wtctr (+ botwt 10 halfw)))
		      (xlib-draw-line! (cons wbctr (+ ctr halfw)) (cons wbctr (- (cadr wbpos) 10 halfw)))
		      (draw-arrow 10 10 (cons wtctr botwt) 'top)
		      (draw-arrow 10 10 (cons wbctr (cadr wbpos)) 'bottom))))
	      short-winlist (cdr winlist))))


(define (drop-last-element! items)
  (let ((n (length items)))
    (if (< n 2)
	(error "drop-last-element! cannot shrink a list with fewer than 2 elements"))
    (set-cdr! (list-tail items (- n 2)) ())))
  
(define (drop-last-element list)
  (if (or (null? list) (null? (cdr list))) '() (cons (car list) (drop-last-element (cdr list)))))

;; constructor
(define* (cnctr-keep-above winlist #&optional (enable? #f))
  (let* ((sortedwl (sort-windows-by-middle-pos winlist #:horiz #f))
	 (clvtlist (map (lambda (w) (window-clv-yt w)) (cdr sortedwl)))
	 (clvblist (map (lambda (w) (window-clv-yb w)) sortedwl)))
    (drop-last-element! clvblist)
    (let ((cnlist (map (lambda (ct cb) (make-cl-constraint cb <= ct)) clvtlist clvblist)))
      (and enable? (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist))
      (list cnlist sortedwl))))

;; declare the keep-above constraint
(define-public uicc-ka
  (make-ui-constraint-class 
   "Vertical separation"
   "Vertical separation.   \n\
Keep one window wholly above another."
   2 cnctr-keep-above
   ui-cnctr-keep-above draw-cn-keep-above 
   cl-is-constraint-satisfied? 
   "cn-keep-above.xpm" #f menuname-as-win-num))

;; keep-to-left-of

;; ui-constructor
(define (ui-cnctr-keep-to-left-of)
  (two-window-or-more-prompter 
   "Horizontal separation"
   "Window on left?" "Window on right?"))

;; draw-proc
(define (draw-cn-keep-to-left-of ui-constraint enable focus mode)
  (let* ((winlist (ui-constraint-windows ui-constraint))
	 (short-winlist (drop-last-element winlist))
	 (width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (for-each (lambda (wl wr)
		(if (not (null? wl))
		    (let* ((halfw  (quotient width 2))
			   (wlpos  (window-viewport-position wl))
			   (wlsz   (window-frame-size wl))
			   (wlctr  (+ (cadr wlpos) (quotient (cadr wlsz) 2)))
			   (wrpos  (window-viewport-position wr))
			   (wrsz   (window-frame-size wr))
			   (wrctr  (+ (cadr wrpos) (quotient (cadr wrsz) 2)))
			   (midtop (min (cadr wlpos) (cadr wrpos)))
			   (midbot (max (+ (cadr wlpos) (cadr wlsz)) (+ (cadr wrpos) (cadr wrsz))))
			   (rewl   (+ (car wlpos) (car wlsz)))
			   (ctr    (+ rewl (quotient (- (car wrpos) rewl) 2))))
		      (xlib-set-line-attributes! width 'on-off-dash)
		      (xlib-draw-line! (cons ctr midtop) (cons ctr midbot))
		      (xlib-set-line-attributes! width)
		      (xlib-draw-line! (cons (- ctr halfw) wlctr) (cons (+ rewl 10 halfw) wlctr))
		      (xlib-draw-line! (cons (+ ctr halfw) wrctr) (cons (- (car wrpos) 10 halfw) wrctr))
		      (draw-arrow 10 10 (cons rewl wlctr) 'left)
		      (draw-arrow 10 10 (cons (car wrpos) wrctr) 'right))))
	      short-winlist (cdr winlist))))

;; constructor
(define* (cnctr-keep-to-left-of winlist #&optional (enable? #f))
  (let* ((sortedwl (sort-windows-by-middle-pos winlist #:horiz #t))
	 (clvrlist (map (lambda (w) (window-clv-xr w)) sortedwl))
	 (clvllist (map (lambda (w) (window-clv-xl w)) (cdr sortedwl))))
    (drop-last-element! clvrlist)
    (let ((cnlist (map (lambda (cr cl) (make-cl-constraint cr <= cl)) clvrlist clvllist)))
      (and enable? (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist))
      (list cnlist sortedwl))))

;; declare the keep-to-left-of constraint
(define-public uicc-klo
  (make-ui-constraint-class 
   "Horizontal separation"
   "Horizontal separation. \n\
Keep one window wholly to the left of another."
   2 cnctr-keep-to-left-of
   ui-cnctr-keep-to-left-of draw-cn-keep-to-left-of
   cl-is-constraint-satisfied? 
   "cn-keep-to-left-of.xpm" #f menuname-as-win-num))


;;----------------------------------------------------------------------
;; Relative inverse-size constraints
;;----------------------------------------------------------------------

;; NOTE: Although it is possible, I would not use these constraints with
;; more than two windows.  I don't believe that the behavior (each 
;; window sums-to-dimension with a master window) would be very intuitive.

;; sum-to-width

;; ui-constructor
(define (ui-cnctr-sum-to-width)
  (two-window-or-more-prompter
   "Sum of Widths Constant"
   "First window?"
   "Second window?"))

;; constructor
(define* (cnctr-sum-to-width winlist #&optional (enable? #f))
  (let* ((width1 (car (window-frame-size (car winlist))))
	 (widthlist (map (lambda (w) (car (window-frame-size w))) (cdr winlist)))
	 (clvlist (map (lambda (w) (make-cl-variable "clvwtotal" (+ width1 w))) widthlist))
	 (cleqlist (map (lambda (w) (cl-plus (window-clv-width (car winlist)) (window-clv-width w))) (cdr winlist)))
	 (cnlist (map (lambda (cleq clv) (make-cl-constraint cleq = clv)) cleqlist clvlist))
	 (sclist (map (lambda (clv) (make-cl-stay-constraint clv cls-required 10)) clvlist)))
    (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist)
    (and enable? (for-each (lambda (sc) (cl-add-constraint (scwm-master-solver) sc)) sclist))
    (list sclist winlist cnlist)))

;; draw-proc
(define (draw-cn-sum-to-width ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 2))
	(error "Expected at least two windows in win-list of cn for an relative inverse size constraint"))
    (let* ((w1ctr (translate-point (window-center-middle (car win-list)) width 0))
	   (w1lm (window-left-middle (car win-list)))
	   (w1rm (window-right-middle (car win-list)))
	   (wctrlist (map (lambda (w) (translate-point (window-center-middle w) width 0)) (cdr win-list))) 
	   (wlmlist (map (lambda (w) (window-left-middle w)) (cdr win-list)))
	   (wrmlist (map (lambda (w) (window-right-middle w)) (cdr win-list))))
      (xlib-set-line-attributes! width)
      (xlib-draw-line! w1lm w1rm)
      (draw-window-line-anchor w1ctr 5)
      (for-each (lambda (wctr wrm wlm)
		  (xlib-draw-line! wlm wrm)
		  (xlib-draw-line! w1ctr wctr)
		  (draw-window-line-anchor wctr 5))
		wctrlist wrmlist wlmlist))))

;; declare horiz. relative size constraint
(define-public uicc-sum-to-width
  (make-ui-constraint-class
   "Sum of Widths Constant"
   "Sum of Widths Constant. \n\
The sum of the widths of the windows \
in the constraint remain constant."
   '(2 '+) cnctr-sum-to-width
   ui-cnctr-sum-to-width draw-cn-sum-to-width
   cl-is-constraint-satisfied?
   "cn-window-width-sum.xpm" #f menuname-as-win-num))

;; Vertical Size

;; ui-constructor
(define (ui-cnctr-sum-to-height)
  (two-window-or-more-prompter
   "Sum of Heights Constant" 
   "First window?"
   "Second window?"))

;; constructor
(define* (cnctr-sum-to-height winlist #&optional (enable? #f))
  (let* ((h1 (cadr (window-frame-size (car winlist))))
	 (hlist (map (lambda (w) (cadr (window-frame-size w))) (cdr winlist)))
	 (clvlist (map (lambda (h) (make-cl-variable "clvhtotal" (+ h1 h))) hlist))
	 (cleqlist (map (lambda (w) (cl-plus (window-clv-height (car winlist)) (window-clv-height w))) (cdr winlist)))
	 (cnlist (map (lambda (cleq clv) (make-cl-constraint cleq = clv)) cleqlist clvlist))
	 (sclist (map (lambda (clv) (make-cl-stay-constraint clv cls-required 10)) clvlist)))
    (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist)
    (and enable? (for-each (lambda (sc) (cl-add-constraint (scwm-master-solver) sc)) sclist))
    (list sclist winlist cnlist)))

;; draw-proc
(define (draw-cn-sum-to-height ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (let* ((w1ctr (translate-point (window-center-middle (car win-list)) 0 width))
	   (w1ct (window-center-top (car win-list)))
	   (w1cb (window-center-bottom (car win-list)))
	   (wctrlist (map (lambda (w) (translate-point (window-center-middle w) 0 width)) (cdr win-list)))
	   (wctlist (map (lambda (w) (window-center-top w)) (cdr win-list)))
	   (wcblist (map (lambda (w) (window-center-bottom w)) (cdr win-list))))
      (xlib-set-line-attributes! width)
      (xlib-draw-line! w1ct w1cb)
      (draw-window-line-anchor w1ctr 5)
      (for-each (lambda (w2ctr w2ct w2cb)
		  (xlib-draw-line! w2ct w2cb)
		  (xlib-draw-line! w1ctr w2ctr)
		  (draw-window-line-anchor w2ctr 5))
		wctrlist wctlist wcblist))))

;; declare the vert. relative size constraint
(define-public uicc-sum-to-height
  (make-ui-constraint-class
   "Sum of Heights Constant"
   "Sum of Heights Constant. \n\
The sum of the heights of the windows \
in the constraint remain constant."
   '(2 '+) cnctr-sum-to-height
   ui-cnctr-sum-to-height draw-cn-sum-to-height
   cl-is-constraint-satisfied?
   "cn-window-height-sum.xpm" #f menuname-as-win-num))



;; global-constraint-instance-list
