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
  :use-module (app scwm xlib-drawing))

;; (use-scwm-modules ui-constraints-classes)
;; (use-scwm-modules ui-constraints)
;; (use-scwm-modules window-locations)
;; (use-scwm-modules window-selection)
;; (use-scwm-modules xlib-drawing)

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
	  (list winlist))
	(let ((win (select-window-interactively 
		    (string-append name ": " p1) msgwin)))
	  (if win (list win) #f)))))


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
   (if (eqv? (vector-ref vec 0) 1) "N" "")
   (if (eqv? (vector-ref vec 3) 1) "S" "")
   (if (eqv? (vector-ref vec 1) 1) "W" "")
   (if (eqv? (vector-ref vec 2) 1) "E" "")))

;; the anchor constraint stores the clv in the first element of OPTS and 
;; constraint nonant in the second element
(define (menuname-anchor ui-constraint)
  (let* ((opts (ui-constraint-opts ui-constraint))
	 (name (ui-constraint-class-name (ui-constraint-class ui-constraint)))
	 (nonant  (car opts))
	 (val (dirvector->string nonant)))
    (string-append name ": " val)))

;; drawing anchor symbols
(define (draw-vertical-anchor-symbol pt size)
  (let ((upleft (translate-point pt (- size) (- size)))
	(upright (translate-point pt size (- size)))
	(downleft (translate-point pt (- size) size))
	(downright (translate-point pt size size)))
    (xlib-draw-line! upleft downright)
    (xlib-draw-line! downright downleft)
    (xlib-draw-line! downleft upright)
    (xlib-draw-line! upright upleft)))

(define (draw-horizontal-anchor-symbol pt size)
  (let ((upleft (translate-point pt (- size) (- size)))
	(upright (translate-point pt size (- size)))
	(downleft (translate-point pt (- size) size))
	(downright (translate-point pt size size)))
    (xlib-draw-line! upleft downright)
    (xlib-draw-line! downright upright)
    (xlib-draw-line! upright downleft)
    (xlib-draw-line! downleft upleft)))


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
	  (list (car winlist) (nonant->dirvector (object-property (car winlist) 'nonant)))) ;; give the default nonant as the middle
	(begin
	  (message-window-set-message! msgwin "Window nonant to anchor?")
	  (with-message-window-shown 
	   msgwin
	   (let* ((winlist (select-viewport-position))
		  (win (car winlist))
		  (nonant (get-window-nonant winlist)))
	     (list win (nonant->dirvector nonant))))))))

;; (nonant->dirvector (get-window-nonant (select-viewport-position)))
;; (cnctr-anchor (select-window-interactively) #(#t #f #f #f) #t)
;; constructor
(define* (cnctr-anchor w1 nonant #&optional (enable? #f))
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
      (xlib-set-line-width! width)
      (if (vector-ref nonant 0)
	  (draw-vertical-anchor-symbol wt 10))
      (if (vector-ref nonant 1)
	  (draw-horizontal-anchor-symbol wl 10))
      (if (vector-ref nonant 2)
	  (draw-horizontal-anchor-symbol wr 10))
      (if (vector-ref nonant 3)
	  (draw-vertical-anchor-symbol wb 10)))))

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
   "anchor.xpm" menuname-anchor))

;;-----------------------------------------------------------------------
;; alignment constraint
;;
;; forces the alignment of some windows based upon
;; where the user clicks in the window
;;-----------------------------------------------------------------------

;; horizontal alignment

;; ui-constructor
(define (ui-cnctr-align)
  (let ((winlist (selected-windows-list)))
    (if (>= (length winlist) 2)
	(begin
	  (unselect-all-windows)
	  (list winlist (map (lambda (w) (object-property w 'nonant)) winlist)))
	(let ((win1 #f)
	      (win2 #f)
	      (slope #f)
	      (nonant1 #f)
	      (nonant2 #f))
	  (message-window-set-message! msgwin "First window nonant to align?")
	  (with-message-window-shown
	   msgwin
	   (set! winlist (select-viewport-position))
	   (set! win1 (car winlist))
	   (set! nonant1 (get-window-nonant winlist))
	   (message-window-set-message! msgwin "Second window nonant to align?")
	   (set! winlist (select-viewport-position))
	   (set! win2 (car winlist))
	   (set! nonant2 (get-window-nonant winlist))
	   (list (list win1 win2) (list nonant1 nonant2)))))))

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
      (xlib-set-line-width! width)
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
    ((0 1 2) "Top")
    ((3 4 5) "Middle")
    ((6 7 8) "Bottom")))

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
   "Horizontal alignment.
Keep windows together using a horizontal connecting bar. \
Also can be used to glue top and bottom edges of windows together."
   '(2 '+) cnctr-halign 
   ui-cnctr-align draw-cn-halign 
   cl-is-constraint-satisfied? 
   "cn-keep-tops-even.xpm" menuname-halign))


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
      (xlib-set-line-width! width)
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
    ((0 3 6) "Top")
    ((1 4 7) "Center")
    ((2 5 8) "Bottom")))

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
   "Vertical alignment.
Keep windows together using a vertical connecting bar. \
Also can be used to glue left and right edges of windows together." 
   '(2 '+) cnctr-valign 
   ui-cnctr-align draw-cn-valign 
   cl-is-constraint-satisfied? 
   "cn-keep-lefts-even.xpm" menuname-valign))


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
    (if (not (= (length win-list) 2))
	(error "Expected two windows in win-list of cn for an relative size constraint"))
    (let* ((w1ctr (translate-point (window-center-middle (car win-list)) width 0))
	   (w1lm (window-left-middle (car win-list)))
	   (w1rm (window-right-middle (car win-list)))
	   (wctrlist (map (lambda (w) (translate-point (window-center-middle w) width 0)) (cdr win-list))) 
	   (wlmlist (map (lambda (w) (window-left-middle w)) (cdr win-list)))
	   (wrmlist (map (lambda (w) (window-right-middle w)) (cdr win-list))))
      (xlib-set-line-width! width)
      (xlib-draw-line! w1lm w1rm)
      (draw-window-line-anchor w1ctr 5)
      (for-each (lambda (wctr wrm wlm)
		  (xlib-draw-line! wlm wrm)
		  (xlib-draw-line! w1ctr wctr)
		  (draw-window-line-anchor wctr 5))
		wctrlist wrmlist wlmlist))))

;; declare horiz. relative size constraint
(define-public uicc-hsize
  (make-ui-constraint-class
   "Relative width"
   "Relative width.
Resize the widths of windows together."
   '(2 '+) cnctr-hsize
   ui-cnctr-hsize draw-cn-hsize
   cl-is-constraint-satisfied?
   "cn-relative-hsize.xpm" menuname-as-win-num))

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
    (let* ((w1ctr (translate-point (window-center-middle (car win-list)) 0 width))
	   (w1ct (window-center-top (car win-list)))
	   (w1cb (window-center-bottom (car win-list)))
	   (wctrlist (map (lambda (w) (translate-point (window-center-middle w) 0 width)) (cdr win-list)))
	   (wctlist (map (lambda (w) (window-center-top w)) (cdr win-list)))
	   (wcblist (map (lambda (w) (window-center-bottom w)) (cdr win-list))))
      (xlib-set-line-width! width)
      (xlib-draw-line! w1ct w1cb)
      (draw-window-line-anchor w1ctr 5)
      (for-each (lambda (w2ctr w2ct w2cb)
		  (xlib-draw-line! w2ct w2cb)
		  (xlib-draw-line! w1ctr w2ctr)
		  (draw-window-line-anchor w2ctr 5))
		wctrlist wctlist wcblist))))

;; declare the vert. relative size constraint
(define-public uicc-vsize
  (make-ui-constraint-class
   "Relative height"
   "Relative height
Resize the heights of windows together." 
   '(2 '+) cnctr-vsize
   ui-cnctr-vsize draw-cn-vsize
   cl-is-constraint-satisfied?
   "cn-relative-vsize.xpm" menuname-as-win-num))


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
    (list (list sc) (list win) cn)))

;; draw-proc
;; JWN:TODO: Find a good representation for this constraint
(define (draw-cn-minhsize ui-constraint enable focus mode)
  (+ 1 1))

;; declare the min. horiz. size constraint
(define-public uicc-minhsize
  (make-ui-constraint-class
   "Minimum width"
   "Minimum width.
Do not let window get narrower than it is."
   1 cnctr-minhsize
   ui-cnctr-minhsize draw-cn-minhsize
   cl-is-constraint-satisfied?
   "cn-min-hsize.xpm" menuname-as-class-name))

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
    (list (list sc) (list win) cn)))

;; draw-proc
;; JWN:TODO: Find a good representation for this constraint
(define (draw-cn-minvsize ui-constraint enable focus mode)
  (+ 1 1))

;; declare the min. vert.  size constraint
(define-public uicc-minvsize
  (make-ui-constraint-class
   "Minimum height"
   "Minimum height.
Do not let window get shorter than it is." 
   1 cnctr-minvsize
   ui-cnctr-minvsize draw-cn-minvsize
   cl-is-constraint-satisfied?
   "cn-min-vsize.xpm" menuname-as-class-name))


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
    (list (list sc) (list win) cn)))

;; draw-proc
;; JWN:TODO: Find a good representation for this constraint
(define (draw-cn-maxhsize ui-constraint enable focus mode)
  (+ 1 1))

;; declare the max. horiz. size constraint
(define-public uicc-maxhsize
  (make-ui-constraint-class
   "Maximum width"
   "Maximum width.
Do not let window get wider than it is." 
   1 cnctr-maxhsize
   ui-cnctr-maxhsize draw-cn-maxhsize
   cl-is-constraint-satisfied?
   "cn-max-hsize.xpm" menuname-as-class-name))

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
    (list (list sc) (list win) cn)))

;; draw-proc
;; JWN:TODO: Find a good representation for this
(define (draw-cn-maxvsize ui-constraint enable focus mode)
  (+ 1 1))

;; declare the max. vert. size constraint
(define-public uicc-maxvsize
  (make-ui-constraint-class
   "Maximum height"
   "Maximum height.
Do not let window get taller than it is." 
   1 cnctr-maxvsize
   ui-cnctr-maxvsize draw-cn-maxvsize
   cl-is-constraint-satisfied?
   "cn-max-vsize.xpm" menuname-as-class-name))


;;-----------------------------------------------------------
;; strict relative position
;;-----------------------------------------------------------

;;  ui-constructor
(define (ui-cnctr-strict-relpos)
  (two-window-or-more-prompter 
   "Strict relative position" 
   "First window?" 
   "Second window?"))

;; utilities functions to create some cl-variables
(define (window-clv-cx win) 
  (cl-plus (window-clv-xl win) (cl-divide (window-clv-width win) 2)))

(define (window-clv-my win)
  (cl-plus (window-clv-yt win) (cl-divide (window-clv-height win) 2)))

;; constructor
(define* (cnctr-strict-relpos winlist #&optional (enable? #f))
  (let* ((pos1 (window-viewport-position (car winlist)))
	 (poslist (map (lambda (w) (window-viewport-position w)) (cdr winlist)))
	 (clv1list (map (lambda (p) (make-cl-variable "clv-xdiff" (- (car pos1) (car p)))) poslist))
	 (clv2list (map (lambda (p) (make-cl-variable "clv-ydiff" (- (cadr pos1) (cadr p)))) poslist))
	 (sc1list (map (lambda (clv1) (make-cl-stay-constraint clv1 cls-required 10)) clv1list))
	 (sc2list (map (lambda (clv2) (make-cl-stay-constraint clv2 cls-required 10)) clv2list))
	 (sclist (append sc1list sc2list))
	 (cleqxlist (map (lambda (w) (cl-minus (window-clv-cx (car winlist)) (window-clv-cx w))) (cdr winlist)))
	 (cleqylist (map (lambda (w) (cl-minus (window-clv-my (car winlist)) (window-clv-my w))) (cdr winlist)))
	 (cn1list (map (lambda (clv1 cleqx) (make-cl-constraint clv1 = cleqx )) clv1list cleqxlist))
	 (cn2list (map (lambda (clv2 cleqy) (make-cl-constraint clv2 = cleqy )) clv2list cleqylist))
	 (cnlist (append cn1list cn2list)))
    (for-each (lambda (cn) (cl-add-constraint (scwm-master-solver) cn)) cnlist)
    (and enable? (for-each (lambda (sc) (cl-add-constraint (scwm-master-solver) sc)) sclist))
    (list sclist winlist cnlist)))

;; draw-proc
(define (draw-cn-strict-relpos ui-constraint enable focus mode)
  (let* ((win-list (ui-constraint-windows ui-constraint))
	 (width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width))
	 (w1ctr (window-center-middle (car win-list)))
	 (w1top (translate-point w1ctr 0 5))
	 (w1bot (translate-point w1ctr 0 -5))
	 (wctrlist (map (lambda (w) (window-center-middle w)) (cdr win-list)))
	 (wtoplist (map (lambda (wctr) (translate-point wctr 0 5)) wctrlist))
	 (wbotlist (map (lambda (wctr) (translate-point wctr 0 -5)) wctrlist)))
    (xlib-set-line-width! width)
    (draw-window-line-anchor w1ctr 5)
    (for-each (lambda (wctr wtop wbot)
		(xlib-draw-line! w1ctr wctr)
		(xlib-draw-line! w1top wtop)
		(xlib-draw-line! w1bot wbot)
		(draw-window-line-anchor wctr 5))
	      wctrlist wtoplist wbotlist)))

;; declare the strict relative position constraint
(define-public uicc-strict-relpos
  (make-ui-constraint-class
   "Strict relative position"
   "Strict relative position.
Move windows around together." 
   '(2 '+) cnctr-strict-relpos
   ui-cnctr-strict-relpos draw-cn-strict-relpos
   cl-is-constraint-satisfied?
   "cn-strict-relative-pos.xpm" menuname-as-win-num))


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
  (let ((winlist (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (let* ((wblist (map (lambda (w) (window-center-bottom w)) winlist))
	   (wtlist (map (lambda (w) (window-center-top w)) (cdr winlist))))
      (drop-last-element! wblist)
      (xlib-set-line-width! width)
      (for-each (lambda (wt wb) 
		  (xlib-draw-line! wb wt)
		  (draw-window-line-anchor wb 5)
		  (draw-window-line-anchor wt 5))
		wtlist wblist))))


(define (drop-last-element! items)
  (let ((n (length items)))
    (if (< n 2)
	(error "drop-last-element! cannot shrink a list with fewer than 2 elements"))
    (set-cdr! (list-tail items (- n 2)) ())))
  
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
   "Vertical separation.
Keep one window wholly above another."
   2 cnctr-keep-above
   ui-cnctr-keep-above draw-cn-keep-above 
   cl-is-constraint-satisfied? 
   "cn-keep-above.xpm" menuname-as-win-num))

;; keep-to-left-of

;; ui-constructor
(define (ui-cnctr-keep-to-left-of)
  (two-window-or-more-prompter 
   "Horizontal separation"
   "Window on left?" "Window on right?"))

;; draw-proc
(define (draw-cn-keep-to-left-of ui-constraint enable focus mode)
  (let ((winlist (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (let* ((wrlist (map (lambda (w) (window-right-middle w)) winlist))
	   (wllist (map (lambda (w) (window-left-middle w)) (cdr winlist))))
      (drop-last-element! wrlist)
      (xlib-set-line-width! width)
      (for-each (lambda (wl wr) 
		  (xlib-draw-line! wr wl)
		  (draw-window-line-anchor wr 5)
		  (draw-window-line-anchor wl 5))
		wllist wrlist))))

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
   "Horizontal separation.
Keep one window wholly to the left of another."
   2 cnctr-keep-to-left-of
   ui-cnctr-keep-to-left-of draw-cn-keep-to-left-of
   cl-is-constraint-satisfied? 
   "cn-keep-to-left-of.xpm" menuname-as-win-num))


;; global-constraint-instance-list
