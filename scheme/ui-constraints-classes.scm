;; $Id$
;; Copyright (C) 1999 Greg J. Badros and Jeff Nichols

(define-module (app scwm ui-constraints-classes)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm simple-constraints)
  :use-module (app scwm message-window)
  :use-module (app scwm window-locations)
  :use-module (app scwm xlib-drawing))

;; (use-modules (app scwm ui-constraints-classes))
;; (use-modules (app scwm xlib-drawing))


;; (load "/home/gjb/scwm/scheme/ui-constraints-classes.scm")
;; (set-current-module the-root-module)
(reset-ui-constraint-classes!)

;;; GJB:FIXME:: we'd like to be able to use color, mode
;;; but cannot w/o using an overlay plance;  Also,
;;; the draw functions should get passed semantic parameters
;;; e.g., is-enabled? and is-focussed? instead of color, width

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

;; returns a generic constructor for use with simple-constraints
;; simple-constraints return a cn value.. This must be
;; converted to a list of a list of cn's
(define (get-simple-2win-cnstr proc)
  (lambda (w1 w2) (list (list (proc w1 w2)) (list w1 w2))))

;; generic menuname-proc's
(define (menuname-as-class-name ui-constraint)
  (ui-constraint-class-name (ui-constraint-class ui-constraint)))

;; helpful draw utilities
(define (draw-window-line-anchor point radius)
  "Draw the circles that anchor a line to a window."
  (let ((diameter (* 2 radius)))
    (xlib-draw-arc! (cons (- (car point) radius) (- (cdr point) radius))
		    diameter diameter 0 360)))

(define (two-window-prompter name p1 p2)
  (let ((w1 (select-window-interactively (string-append name ": " p1) msgwin))
	(w2 (select-window-interactively (string-append name ": " p2) msgwin)))
    (if (or (equal? w1 #f) (equal? w2 #f)) #f
	(list w1 w2))))

(define (ui-cnctr-keep-lefts-even)
  (two-window-prompter "keep-lefts-even" "First window?" "Second window?"))

(define (draw-cn-keep-lefts-even ui-constraint enable focus mode)
  (let ((cn (car (ui-constraint-cn ui-constraint)))
	(win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 2))
	(error "Expected only two windows in win-list of cn for draw-cn-keep-lefts-even"))
    (let* ((w1 (car win-list))
	   (w2 (cadr win-list))
	   (w1pos (window-left-middle w1))
	   (w2pos (window-left-middle w2)))
      (xlib-set-line-width! width)
      ;; GJB:FIXME:: we'd like to be able to use color, mode
      ;; but cannot w/o using an overlay plance
      (xlib-draw-line! w1pos w2pos)
      (draw-window-line-anchor w1pos 5)
      (draw-window-line-anchor w2pos 5))))

(define-public uicc-kle
  (make-ui-constraint-class 
   "keep-lefts-even" 2 (get-simple-2win-cnstr keep-lefts-even)
   ui-cnctr-keep-lefts-even draw-cn-keep-lefts-even 
   cl-is-constraint-satisfied? 
   "cn-keep-lefts-even.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-above)
  (two-window-prompter "keep-above" "Window on top?" "Window below?"))

(define (draw-cn-keep-above ui-constraint enable focus mode)
  (let ((cn (car (ui-constraint-cn ui-constraint)))
	(win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 2))
	(error "Expected only two windows in win-list of cn for draw-cn-keep-lefts-even"))
    (let* ((w1 (car win-list))
	   (w2 (cadr win-list))
	   (w1pos (window-center-bottom w1))
	   (w2pos (window-center-top w2)))
      (xlib-set-line-width! width)
      ;; GJB:FIXME:: we'd like to be able to use color, mode
      ;; but cannot w/o using an overlay plance
      (xlib-draw-line! w1pos w2pos)
      (draw-window-line-anchor w1pos 5)
      (draw-window-line-anchor w2pos 5))))

(define-public uicc-ka
  (make-ui-constraint-class 
   "keep-above" 2 (get-simple-2win-cnstr keep-above)
   ui-cnctr-keep-above draw-cn-keep-above 
   cl-is-constraint-satisfied? 
   "cn-keep-above.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-to-left-of)
  (two-window-prompter "keep-to-left-of" "Window on left?" "Window on right?"))

(define (draw-cn-keep-to-left-of ui-constraint enable focus mode)
  (let ((cn (car (ui-constraint-cn ui-constraint)))
	(win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 2))
	(error "Expected only two windows in win-list of cn for draw-cn-keep-lefts-even"))
    (let* ((w1 (car win-list))
	   (w2 (cadr win-list))
	   (w1pos (window-right-middle w1))
	   (w2pos (window-left-middle w2)))
      (xlib-set-line-width! width)
      (xlib-draw-line! w1pos w2pos)
      (draw-window-line-anchor w1pos 5)
      (draw-window-line-anchor w2pos 5))))

(define-public uicc-klo
  (make-ui-constraint-class 
   "keep-to-left-of" 2 (get-simple-2win-cnstr keep-to-left-of)
   ui-cnctr-keep-to-left-of draw-cn-keep-to-left-of
   cl-is-constraint-satisfied? 
   "cn-keep-to-left-of.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-rights-even)
  (two-window-prompter "keep-rights-even" "First window?" "Second window?"))

(define (draw-cn-keep-rights-even ui-constraint enable focus mode)
  (let ((cn (car (ui-constraint-cn ui-constraint)))
	(win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 2))
	(error "Expected only two windows in win-list of cn for draw-cn-keep-rights-even"))
    (let* ((w1 (car win-list))
	   (w2 (cadr win-list))
	   (w1pos (window-right-middle w1))
	   (w2pos (window-right-middle w2)))
      (xlib-set-line-width! width)
      ;; GJB:FIXME:: we'd like to be able to use color, mode
      ;; but cannot w/o using an overlay plance
      (xlib-draw-line! w1pos w2pos)
      (draw-window-line-anchor w1pos 5)
      (draw-window-line-anchor w2pos 5))))

(define-public uicc-kre
  (make-ui-constraint-class 
   "keep-rights-even" 2 (get-simple-2win-cnstr keep-rights-even) 
   ui-cnctr-keep-rights-even draw-cn-keep-rights-even 
   cl-is-constraint-satisfied? 
   "cn-keep-rights-even.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-tops-even)
  (two-window-prompter "keep-tops-even" "First window?" "Second window?"))

(define (draw-cn-keep-tops-even ui-constraint enable focus mode)
  (let ((cn (car (ui-constraint-cn ui-constraint)))
	(win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 2))
	(error "Expected only two windows in win-list of cn for draw-cn-keep-tops-even"))
    (let* ((w1 (car win-list))
	   (w2 (cadr win-list))
	   (w1pos (window-center-top w1))
	   (w2pos (window-center-top w2)))
      (xlib-set-line-width! width)
      ;; GJB:FIXME:: we'd like to be able to use color, mode
      ;; but cannot w/o using an overlay plance
      (xlib-draw-line! w1pos w2pos)
      (draw-window-line-anchor w1pos 5)
      (draw-window-line-anchor w2pos 5))))

(define-public uicc-kte
  (make-ui-constraint-class 
   "keep-tops-even" 2 (get-simple-2win-cnstr keep-tops-even)
   ui-cnctr-keep-tops-even draw-cn-keep-tops-even 
   cl-is-constraint-satisfied? 
   "cn-keep-tops-even.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-bottoms-even)
  (two-window-prompter "keep-bottoms-even" "First window?" "Second window?"))

(define (draw-cn-keep-bottoms-even ui-constraint enable focus mode)
  (let ((cn (car (ui-constraint-cn ui-constraint)))
	(win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 2))
	(error "Expected only two windows in win-list of cn for draw-cn-keep-bottoms-even"))
    (let* ((w1 (car win-list))
	   (w2 (cadr win-list))
	   (w1pos (window-center-bottom w1))
	   (w2pos (window-center-bottom w2)))
      (xlib-set-line-width! width)
      ;; GJB:FIXME:: we'd like to be able to use color, mode
      ;; but cannot w/o using an overlay plance
      (xlib-draw-line! w1pos w2pos)
      (draw-window-line-anchor w1pos 5)
      (draw-window-line-anchor w2pos 5))))

(define-public uicc-kbe
  (make-ui-constraint-class 
   "keep-bottoms-even" 2 (get-simple-2win-cnstr keep-bottoms-even)
   ui-cnctr-keep-bottoms-even draw-cn-keep-bottoms-even 
   cl-is-constraint-satisfied? 
   "cn-keep-bottoms-even.xpm" menuname-as-class-name))


(define (one-window-prompter name p1)
  (let ((w1 (select-window-interactively (string-append name ": " p1) msgwin)))
    (if (equal? w1 #f) #f
	(list w1))))

;; Perhaps should move this elsewhere
;; TRANSLATE POINT

(define (translate-point pt dx dy)
  (let* ((x (car pt))
	 (y (cdr pt)))
    (cons (+ x dx) (+ y dy))))

;; ------

;; translates a quadrant vector into a string for display
(define (quadvec->string vec)
  (let*  ((str  
	   (string-append 
	    (if (eq? (vector-ref vec 0) 1) "N" "")
	    (if (eq? (vector-ref vec 3) 1) "S" "")
	    (if (eq? (vector-ref vec 1) 1) "W" "")
	    (if (eq? (vector-ref vec 2) 1) "E" ""))))
    str))

;; the anchor constraint stores the clv in the first element of OPTS and 
;; constraint quadrant in the second element
(define (menuname-anchor ui-constraint)
  (let* ((opts (ui-constraint-opts ui-constraint))
	 (name (ui-constraint-class-name (ui-constraint-class ui-constraint)))
	 (quad  (car opts))
	 (val (quadvec->string quad)))
    (string-append name ": " val)))


(define (draw-vertical-anchor-symbol pt size)
  (let* ((upleft (translate-point pt (- size) (- size)))
	 (upright (translate-point pt size (- size)))
	 (downleft (translate-point pt (- size) size))
	 (downright (translate-point pt size size)))
    (xlib-draw-line! upleft downright)
    (xlib-draw-line! downright downleft)
    (xlib-draw-line! downleft upright)
    (xlib-draw-line! upright upleft)))

(define (draw-horizontal-anchor-symbol pt size)
  (let* ((upleft (translate-point pt (- size) (- size)))
	 (upright (translate-point pt size (- size)))
	 (downleft (translate-point pt (- size) size))
	 (downright (translate-point pt size size)))
    (xlib-draw-line! upleft downright)
    (xlib-draw-line! downright upright)
    (xlib-draw-line! upright downleft)
    (xlib-draw-line! downleft upleft)))


(define-public (quadrant->vector quad)
  (let ((vec (make-vector 4 0)))
    (case quad
      ((0 1 2 4) (vector-set! vec 0 1))) ;; constrain top
    (case quad
      ((0 3 4 6) (vector-set! vec 1 1))) ;; constrain left
    (case quad
      ((2 4 5 8) (vector-set! vec 2 1))) ;; constrain right
    (case quad
      ((4 6 7 8) (vector-set! vec 3 1))) ;; constrain bottom
    vec))


(define (ui-cnctr-anchor)
  (let ((msgwin ui-constraint-prompter-msgwin))
    (message-window-set-message! msgwin "Select window to anchor")
    (message-window-show! msgwin)
    (let* ((winlist (select-viewport-position))
	   (win (car winlist))
	   (quad (get-window-quadrant winlist)))
      (message-window-hide! msgwin)
      (list win (quadrant->vector quad)))))

      

(define* (cnctr-anchor w1 quad #&optional (enable? #f))
  (let* ((top (cdr (window-center-top w1)))
	 (bot (cdr (window-center-bottom w1)))
	 (rgt (car (window-right-middle w1)))
	 (lft (car (window-left-middle w1)))
	 (cn-list ())
	 (sc-list ())
	 (clv-list ()))

    (if (eq? (vector-ref quad 0) 1)
	(let* ((w1-yt (window-clv-yt w1))
	       (clv (make-cl-variable "clvt" top))
	       (cn (make-cl-constraint w1-yt = clv))
	       (sc (make-cl-stay-constraint clv cls-required 10)))
	  (cl-add-constraint (scwm-master-solver) cn)
	  (and enable? (cl-add-constraint (scwm-master-solver) sc))
	  (set! cn-list (cons cn cn-list))
	  (set! sc-list (cons sc sc-list))
	  (set! clv-list (cons clv clv-list))))

    (if (eq? (vector-ref quad 1) 1)
	(let* ((w1-xl (window-clv-xl w1))
	       (clv (make-cl-variable "clvl" lft))
	       (cn (make-cl-constraint w1-xl = clv))
	       (sc (make-cl-stay-constraint clv cls-required 10)))
	  (cl-add-constraint (scwm-master-solver) cn)
	  (and enable? (cl-add-constraint (scwm-master-solver) sc))
	  (set! cn-list (cons cn cn-list))
	  (set! sc-list (cons sc sc-list))
	  (set! clv-list (cons clv clv-list))))

    (if (eq? (vector-ref quad 2) 1)
	(let* ((w1-xr (window-clv-xr w1))
	       (clv (make-cl-variable "clvr" rgt))
	       (cn (make-cl-constraint w1-xr = clv))
	       (sc (make-cl-stay-constraint clv cls-required 10)))
	  (cl-add-constraint (scwm-master-solver) cn)
	  (and enable? (cl-add-constraint (scwm-master-solver) sc))
	  (set! cn-list (cons cn cn-list))
	  (set! sc-list (cons sc sc-list))
	  (set! clv-list (cons clv clv-list))))

    (if (eq? (vector-ref quad 3) 1)
	(let* ((w1-yb (window-clv-yb w1))
	       (clv (make-cl-variable "clvb" bot))
	       (cn (make-cl-constraint w1-yb = clv))
	       (sc (make-cl-stay-constraint clv cls-required 10)))
	  (cl-add-constraint (scwm-master-solver) cn)
	  (and enable? (cl-add-constraint (scwm-master-solver) sc))
	  (set! cn-list (cons cn cn-list))
	  (set! sc-list (cons sc sc-list))
	  (set! clv-list (cons clv clv-list))))

    (list sc-list (list w1) quad cn-list clv-list)))

;;    (list (keep-top-at-v w1 clv enable?) clv sc)))


(define (draw-cn-anchor ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(opts (ui-constraint-opts ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 1))
	(error "Expected only one window in win-list of cn for anchor"))
    (if (eq? opts #f)
	(error "Expected some optional information for an anchor."))
    (let* ((w (car win-list))
	   (wt (window-center-top w))
	   (wb (window-center-bottom w))
	   (wl (window-left-middle w))
	   (wr (window-right-middle w))
	   (quad (car opts)))
      (if (not (vector? quad))
	  (error "Quadrant value in opts list should be a vector."))
      (xlib-set-line-width! width)
      (if (eq? (vector-ref quad 0) 1)
	  (draw-vertical-anchor-symbol wt 10))
      (if (eq? (vector-ref quad 1) 1)
	  (draw-horizontal-anchor-symbol wl 10))
      (if (eq? (vector-ref quad 2) 1)
	  (draw-horizontal-anchor-symbol wr 10))
      (if (eq? (vector-ref quad 3) 1)
	  (draw-vertical-anchor-symbol wb 10)))))


(define-public uicc-anchor
  (make-ui-constraint-class 
   "anchor" 1 cnctr-anchor 
   ui-cnctr-anchor draw-cn-anchor 
   cl-is-constraint-satisfied? 
   "anchor.xpm" menuname-anchor))

;; global-constraint-instance-list
