;; $Id$
;; (C) 1999 Greg J. Badros and Jeff Nichols

(define-module (app scwm ui-constraints-classes)
  :use-module (app scwm base)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm simple-constraints)
  :use-module (app scwm message-window)
  :use-module (app scwm window-locations)
  :use-module (app scwm xlib-drawing))

;; (use-modules (app scwm ui-constraints-classes))


;; (load "/scratch/gjb/scwm/scheme/ui-constraints-classes.scm")
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


;; public drawing variables

(define-public ui-constraint-in-focus-width 4)
(define-public ui-constraint-out-focus-width 2)


;; alias for internal use
(define msgwin ui-constraint-prompter-msgwin)

;; generic menuname-proc's
(define (menuname-as-class-name ui-constraint)
  (ui-constraint-class-name (ui-constraint-class ui-constraint)))

;; all keep-at-value constraints store the clv in the first element of OPTS
(define (menuname-keep-at-value ui-constraint)
  (let* ((opts (ui-constraint-opts ui-constraint))
	 (name (ui-constraint-class-name (ui-constraint-class ui-constraint)))
	 (val  (cl-value (car opts))))
    (string-append name ": " (number->string val))))

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
  (let ((cn (ui-constraint-cn ui-constraint))
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
   "keep-lefts-even" 2 keep-lefts-even 
   ui-cnctr-keep-lefts-even draw-cn-keep-lefts-even 
   cl-is-constraint-satisfied? 
   "cn-keep-lefts-even.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-above)
  (two-window-prompter "keep-above" "Window on top?" "Window below?"))

(define (draw-cn-keep-above ui-constraint enable focus mode)
  (let ((cn (ui-constraint-cn ui-constraint))
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
   "keep-above" 2 keep-above 
   ui-cnctr-keep-above draw-cn-keep-above 
   cl-is-constraint-satisfied? 
   "cn-keep-above.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-to-left-of)
  (two-window-prompter "keep-to-left-of" "Window on left?" "Window on right?"))

(define (draw-cn-keep-to-left-of ui-constraint enable focus mode)
  (let ((cn (ui-constraint-cn ui-constraint))
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
   "keep-to-left-of" 2 keep-to-left-of
   ui-cnctr-keep-to-left-of draw-cn-keep-to-left-of
   cl-is-constraint-satisfied? 
   "cn-keep-to-left-of.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-rights-even)
  (two-window-prompter "keep-rights-even" "First window?" "Second window?"))

(define (draw-cn-keep-rights-even ui-constraint enable focus mode)
  (let ((cn (ui-constraint-cn ui-constraint))
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
   "keep-rights-even" 2 keep-rights-even 
   ui-cnctr-keep-rights-even draw-cn-keep-rights-even 
   cl-is-constraint-satisfied? 
   "cn-keep-rights-even.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-tops-even)
  (two-window-prompter "keep-tops-even" "First window?" "Second window?"))

(define (draw-cn-keep-tops-even ui-constraint enable focus mode)
  (let ((cn (ui-constraint-cn ui-constraint))
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
   "keep-tops-even" 2 keep-tops-even 
   ui-cnctr-keep-tops-even draw-cn-keep-tops-even 
   cl-is-constraint-satisfied? 
   "cn-keep-tops-even.xpm" menuname-as-class-name))


(define (ui-cnctr-keep-bottoms-even)
  (two-window-prompter "keep-bottoms-even" "First window?" "Second window?"))

(define (draw-cn-keep-bottoms-even ui-constraint enable focus mode)
  (let ((cn (ui-constraint-cn ui-constraint))
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
   "keep-bottoms-even" 2 keep-bottoms-even 
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


(define (ui-cnctr-keep-top-at-value)
  (one-window-prompter "keep-top-at-value" "Window to anchor?"))

(define (cnctr-keep-top-at-value w1)
  (let* ((top (cadr (window-position w1)))
	 (clv (make-cl-variable "clv" top))
	 (sc  (make-cl-stay-constraint clv cls-required 10)))
    (cl-add-constraint (scwm-master-solver) sc)
    (list (keep-top-at-v w1 clv) clv sc)))

(define (draw-cn-keep-top-at-value ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 1))
	(error "Expected only one window in win-list of cn for draw-cn-keep-top-at-value"))
    (let* ((w (car win-list))
	   (wcen (window-center-top w)))
      (xlib-set-line-width! width)
      (draw-vertical-anchor-symbol wcen 10))))

(define-public uicc-ktc
  (make-ui-constraint-class 
   "keep-top-at-value" 1 cnctr-keep-top-at-value 
   ui-cnctr-keep-top-at-value draw-cn-keep-top-at-value 
   cl-is-constraint-satisfied? 
   "cn-keep-top-at-value.xpm" menuname-keep-at-value))


(define (ui-cnctr-keep-left-at-value)
  (one-window-prompter "keep-left-at-value" "Window to anchor?"))

(define (cnctr-keep-left-at-value w1)
  (let* ((left (car (window-position w1)))
	 (clv (make-cl-variable "clv" left))
	 (sc  (make-cl-stay-constraint clv cls-required 10)))
    (cl-add-constraint (scwm-master-solver) sc)
    (list (keep-left-at-v w1 clv) clv sc)))

(define (draw-cn-keep-left-at-value ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 1))
	(error "Expected only one window in win-list of cn for draw-cn-keep-left-at-value"))
    (let* ((w (car win-list))
	   (wcen (window-left-middle w)))
      (xlib-set-line-width! width)
      (draw-horizontal-anchor-symbol wcen 10))))

(define-public uicc-klc
  (make-ui-constraint-class 
   "keep-left-at-value" 1 cnctr-keep-left-at-value 
   ui-cnctr-keep-left-at-value draw-cn-keep-left-at-value 
   cl-is-constraint-satisfied? 
   "cn-keep-left-at-value.xpm" menuname-keep-at-value))


(define (ui-cnctr-keep-right-at-value)
  (one-window-prompter "keep-right-at-value" "Window to anchor?"))

(define (cnctr-keep-right-at-value w1)
  (let* ((right (+ (car (window-position w1)) (car (window-size w1))))
	 (clv (make-cl-variable "clv" right))
	 (sc  (make-cl-stay-constraint clv cls-required 10)))
    (cl-add-constraint (scwm-master-solver) sc)
    (list (keep-right-at-v w1 clv) clv sc)))

(define (draw-cn-keep-right-at-value ui-constraint enable focus mode)
  (let ((win-list (ui-constraint-windows ui-constraint))
	(width (if focus ui-constraint-in-focus-width ui-constraint-out-focus-width)))
    (if (not (= (length win-list) 1))
	(error "Expected only one window in win-list of cn for draw-cn-keep-right-at-value"))
    (let* ((w (car win-list))
	   (wcen (window-right-middle w)))
      (xlib-set-line-width! width)
      (draw-horizontal-anchor-symbol wcen 10))))

(define-public uicc-krc
  (make-ui-constraint-class 
   "keep-right-at-value" 1 cnctr-keep-right-at-value 
   ui-cnctr-keep-right-at-value draw-cn-keep-right-at-value 
   cl-is-constraint-satisfied? 
   "cn-keep-right-at-value.xpm" menuname-keep-at-value))


;; global-constraint-instance-list
