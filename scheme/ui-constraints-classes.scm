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

;; alias for internal use
(define msgwin ui-constraint-prompter-msgwin)

(define (draw-window-line-anchor point radius)
  "Draw the circles that anchor a line to a window."
  (let ((diameter (* 2 radius)))
    (xlib-draw-arc! (cons (- (car point) radius) (- (cdr point) radius))
		    diameter diameter 0 360)))

(define (two-window-prompter name p1 p2)
  (let ((w1 (select-window-interactively (string-append name ": " p1) msgwin))
	(w2 (select-window-interactively (string-append name ": " p2) msgwin)))
    (list w1 w2)))

(define (ui-cnctr-keep-lefts-even)
  (two-window-prompter "keep-lefts-even" "First window?" "Second window?"))

(define (draw-cn-keep-lefts-even ui-constraint color width mode)
  (let ((cn (ui-constraint-cn ui-constraint))
	(win-list (ui-constraint-windows ui-constraint)))
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
   "cn-keep-lefts-even.xpm"))


(define (ui-cnctr-keep-above)
  (two-window-prompter "keep-above" "Window on top?" "Window below?"))

(define (draw-cn-keep-above ui-constraint color width mode)
  (let ((cn (ui-constraint-cn ui-constraint))
	(win-list (ui-constraint-windows ui-constraint)))
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
   "cn-keep-above.xpm"))


(define (ui-cnctr-keep-to-left-of)
  (two-window-prompter "keep-to-left-of" "Window on left?" "Window on right?"))

(define (draw-cn-keep-to-left-of ui-constraint color width mode)
  (let ((cn (ui-constraint-cn ui-constraint))
	(win-list (ui-constraint-windows ui-constraint)))
    (if (not (= (length win-list) 2))
	(error "Expected only two windows in win-list of cn for draw-cn-keep-lefts-even"))
    (let* ((w1 (car win-list))
	   (w2 (cadr win-list))
	   (w1pos (window-left-middle w1))
	   (w2pos (window-right-middle w2)))
      (xlib-set-line-width! width)
      (xlib-draw-line! w1pos w2pos)
      (draw-window-line-anchor w1pos 5)
      (draw-window-line-anchor w2pos 5))))

(define-public uicc-klo
  (make-ui-constraint-class 
   "keep-to-left-of" 2 keep-to-left-of
   ui-cnctr-keep-to-left-of draw-cn-keep-to-left-of
   cl-is-constraint-satisfied? 
   "cn-keep-to-left-of.xpm"))

;; global-constraint-instance-list
