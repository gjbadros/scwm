;; $Id$
;; (C) 1999 Greg J. Badros and Jeff Nichols

(define-module (app scwm ui-constraints-classes)
  :use-module (app scwm base)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm simple-constraints)
  :use-module (app scwm window-locations)
  :use-module (app scwm xlib-drawing))

;; (load "/scratch/gjb/scwm/scheme/ui-constraints-classes.scm")
;; (set-current-module the-root-module)


(xlib-set-line-width! 5)

(define (ui-cnctr-keep-lefts-even)
  (let ((w1 (select-window-interactively "keep-lefts-even: First window?"))
	(w2 (select-window-interactively "keep-lefts-even: Second window?")))
    (list w1 w2)))

(define (draw-cn-keep-lefts-even ui-constraint color width mode)
  (let ((cn (ui-constraint-cn ui-constraint))
	(win-list (ui-constraint-windows ui-constraint)))
    (if (not (= (length win-list) 2))
	(error "Expected only two windows in win-list of cn for draw-cn-keep-lefts-even"))
    (let* ((w1 (car win-list))
	   (w2 (cadr win-list))
	   (w1pos (window-position w1))
	   (w2pos (window-position w2))
	   (w1x (car w1pos))
	   (w1y (cadr w1pos))
	   (w2x (car w2pos))
	   (w2y (cadr w2pos)))
      (xlib-draw-line! w1x w1y w2x w2y)
      (xlib-draw-arc! (- w1x 5) (- w1y 5) 10 10 0 360)
      (xlib-draw-arc! (- w2x 5) (- w2y 5) 10 10 0 360)
      )))

(define-public uicc-kle (make-ui-constraint-class "keep-lefts-even" 2 keep-lefts-even 
						  ui-cnctr-keep-lefts-even 
						  draw-cn-keep-lefts-even 
						  cl-is-constraint-satisfied?))

;; global-constraint-instance-list
