;; $Id$

(use-modules (app scwm ui-constraints)
	     (app scwm xlib-drawing))

(define (cnctr-keep-lefts-even win-list)
  (apply keep-lefts-even win-list))

(define (ui-cnctr-keep-lefts-even)
  (let ((w1 (select-window-interactively "W1?"))
	(w2 (select-window-interactively "W2?")))
    (cnctr-keep-lefts-even w1 w2)))

(xlib-set-line-width! 5)

(define (draw-cn-keep-lefts-even ui-constraint)
  (xlib-draw-rectangle! 0 0 20 20))

;; (define (cl-is-constraint-satisfied) #t)

(define uicc-kle (make-ui-constraint-class "keep-lefts-even" 2 cnctr-keep-lefts-even 
					   ui-cnctr-keep-lefts-even draw-cn-keep-lefts-even cl-is-constraint-satisfied))

;; global-constraint-class-list
;; (delete-ui-constraint-class! uicc-kle)

;; (ui-constraint-class? uicc-kle)

;; (ui-constraint-class-name uicc-kle)

;; (ui-constraint-class-num-windows uicc-kle)

;; (ui-constraint-class-ctr uicc-kle)

;; (ui-constraint-class-ui-ctr uicc-kle)

;; (ui-constraint-class-draw-proc uicc-kle)
;; (ui-constraint-class-draw-proc 4)

;; (ui-constraint-class-satisfied-proc uicc-kle)





