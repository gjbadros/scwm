;; $Id$
;; (C) 1999 Greg J. Badros and Jeff Nichols

(use-modules (app scwm ui-constraints)
	     (app scwm xlib-drawing))

;; (load "/scratch/gjb/scwm/scheme/ui-constraints.scm")

(define (ui-cnctr-keep-lefts-even)
  (let ((w1 (select-window-interactively "W1?"))
	(w2 (select-window-interactively "W2?")))
    (list w1 w2)))

(xlib-set-line-width! 5)

(define (draw-cn-keep-lefts-even ui-constraint color width mode)
  (xlib-draw-rectangle! 0 0 20 20))

;; (define (cl-is-constraint-satisfied) #t)

(define uicc-kle (make-ui-constraint-class "keep-lefts-even" 2 keep-lefts-even 
					   ui-cnctr-keep-lefts-even 
					   draw-cn-keep-lefts-even 
					   cl-is-constraint-satisfied?))

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

;; (set-current-module the-root-module)

(make-ui-constraint uicc-kle (list (select-window-interactively) (select-window-interactively)))

(make-ui-constraint-interactively uicc-kle)

;; (length global-constraint-instance-list)
;; (define w (select-window-interactively))
;; (window-in-list-in-focus? (list w))
;; (ui-constraint-cn (car global-constraint-instance-list))
;; (ui-constraint-windows (car global-constraint-instance-list))
;; (constrained-window-in-focus? (car global-constraint-instance-list))

(draw-constraint (car global-constraint-instance-list))

(undraw-constraint (car global-constraint-instance-list))

(ui-constraint? (car global-constraint-instance-list))

;; (ui-constraint? 4)

(ui-constraint-class (car global-constraint-instance-list))

(draw-all-constraints)

(enable-ui-constraint (car global-constraint-instance-list))
(disable-ui-constraint (car global-constraint-instance-list))

;; (define UI-CONSTRAINT (car global-constraint-instance-list))
;; (set-enable! UI-CONSTRAINT #f)
