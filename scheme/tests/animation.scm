;; $Id$

(use-scwm-modules c-animation base)

(begin
  (define w1 (select-window-interactively))
  (define w2 (select-window-interactively))
  (define w3 (select-window-interactively)))
  

(animate-windows
 `(
   (,w1 #f 
      (488 . 359)
      (488 . 359)
      (0 . 0)
      (100 . 100)
      (#f . #f))
   (,w2 #f 
      (488 . 359)
      (488 . 359)
      (100 . 100)
      (200 . 500)
      (#f . #f))
   (,w3 #f 
      (488 . 359)
      (400 . 300)
      (500 . 200)
      (600 . 100)
      (#f . #f))
   ))


(define (wiggle-window)
  (let ((w (get-window)))
    (window-shade w #t)
    (un-window-shade w #t)))

(use-scwm-modules c-animation base constraints)

(start-constraints)

(define (animate-scwm-resolve solver)
  (let ((xforms (cl-resolve-xforms 50)))
    (if (not (null? xforms))
	(animate-windows xforms #f #f))))

(define-public (start-animating-scwm-resolves)
  (add-hook! scwm-resolve-hook animate-scwm-resolve))
(define-public (stop-animating-scwm-resolves)
  (remove-hook! scwm-resolve-hook))

