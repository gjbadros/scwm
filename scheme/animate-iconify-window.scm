;; $Id$
;; (C) 1999 Greg J. Badros

(define-module (app scwm animate-iconify-window)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm virtual)
  :use-module (app scwm xlib-drawing))

;; (use-modules (app scwm optargs))
;; (use-modules (app scwm xlib-drawing))

(define (point-list->point-pair l)
  (cons (car l) (cadr l)))

(define (round-to-pct n pct)
  (round (* n pct)))

(define (move-point-pair pair dx dy)
  (let ((x (car pair))
	(y (cdr pair)))
    (cons (+ x dx) (+ y dy))))

(define (half n)
  (round (/ n 2)))

(define (sleep-ms ms)
  (select '() '() '() 0 (* 1000 ms)))

(define (animate-iconify-or-deiconify win-pos win-size ms-delay 
				      before-animation-proc after-animation-proc pct-sizes)
  (let* ((wsize win-size)
	 (wpos (point-list->point-pair win-pos))
	 (w (car wsize))
	 (h (cadr wsize)))
    (set-X-server-synchronize! #t)
    (xlib-set-line-width! 4)
    (and before-animation-proc (before-animation-proc))
    (with-grabbed-server
     (lambda ()
       (for-each (lambda (pct)
		   (let* ((nw (round-to-pct w pct))
			  (nh (round-to-pct h pct))
			  (new-wpos (move-point-pair wpos (half (- w nw)) (half (- h nh)))))
		     (xlib-draw-rectangle! new-wpos nw nh)
		     (sleep-ms ms-delay)
		     (xlib-draw-rectangle! new-wpos nw nh)))
		 pct-sizes)))
    (and after-animation-proc (after-animation-proc))
    (set-X-server-synchronize! #f)))

(define*-public (animate-iconify #&optional (win (get-window)))
   (animate-iconify-or-deiconify (window-viewport-position win) (window-frame-size win)
				 20 (lambda () (iconify win)) #f
				 '(1.0 .9 .8 .7 .6 .5 .4 .3 .2 .1)))


(define*-public (animate-deiconify #&optional (win (get-window)))
   (animate-iconify-or-deiconify (window-viewport-position win) (window-frame-size win)
				 20 #f (lambda () (deiconify win))
				 '(.1 .2 .3 .4 .5 .6 .7 .8 .9 1.0)))

(define*-public (animate-deiconify-to-current-viewport #&optional (win (get-window)))
   (animate-iconify-or-deiconify (apply virtual->viewport 
					(window-position-in-viewport 
					 (current-viewport-offset-xx)
					 (current-viewport-offset-yy)
					 win))
				 (window-frame-size win)
				 20 #f (lambda () (deiconify-to-current-viewport win))
				 '(.1 .2 .3 .4 .5 .6 .7 .8 .9 1.0)))

;; (define w (select-window-interactively))
;; (begin (animate-iconify w) (sleep 1) (animate-deiconify w))

;;(window-frame-size (select-window-interactively))
