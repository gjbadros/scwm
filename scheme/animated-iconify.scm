;; $Id$
;; Copyright (C) 1999 Greg J. Badros

(define-module (app scwm animated-iconify)
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

(define (animate-iconify-or-deiconify icon-pos win-pos icon-size win-size 
                                      ms-delay before-animation-proc 
                                      after-animation-proc pct-sizes)
  (let* ((delta-pos (map - win-pos icon-pos))
         (delta-size (map - win-size icon-size)))
    (set-X-server-synchronize! #t)
    (xlib-set-line-attributes! 6 'solid)
    (and before-animation-proc (before-animation-proc))
    (with-grabbed-server
     (lambda ()
       (for-each (lambda (pct)
		   (let* ((new-wsize (map + icon-size
                                          (map round-to-pct delta-size 
                                               (list pct pct))))
			  (new-wpos 
                           (point-list->point-pair
                            (map + icon-pos
                                 (map round-to-pct delta-pos 
                                      (list pct pct))))))
		     (apply xlib-draw-rectangle! new-wpos new-wsize)
		     (sleep-ms ms-delay)
		     (apply xlib-draw-rectangle! new-wpos new-wsize)))
		 pct-sizes)))
    (and after-animation-proc (after-animation-proc))
    (set-X-server-synchronize! #f)))

(define*-public (animated-iconify #&optional (win (get-window)))
  "Iconify WIN using a simple animation of a shrinking rectangle.
The rectangle moves towards the icon's location, if there is one."
  (cond
   ((not (iconified? win))
    (iconify win) ;; ensure a useful icon position the first time
    (set-window-property! win 'last-viewport-position (window-viewport-position win))
    (animate-iconify-or-deiconify (icon-viewport-position win) 
                                  (window-viewport-position win) 
                                  (icon-size win)
                                  (window-frame-size win)
                                  20 (lambda () (iconify win)) #f
                                  '(1.0 .9 .8 .7 .6 .5 .4 .3 .2 .1 0.0)))))


(define*-public (animated-deiconify #&optional (win (get-window)))
  "Deiconify WIN using a simple animation of a growing rectangle.
The rectangle grows outwards from the icon, if there is one."
  (if (iconified? win)
      (animate-iconify-or-deiconify  (icon-viewport-position win)
                                     (window-viewport-position win)
                                     (icon-size win)
                                     (window-frame-size win)
                                     20 #f (lambda () (deiconify win))
                                     '(0.0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1.0))))

(define*-public (animated-deiconify-to-last-viewport-position #&optional (win (get-window)))
  "Deiconify WIN with an animation to the same viewport position as it was iconified from."
  (if (iconified? win)
      (let ((pos (or (window-property win 'last-viewport-position)
		     (apply virtual->viewport
			    (window-position-in-viewport
			     (current-viewport-offset-xx)
			     (current-viewport-offset-yy)
			     win)))))
	(animate-iconify-or-deiconify (icon-viewport-position win)
				      pos
				      (icon-size win)
				      (window-frame-size win)
				      20 #f (lambda () (apply deiconify 
							      (cons win (apply viewport->virtual pos))))
						    '(0.0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1.0)))))


(define*-public (animated-deiconify-to-current-viewport #&optional (win (get-window)))
  "Deicionify WIN with an animation to the current viewport."
  (if (iconified? win)
    (animate-iconify-or-deiconify (icon-viewport-position win)
                                  (apply virtual->viewport 
                                         (window-position-in-viewport 
                                          (current-viewport-offset-xx)
                                          (current-viewport-offset-yy)
                                          win))
                                  (icon-size win)
                                  (window-frame-size win)
                                  20 #f (lambda () (deiconify-to-current-viewport win))
                                  '(0.0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1.0))))

(define*-public (animated-toggle-iconify #&optional (win (get-window)))
  "Iconify WIN if not iconified, or de-iconify WIN if it is iconified.
Uses animation, in either case."
  (if win
      (if (iconified? win)
          (animated-deiconify win)
          (animated-iconify win))))

;; (define w (select-window-interactively))
;; (begin (animate-iconify w) (sleep 1) (animate-deiconify w))

;;(window-frame-size (select-window-interactively))
;; (window-property w 'last-viewport-position)

;; (animated-iconify w)
;; (animated-deiconify w)
;; (animated-deiconify-to-current-viewport w)
;; (animated-deiconify-to-last-viewport-position w)
