;; $Id$
;; Copyright (C) 1999 Greg J. Badros

(define-module (app scwm window-locations)
  :use-module (app scwm base))

(define (half n)
  (truncate (/ n 2)))

(define-public (pair-virtual->viewport pt)
  "A version of virtual->viewport that works on the pair points used by this
module."
  (let* ((x (car pt))
	 (y (cdr pt))
	 (newpt (virtual->viewport x y)))
    (cons (car newpt) (cadr newpt))))

(define-public (window-center-top win)
  "Return a pair (X . Y) that is the pixel position of the center, top of WIN
relative to current viewport."
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size)))
    (cons (+ xl (half w)) yt)))

(define-public (window-center-bottom win)
  "Return a pair (X . Y) that is the pixel position of the center, bottom of WIN
relative to the current viewport."
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size))
	 (h (cadr size)))
    (cons (+ xl (half w)) (+ yt h))))

(define-public (window-left-middle win)
  "Return a pair (X . Y) that is the pixel position of the left, middle of WIN
relative to the current viewport."
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (h (cadr size)))
    (cons xl (+ yt (half h)))))

(define-public (window-right-middle win)
  "Return a pair (X . Y) that is the pixel position of the right, middle of WIN
relative to the current viewport."
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size))
	 (h (cadr size)))
    (cons (+ xl w) (+ yt (half h)))))

(define-public (window-left-top win)
  "Return a pair (X . Y) that is the pixel position of the left, top of WIN
relative to the current viewport."
  (window-viewport-position win))

(define-public (window-left-bottom win)
  "Return a pair (X . Y) that is the pixel position of the left, bottom of WIN
relative to the current viewport."
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (h (cadr size)))
    (cons xl (+ yt h))))

(define-public (window-right-top win)
  "Return a pair (X . Y) that is the pixel position of the right, top of WIN
relative to the current viewport."
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size)))
    (cons (+ xl w) yt)))

(define-public (window-right-bottom win)
  "Return a pair (X . Y) that is the pixel position of the right, bottom of WIN
relative to the current viewport."
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size))
	 (h (cadr size)))
    (cons (+ xl w) (+ yt h))))

(define-public (get-window-quadrant select-list)
  "Argument SELECT-LIST is a list of (WIN X Y) possibly returned from 
select-viewport-position.  Return a list (WIN QUADRANT) which gives the WIN argument 
and the quadrant of WIN in which the point (X, Y) resides.  X and Y are assumed to 
be viewport relative."
  (let* ((pos (window-viewport-position (car select-list)))
         (size (window-frame-size (car select-list)))
	 (dx (- (cadr select-list) (car pos)))
	 (dy (- (caddr select-list) (cadr pos)))
	 (qx (quotient dx (quotient (car size) 3)))
	 (qy (quotient dy (quotient (cadr size) 3))))
    (+ (* 3 qy) qx)))

;; (get-window-quadrant (select-viewport-position))


