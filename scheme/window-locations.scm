;;; $Id$
;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm window-locations)
  :use-module (app scwm base))

(define (half n)
  (scwm-round/ n 2))

(define-public (pair-virtual->viewport pt)
  "Convert a pair point from virtual to viewport coordinates.
Return value is (viewport-x . viewport-y)."
  (let* ((x (car pt))
	 (y (cdr pt))
	 (newpt (virtual->viewport x y)))
    (cons (car newpt) (cadr newpt))))

(define-public (window-center-middle win)
  "Return pair (viewport-x . viewport-y) that is the middle of WIN."
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size))
	 (h (cadr size)))
    (cons (+ xl (half w)) (+ yt (half h)))))

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
  (let ((pos (window-viewport-position win)))
    (cons (car pos) (cadr pos))))

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

;; (window-viewport-position-of 'center (get-window))
(define-public (window-viewport-position-of sym win)
  "Return a list (X Y) that is the viewport position of the SYM part of WIN.
SYM is one of northwest north northeast west center east southwest south southeast."
  (let ((pos
	 ((case sym
	    ((north) window-center-top)
	    ((northeast) window-right-top)
	    ((east) window-right-middle)
	    ((southeast) window-right-bottom)
	    ((south) window-center-bottom)
	    ((southwest) window-left-bottom)
	    ((west) window-left-middle)
	    ((northwest) window-left-top)
	    ((center) window-center-middle)
	    (else (error "SYM must be one of northwest north northeast west center east southwest south southeast")))
	  win)))
    (list (car pos) (cdr pos))))
  
