;; $Id$
;; (C) 1999 Greg J. Badros

(define-module (app scwm window-locations))

(define (half n)
  (truncate (/ n 2)))

(define-public (window-center-top win)
  "Return a pair (X . Y) that is the pixel position of the center, top of WIN."
  (let* ((pos (window-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size)))
    (cons (+ xl (half w)) yt)))

(define-public (window-center-bottom win)
  "Return a pair (X . Y) that is the pixel position of the center, bottom of WIN."
  (let* ((pos (window-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size))
	 (h (cadr size)))
    (cons (+ xl (half w)) (+ yt h))))

(define-public (window-left-middle win)
  "Return a pair (X . Y) that is the pixel position of the left, middle of WIN."
  (let* ((pos (window-position win))
	 (size (window-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (h (cadr size)))
    (cons xl (+ yt (half h)))))

(define-public (window-right-middle win)
  "Return a pair (X . Y) that is the pixel position of the right, middle of WIN."
  (let* ((pos (window-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size))
	 (h (cadr size)))
    (cons (+ xl w) (+ yt (half h)))))

(define-public (window-left-top win)
  "Return a pair (X . Y) that is the pixel position of the left, top of WIN."
  (window-position win))

(define-public (window-left-bottom win)
  "Return a pair (X . Y) that is the pixel position of the left, bottom of WIN."
  (let* ((pos (window-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (h (cadr size)))
    (cons xl (+ yt h))))

(define-public (window-right-top win)
  "Return a pair (X . Y) that is the pixel position of the right, top of WIN."
  (let* ((pos (window-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size)))
    (cons (+ xl w) yt)))

(define-public (window-right-bottom win)
  "Return a pair (X . Y) that is the pixel position of the right, bottom of WIN."
  (let* ((pos (window-position win))
	 (size (window-frame-size win))
	 (xl (car pos))
	 (yt (cadr pos))
	 (w (car size))
	 (h (cadr size)))
    (cons (+ xl w) (+ yt h))))
