;;; $Id$
;;; rectangle.scm
;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm rectangle)
  :use-module (app scwm optargs))

;;; window-corners, enclosing-rectangle, tile-windows-interactively
;;; By Greg J. Badros --07/04/99 gjb
;;; Inspired by code suggested by Todd Larason -- 17-Apr-1999
;; (window-corners (get-window))
(define-public (window-corners win)
  "Return the four coordinates of the corners of the location of WIN.
Return value's car is the top left, cdr is the bottom right.
That is, the returned list is: ((nw-x nw-y) se-x se-y).  Use
`rect-nw-x', `rect-nw-y', `rect-se-x', `rect-se-y' to take apart
the returned list."
  (let ((p (window-position win))
	(s (window-frame-size win)))
    (set-cdr! (cdr s) ())
    (cons p (map + p s))))

(define-public rect-nw car)
(define-public rect-se cdr)
(define-public rect-nw-x caar)
(define-public rect-nw-y cadar)
(define-public rect-se-x cadr)
(define-public rect-se-y caddr)

;; (enclosing-rectangle l)
(define-public (enclosing-rectangle wins)
  "Return the smallest rectangle that encloses the windows WINS.
Return value's car is the top left of the rectangle, cdr is
the bottom right.
That is, the returned list is: ((nw-x nw-y) se-x se-y)."
  (let ((window-corners (map window-corners wins)))
    (let ((x1 (apply min (map rect-nw-x window-corners)))
	  (y1 (apply min (map rect-nw-y window-corners)))
	  (x2 (apply max (map rect-se-x window-corners)))
	  (y2 (apply max (map rect-se-y window-corners))))
      (cons (list x1 y1) (list x2 y2)))))

