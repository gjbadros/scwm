;;; $Id$
;;; dir-focus.scm -- moves the focus to the closest window in a given direction
;;; By Greg J. Badros
;;; Largely copied from GWM version by Teemu Hirsimaki <thirsima@cc.hut.fi>
;;; 
;;; NOT YET DONE CONVERTING TO SCWM! --07/03/99 gjb
;;;
;;; The function is "(focus-dir DIR)" where DIR is 'north, 'east,
;;; 'south or 'west.  It searches the closest window in the given
;;; direction and sets focus in it.  The variable dir-skip-list
;;; specifies regular expressions for windows to ignore.

(define-module (app scwm dir-focus)
  :use-module (app scwm base)
  :use-module (app scwm winlist)
  :use-module (app scwm wininfo)
  :use-module (app scwm optargs))

(define-public dirlist-skip-proc winlist-skip?)

;; Score function
(define-public (dir-score-function distance offset)
  (if (< distance 1)
      0
      (+ (/ (* 1000 (abs offset)) distance) distance)))

;; Calculates a score for a window.  The smaller the better.
(define-public (dir-calculate-score win dir)
  (let* ((pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (window-x (car pos))
	 (window-y (cadr pos))
	 (window-width (car size))
	 (window-height (cadr size)))
    (let ((win-x (+ window-x (/ window-width 2)))
	  (win-y (+ window-y (/ window-height 2)))
	  (cur-x (car (pointer-position)))
	  (cur-y (cadr (pointer-position))))
      (cond
       ((eq? dir 'north)
	(dir-score-function (- cur-y win-y) (- cur-x win-x)))
       ((eq? dir 'south)
	(dir-score-function (- win-y cur-y) (- cur-x win-x)))
       ((eq? dir 'east)
	(dir-score-function (- win-x cur-x) (- cur-y win-y)))
       ((eq? dir 'west)
	(dir-score-function (- cur-x win-x) (- cur-y win-y)))))))

;; Moves focus to the closest window in the given direction
(define*-public (dir-focus dir #&optional (win (current-window-with-pointer)))
  (let ((cur win)
	(best-score 0)
	(best-win #f))
    (for-each (lambda (w)
		(if (not (eq? w win))
		    (let ((score (dir-calculate-score w dir)))
		      (and (> score 0)
			   (or (< score best-score)
			       (not best-win))
			   (set! best-score score)
			   (set! best-win w)))))
	      (list-windows #:only visible? #:except dirlist-skip-proc))
    (if (and best-win
	     (not (eq? best-win win)))
	(begin
	  (raise-window best-win)
	  (warp-to-window best-win)))))

;;; end of file