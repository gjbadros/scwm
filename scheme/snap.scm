;;;; $Id$
;;;; Copyright (C) 1999 Danius Michaelides
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

(define-module (app scwm snap)
  :use-module (app scwm base)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm optargs)
  )

;;; If #t then snap to edges that are on the same side
;;;   of both windows, otherwise only snap to edges on
;;;   the outside of both windows (i.e. only allow snaps
;;;   in which the windows don't overlap).
(define-public snap-matches-inside-edges #t)

(define current-windows '())
(define current-windows-info '())
(define trail '())
(define myinfo '())

(define snap-width 10)

(define last-x -1)
(define last-y -1)

(define (wininfo win)
  "Returns list (north-y east-x south-y west-x) for WIN."
  (let ((pos (window-viewport-position win))
        (size (window-frame-size win)))
    (list (cadr pos) (+ (car pos) (car size))
          (+ (cadr pos) (cadr size)) (car pos))))

(define wininfo-n car)
(define wininfo-e cadr)
(define wininfo-s caddr)
(define wininfo-w cadddr)

(define wininfo-x wininfo-w)
(define wininfo-y wininfo-n)
(define (wininfo-width wi) (- (wininfo-e wi)(wininfo-w wi)))
(define (wininfo-height wi) (- (wininfo-s wi)(wininfo-n wi)))

;;; Determines whether the interval (a1,a2) overlaps (b1,b2).
(define (overlap-lines a1 a2 b1 b2)
  (if (< b1 a1)
      (overlap-lines b1 b2 a1 a2)
      (< b1 a2)))

;;; Determines minimum distance from a1 or a2 to b1 or b2.
;;; a1<=a2 and b1<=b2
;;; Returns (dist . edge-indicator)
;;; edge-indicator values:
;;;  val  a  b
;;;   0 = 1, 1
;;;   1 = 1, 2
;;;   2 = 2, 1
;;;   3 = 2, 2
(define (distance-lines a1 a2 b1 b2)
  (if (< b1 a1)
      (let* ((dist-ind (distance-lines b1 b2 a1 a2))
             (ind (cdr dist-ind)))
        (if (and (> ind 0) (< ind 3))
            (cons (car dist-ind) (- 3 ind))
            dist-ind))
      ;; a1<=a2 and a1<=b1<=b2
      (let ((dists (list
                    (abs (- a1 b1))
                    (abs (- a2 b1))
                    (abs (- a2 b2))))
            (idxs (list 0 2 3))
            (min-val 10000)
            (min-idx -1))
        (for-each (lambda (dist idx)
                    (if (< dist min-val)
                        (begin (set! min-val dist)
                               (set! min-idx idx))))
                  dists idxs)
        (cons min-val min-idx))))

(define (dist-horizontal a-win b-win)
  (if (not (overlap-lines (wininfo-n a-win) (wininfo-s a-win)
			  (wininfo-n b-win) (wininfo-s b-win)))
      (cons -1 -1)
      (distance-lines (wininfo-w a-win) (wininfo-e a-win)
                      (wininfo-w b-win) (wininfo-e b-win))))
					;
					;  (if (> (wininfo-x a-win) (wininfo-x b-win))
					;    (dist-horizontal b-win a-win)
					;    )

(define (dist-vertical a-win b-win)
  (if (not (overlap-lines (wininfo-w a-win) (wininfo-e a-win)
			  (wininfo-w b-win) (wininfo-e b-win)))
      (cons -1 -1)
      (distance-lines (wininfo-n a-win) (wininfo-s a-win)
                      (wininfo-n b-win) (wininfo-s b-win))))
					;
					;  (if (> (wininfo-y a-win) (wininfo-y b-win))
					;    (dist-vertical b-win a-win)
					;    )

(define (closest info-l dist-l)
  (define (closest-iter info-l dist-l result len-ind)
    (if (null? info-l)
	(if (< (car len-ind) snap-width)
	    (cons result len-ind)
	    '())
	(if (and (not (eq? (caar dist-l) -1))
                 (or snap-matches-inside-edges
                     (and (not (= (cdar dist-l) 0))
                          (not (= (cdar dist-l) 3))))
		 (< (caar dist-l) (car len-ind)))
	    (closest-iter (cdr info-l) (cdr dist-l) (car info-l) (car dist-l))
	    (closest-iter (cdr info-l) (cdr dist-l) result len-ind))))
  (closest-iter info-l dist-l '() (cons 32000 -1)))

;;;; Interactive move start hook
(define (imsh win)
  (set! current-windows 
        (list-windows #:only visible? #:except (lambda(x)(equal? x win))))
  (set! current-windows-info
	(append
	 (map wininfo current-windows)
         ;; add lines to represent the edges of the screen
	 (list
	  (list -10000                10000     0        -10000       )
	  (list -10000                10000 10000 (car (display-size)))
	  (list (cadr (display-size)) 10000 10000        -10000       )
	  (list -10000                    0 10000        -10000       ))))
  (set! trail '())
  (set! last-x -10000)
  (set! last-y -10000)
  )

;;;; Interactive move new position hook
(define (imnph win x y)
  (if (or (not (= x last-x))
          (not (= y last-y)))
      (begin
	(set! trail (cons (list x y) trail))
	(set! last-x x)
	(set! last-y y)
	(set! myinfo (wininfo win))
;;;; we dont actually do anything in the current version
;;;; if we were doing the snap interactively we'd do most of
;;;; whats in imfh currently
;;;; for fun, uncomment this:
;;;;        (imfh win)
	))
  )

;;;; Interactive move finish hook
(define (imfh win)
;;;; these should be in the imnph function to make snapping interactive
  (let* (
	 (myinfo (wininfo win))
	 (hozchoice
	  (closest current-windows-info
		   (map (lambda(x)(dist-horizontal myinfo x)) current-windows-info)))
	 (vertchoice
	  (closest current-windows-info
		   (map (lambda(x)(dist-vertical myinfo x)) current-windows-info)))
	 )
    (move-to
     (if (null? hozchoice)
	 (car (window-viewport-position win))
         (cond 
          ;; chose my left to line up my left with other left
          ((= (cddr hozchoice) 0) (wininfo-x (car hozchoice)))
          ;; chose my left to line up my left with other right
          ((= (cddr hozchoice) 1) (+ (wininfo-x     (car hozchoice)) 
                                     (wininfo-width (car hozchoice))))
          ;; chose my left to line up my right with other left
          ((= (cddr hozchoice) 2) (- (wininfo-x (car hozchoice))
                                     (wininfo-width myinfo)))
          ;; chose my left to line up my right with other right
          ((= (cddr hozchoice) 3) (- (+ (wininfo-x     (car hozchoice))
                                        (wininfo-width (car hozchoice)))
                                     (wininfo-width myinfo)))
          (else (car (window-viewport-position win)))))
     (if (null? vertchoice)
	 (cadr (window-viewport-position win))
         (cond 
          ;; chose my top to line up my top with other top
          ((= (cddr vertchoice) 0) (wininfo-y (car vertchoice)))
          ;; chose my top to line up my top with other bottom
          ((= (cddr vertchoice) 1) (+ (wininfo-y (car vertchoice)) 
                                      (wininfo-height (car vertchoice))))
          ;; chose my top to line up my bottom with other top
          ((= (cddr vertchoice) 2) (- (wininfo-y (car vertchoice))
                                      (wininfo-height myinfo)))
          ;; chose my top to line up my bottom with other bottom
          ((= (cddr vertchoice) 3) (- (+ (wininfo-y (car vertchoice))
                                         (wininfo-height (car vertchoice)))
                                      (wininfo-height myinfo)))
          (else (cadr (window-viewport-position win)))))
     win)
    )
  )

(define-public (snap-disable)
  "Turn off auto-snapping during interactive moves."
  (remove-hook! interactive-move-start-hook imsh)
  ;(remove-hook! interactive-move-new-position-hook imnph)
  (remove-hook! interactive-move-finish-hook imfh)
  )

;;; DEPRECATED.  Use 'snap-disable' instead.
(define-public snap-reset snap-disable)

;; (snap-initialize)
(define*-public (snap-initialize #&optional (sw 25))
  "Turn on auto-snapping during interactive moves."
  (set! current-windows '())
  (set! current-windows-info '())
  (set! trail '())
  (set! myinfo '())

  (set! snap-width sw)

  (set! last-x -1)
  (set! last-y -1)

  (add-hook! interactive-move-start-hook imsh)
  ;(add-hook! interactive-move-new-position-hook imnph)
  (add-hook! interactive-move-finish-hook imfh)
  )

