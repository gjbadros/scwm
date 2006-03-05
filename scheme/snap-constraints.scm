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

(define-module (app scwm snap-constraints)
  :use-module (app scwm base)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm constraints)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm ui-constraints-classes)
  :use-module (app scwm optargs)
  )

(define current-windows '())
(define current-windows-info '())
(define trail '())
(define myinfo '())

(define snap-width 10)

(define last-x -1)
(define last-y -1)

(define (wininfo win)
  (let ((pos (window-viewport-position win))
        (size (window-frame-size win)))
    (list (cadr pos) (+ (car pos) (car size))
          (+ (cadr pos) (cadr size)) (car pos) win)))

(define (wininfo-n wi) (car wi))
(define (wininfo-e wi) (cadr wi))
(define (wininfo-s wi) (caddr wi))
(define (wininfo-w wi) (cadddr wi))
(define (wininfo-win wi) (list-ref wi 4))

(define wininfo-x wininfo-w)
(define wininfo-y wininfo-n)
(define (wininfo-width wi) (- (wininfo-e wi) (wininfo-w wi)))
(define (wininfo-height wi) (- (wininfo-s wi) (wininfo-n wi)))

(define (overlap-lines a1 a2 b1 b2)
  (if (< b1 a1)
      (overlap-lines b1 b2 a1 a2)
      (< b1 a2)))

(define (distance-lines a1 a2 b1 b2)
  (if (< b1 a1)
      (distance-lines b1 b2 a1 a2)
      (min
       (abs (- a2 b1))
       (abs (- a1 b2)))))

(define (closest-edge a1 a2 b1 b2)
  (if (< b1 a1)
      (not (closest-edge b1 b2 a1 a2))
      (< (abs (- a2 b1))
	 (abs (- a1 b2)))))

(define (dist-horizontal a-win b-win)
  (if (not (overlap-lines (wininfo-n a-win) (wininfo-s a-win)
			  (wininfo-n b-win) (wininfo-s b-win)))
      -1
      (distance-lines (wininfo-w a-win) (wininfo-e a-win)
                      (wininfo-w b-win) (wininfo-e b-win))))
					;
					;  (if (> (wininfo-x a-win) (wininfo-x b-win))
					;    (dist-horizontal b-win a-win)
					;    )

(define (dist-vertical a-win b-win)
  (if (not (overlap-lines (wininfo-w a-win) (wininfo-e a-win)
			  (wininfo-w b-win) (wininfo-e b-win)))
      -1
      (distance-lines (wininfo-n a-win) (wininfo-s a-win)
                      (wininfo-n b-win) (wininfo-s b-win))))
					;
					;  (if (> (wininfo-y a-win) (wininfo-y b-win))
					;    (dist-vertical b-win a-win)
					;    )

(define (closest info-l dist-l)
  (define (closest-iter info-l dist-l result len)
    (if (null? info-l)
	(if (< len snap-width)
	    result
	    '())
	(if (and (not (eq? (car dist-l) -1))
		 (< (car dist-l) len))
	    (closest-iter (cdr info-l) (cdr dist-l) (car info-l) (car dist-l))
	    (closest-iter (cdr info-l) (cdr dist-l) result len))))
  (closest-iter info-l dist-l '() 32000))

;;;; Interactive move start hook
(define (imsh win)
  (set! current-windows 
        (list-windows #:only visible? #:except (lambda(x)(equal? x win))))
  (set! current-windows-info
	(append
	 (map wininfo current-windows)
	 (list
	  '(-10000 10000 0 -10000 #f)
	  (list -10000 10000 10000 (car (display-size)) #f)
	  (list (cadr (display-size)) 10000 10000 -10000 #f)
	  '(-10000 0 10000 -10000 #f))))
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
;;;; whats in imeh currently
;;;; for fun, uncomment this:
;;;; (imeh win)
	))
  )

(define-public (edges->nonant horiz vert)
  "Return a nonant number from a list of edges that are involved.
HORIZ can be 'left, 'right. or 'none (or #f).
VERT can be 'top, 'bottom, or 'none (or #f)."
  (cond ((eq? horiz 'left)
	 (case vert
	   ((top) 0)
	   ((bottom) 6)
	   (else 3)))
	((eq? horiz 'right)
	 (case vert
	   ((top) 2)
	   ((bottom) 8)
	   (else 5)))
	(else
	 (case vert
	   ((top) 1)
	   ((bottom) 7)
	   (else 4)))))

;;(edges->nonant 'left 'none)
;;(edges->nonant 'none 'none)
;;(edges->nonant 'left 'top)
;;(edges->nonant 'right 'bottom)


(define-public (invert-nonant nonant)
  "Return the opposite nonant from NONANT.
E.g., passing 0 answers 8, passing 1 answers 7, etc."
  (- 8 nonant))

;;;; Interactive move end hook
(define (imeh win)
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
    (let ((hoz-constraint 'none)  ;; or 'left, 'right
	  (hoz-win #f)
	  (vert-constraint 'none) ;; or 'top, 'bottom
	  (vert-win #f))
      (move-to
       (if (null? hozchoice)
	   #f
	   (begin
	     (set! hoz-win (wininfo-win hozchoice))
	     (if (closest-edge (wininfo-w myinfo) (wininfo-e myinfo)
			       (wininfo-w hozchoice) (wininfo-e hozchoice))
		 (begin
		   (set! hoz-constraint 'left)
		   (- (wininfo-x hozchoice) (wininfo-width myinfo)))
		 (begin
		   (set! hoz-constraint 'right)
		   (+ (wininfo-x hozchoice) (wininfo-width hozchoice))))))
       (if (null? vertchoice)
	   #f
	   (begin
	     (set! vert-win (wininfo-win vertchoice))
	     (if (closest-edge (wininfo-n myinfo) (wininfo-s myinfo)
			       (wininfo-n vertchoice) (wininfo-s vertchoice))
		 (begin
		   (set! vert-constraint 'top)
		   (- (wininfo-y vertchoice) (wininfo-height myinfo)))
		 (begin
		   (set! vert-constraint 'bottom)
		   (+ (wininfo-y vertchoice) (wininfo-height vertchoice))))
	     ))
       win)
      (let* ((n1 (edges->nonant hoz-constraint vert-constraint))
	     (n2 (invert-nonant n1)))
	(if (not hoz-win)
	    (set! hoz-win vert-win))
	(if (and hoz-win
		 (eq? '()
		      (ui-constraints-involving-two-windows hoz-win win))
		 (eq? (sticky-window? win) (sticky-window? hoz-win)))
	    (let ((uic
		   (make-ui-constraint uicc-strict-relpos
				       (list (list hoz-win win) (list n1 n2)))))
	      (if (ui-constraint? uic)
		  (begin
		    (set-object-property! uic 'inferred #t)
		    (enable-ui-constraint uic)))))))
    )
  )
  
(define*-public (snap-reset)
  "Turn off auto-snapping during interactive moves."
  (interactive)
  (remove-hook! interactive-move-start-hook imsh)
  ;; (remove-hook! interactive-move-new-position-hook imnph)
  (remove-hook! interactive-move-finish-hook imeh)
  )

;; ui-constraints
;; snap-constraints
;; (snap-initialize)
(define*-public (snap-initialize #:optional (sw 25))
  "Turn on auto-snapping during interactive moves."
  (interactive)
  (if (not (scwm-master-solver))
      (start-constraints))
  (set! current-windows '())
  (set! current-windows-info '())
  (set! trail '())
  (set! myinfo '())

  (set! snap-width sw)

  (set! last-x -1)
  (set! last-y -1)

  (add-hook! interactive-move-start-hook imsh)
  ;;(add-hook! interactive-move-new-position-hook imnph)
  (add-hook! interactive-move-finish-hook imeh)
  )

#!
;; (bind-mouse 'title "H-3" #f move-after-disabling-constraints)
;; (bind-mouse 'sidebar "H-3" #f move-after-disabling-constraints)
;; (move-after-disabling-constraints)
!#
