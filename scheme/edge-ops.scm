;;;; $Id$
;;;; Copyright (C) 1999 Sam Falkner
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



(define-module (app scwm edge-ops)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  :use-module (app scwm winlist))



(define-scwm-group edge-ops "Edge Operations")

(define current-edge-ops-scroll-delay #f)
(define edge-ops-time-hook #f)
(define edge-ops-wrap-avoid #f)
(define edge-ops-last-scroll 0)

(define-scwm-option *edge-ops-scroll-delay* #f
  "Delay in milliseconds for scrolling if the mouse cursor is on the edge."
  #:type 'integer
  #:group 'edge-ops
  #:range '(0 . 10000)
  #:setter (lambda (v)
	     (set! *edge-ops-scroll-delay* v)
	     (set! current-edge-ops-scroll-delay #f))
  #:favorites '(0 100 300 500 1000 2000 3000))

(define-scwm-option *edge-ops-scroll-backoff* #t
  "Allow edge scrolling with no delay, after an initial scroll.  
If #f, then this never happens.  If #t, then this always happens.  If
#a number, then it's the time in milliseconds which, after no
#scrolling has occured, the delay goes back to its original value."
  #:type 'boolean
  #:group 'edge-ops
  )

(add-hook!
 edge-enter-hook
 (lambda (dir)
   (cond
    ((not current-edge-ops-scroll-delay)
     (set! current-edge-ops-scroll-delay (scwm-option-get *edge-ops-scroll-delay*))))
   (let* ((dtime current-edge-ops-scroll-delay)
	  (pointer-pos (pointer-position))
	  (p-x (car pointer-pos))
	  (p-y (cadr pointer-pos)))
     (cond
      (edge-ops-wrap-avoid
       (set! edge-ops-wrap-avoid #f))
      ((and (eq? dir 'north) (edge-y-wrap))
       (set! edge-ops-wrap-avoid #t)
       (move-pointer-to p-x (cadr (display-size))))
      ((and (eq? dir 'south) (edge-y-wrap))
       (set! edge-ops-wrap-avoid #t)
       (move-pointer-to p-x 0))
      ((and (eq? dir 'east) (edge-x-wrap))
       (set! edge-ops-wrap-avoid #t)
       (move-pointer-to 0 p-y))
      ((and (eq? dir 'west) (edge-x-wrap))
       (set! edge-ops-wrap-avoid #t)
       (move-pointer-to (car (display-size)) p-y))

      ((and dtime (equal? dtime 0))
       (edge-ops-scroll dir))
      ((and dtime (> dtime 0))
       (set! edge-ops-time-hook
	     (add-timer-hook!
	      (* dtime 1000)
	      (lambda () (edge-ops-scroll-timer dtime dir)))))))))

(define (edge-ops-scroll-timer dtime dir)
  (cond
   ((and edge-ops-scroll-backoff (> current-edge-ops-scroll-delay 1))
    (set! current-edge-ops-scroll-delay 1) ; not zero :-(
    (set! edge-ops-time-hook
	  (add-timer-hook! 1 (lambda () (edge-ops-scroll-timer 1 dir))))
    (cond
     ((number? edge-ops-scroll-backoff)
      (add-timer-hook!
       (* edge-ops-scroll-backoff 1000)
       (lambda () (edge-ops-delay-reset))))))
   (#t
    (set! edge-ops-time-hook
	  (add-timer-hook!
	   (* dtime 1000)
	   (lambda () (edge-ops-scroll-timer dtime dir))))))
  (edge-ops-scroll dir))

(define (edge-ops-delay-reset)
  (let ((how-long (- (current-time) edge-ops-last-scroll)))
    (cond
     ((< (* how-long 1000) edge-ops-scroll-backoff)
      (add-timer-hook!
       (* edge-ops-scroll-backoff 1000)
       (lambda () (edge-ops-delay-reset))))
     (#t
      (set! current-edge-ops-scroll-delay (scwm-option-get *edge-ops-scroll-delay*))))))

(add-hook!
 edge-leave-hook
 (lambda (dir)
   (cond
    (edge-ops-time-hook
     (remove-timer-hook! edge-ops-time-hook)
     (set! edge-ops-time-hook #f)))))

(define (edge-ops-scroll direction)
  (set! edge-ops-last-scroll (current-time))
  (let* ((pp (pointer-position))
	 (ppx (car pp))
	 (ppy (cadr pp))
	 (dy (edge-y-scroll))
	 (dx (edge-x-scroll)))
    (cond
     ((eq? direction 'north)
      (move-viewport 0 (* -1 dy))
      (move-pointer-to ppx dy))
     ((eq? direction 'south)
      (move-viewport 0 dy)
      (move-pointer-to ppx (- (cadr (display-size)) dy)))
     ((eq? direction 'west)
      (move-viewport (* -1 dx) 0)
      (move-pointer-to dx ppy))
     ((eq? direction 'east)
      (move-viewport dx 0)
      (move-pointer-to (- (car (display-size)) dx) ppy)))))
