;;;; 	Copyright (C) 1997 Maciej Stachowiak
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



(define-module (app scwm wininfo)
  :use-module (app scwm optargs))



;; on-desk?


(define*-public (on-desk? n #&optional (w (get-window)))
  (if w (= n (get-window-desk w))))

(define*-public ((on-desk-n? n) #&optional (w (get-window)))
  (on-desk? n w))

(define*-public (on-current-desk? #&optional (w (get-window)))
  (on-desk? (get-current-desk) w))

(define (rectangle-overlap? x1-1 y1-1 w1 h1 x2-1 y2-1 w2 h2)
  (let ((x1-2 (+ x1-1 w1))
	(y1-2 (+ y1-1 h1))
	(x2-2 (+ x2-1 w2))
	(y2-2 (+ y2-1 h2)))
    (or (and (>= x1-1 x2-1) (<= x1-1 x2-2) (>= y1-1 y2-1) (<= y1-1 y2-2))
	(and (>= x1-1 x2-1) (<= x1-1 x2-2) (>= y1-2 y2-1) (<= y1-2 y2-2))
	(and (>= x1-2 x2-1) (<= x1-2 x2-2) (>= y1-1 y2-1) (<= y1-1 y2-2))
	(and (>= x1-2 x2-1) (<= x1-2 x2-2) (>= y1-2 y2-1) (<= y1-2 y2-2)))))


(define*-public (in-viewport-any-desk? #&optional (w (get-window)))
  (if w (apply rectangle-overlap? 
	       (append
		(get-window-position w)
		(get-window-size w)
		(get-viewport-position)
		(get-display-size)))))

(define*-public (visible? #&optional (w (get-window)))
  (if w (and (on-current-desk? w)
	     (in-viewport-any-desk? w))))

(define-public (get-geometry-string w)
  (let ((i (iconified? w))
	(pos (get-window-position w))
	(size (get-window-size w)))
    (string-append (if i "(" "")
		   "+" (number->string (car pos)) 
		   "+" (number->string (cadr pos))
		   "x" (number->string (car size))
		   "x" (number->string (cadr size))
		   (if i ")" ""))))


