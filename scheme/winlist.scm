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



(define-module (app scwm winlist)
  :use-module (app scwm optargs)
  :use-module (app scwm wininfo)
  :use-module (app scwm base)
  :use-module (ice-9 common-list))



(define-public (default-winlist-proc w)
  (deiconify w)
  (focus w)
  (raise-window w)
  (warp-to-window w)
  (move-pointer (%x 5) (%y 5)))

(define-public window-list-proc default-winlist-proc)

(define (filter-only-except l only except)
  (pick (lambda (item)
	  (and
	   (and-map (lambda (pred) (pred item)) 
		    (if (list? only) only (list only)))
	   (not (or-map (lambda (pred) (pred item)) 
			(if (list? except) except (list except))))))
	l))


(define*-public (list-windows #&key (only '()) (except '()))
	(filter-only-except (list-all-windows) only except))


(define*-public (show-window-list-menu #&key (only '()) (except '())
				       (proc window-list-proc)
				       (show-geometry #f))
  (popup (apply make-menu (if show-geometry "Window:\tGeometry:"
			      "Window List")
		'title
		(map (lambda (x)
		       (list 
			(if show-geometry
			    (string-append
			     (window-title x) "\t"
			     (window-geometry-string x))
			    (window-title x))
			(lambda () (proc x))))
		     (list-windows #:only only #:except except)))))


(define (rotate-around w wl)
  (append (cond
	   ((memq w wl) => cdr)
	   (else wl))
	  (cond
	   ((memq w (reverse wl)) 
	    => (lambda (x)
		 (reverse (cdr x))))
	   (else '()))))


(define*-public (next-window #&key (window (get-window))
			     (only '()) (except '()) (proc window-list-proc))
  (if window 
      (let* ((wl (list-all-windows))
	     (rotwl (rotate-around window wl)))
	(cond
	 ((filter-only-except rotwl only except)
	  => (lambda (x) (proc (car x))))))))

(define*-public (prev-window #&key (window (get-window))
			     (only '()) (except '()) (proc window-list-proc))
  (if window 
      (let* ((wl (list-all-windows))
	     (rotwl (reverse (rotate-around window wl))))
	(cond
	 ((filter-only-except rotwl only except)
	  => (lambda (x) (proc (car x))))))))

