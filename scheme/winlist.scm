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



(define*-public (default-winlist-proc #&optional (w (get-window)))
  (cond
   (w (deiconify w)
      (focus w)
      (raise-window w)
      (warp-to-window w)
      (move-pointer (w%x 20 w) (w%y 20 w)))))

(define (listify-if-atom l)
  (if (or (pair? l) (null? l)) l (list l)))

(define-public window-list-proc default-winlist-proc)

(define (filter-only-except l only except)
  (pick (lambda (item)
	  (and
	   (and-map (lambda (pred) (pred item)) 
		    (listify-if-atom only))
	   (not (or-map (lambda (pred) (pred item)) 
			(listify-if-atom except)))))
	l))


(define*-public (list-windows #&key (only '()) (except '()))
	(filter-only-except (list-all-windows) only except))



(define*-public (winlist-hit #&optional (w (get-window)))
  (if w (set-object-property! w 'winlist-skip #f)))

(define*-public (winlist-skip #&optional (w (get-window)))
  (if w (set-object-property! w 'winlist-skip #t)))

(define*-public (winlist-skip? #&optional (w (get-window)))
  (if w (object-property w 'winlist-skip)) #f)


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
		     (list-windows #:only only #:except 
				   (cons 
				    winlist-skip?
				    (listify-if-atom except))))))) 

(define (rotate-around w wl)
  (append (cond
	   ((memq w wl) => cdr)
	   (else wl))
	  (cond
	   ((memq w (reverse wl)) 
	    => (lambda (x)
		 (reverse (cdr x))))
	   (else '()))))


(define*-public (circulate-hit #&optional (w (get-window)))
  (if w (set-object-property! w 'circulate-skip #f)))

(define*-public (circulate-skip #&optional (w (get-window)))
  (if w (set-object-property! w 'circulate-skip #t)))

(define*-public (circulate-skip? #&optional (w (get-window)))
  (if w (object-property w 'circulate-skip) #f))

(define*-public (circulate-hit-icon #&optional (w (get-window)))
  (if w (set-object-property! w 'circulate-skip-icon #f)))

(define*-public (circulate-skip-icon #&optional (w (get-window)))
  (if w (set-object-property! w 'circulate-skip-icon #t)))

(define*-public (circulate-skip-icon? #&optional (w (get-window)))
  (if w (object-property w 'circulate-skip-icon) #f))

(define*-public (should-circulate-skip? #&optional (w (get-window)))
  (if w 
      (or (circulate-skip? w) (and (iconified? w) (circulate-skip-icon? w)))
      #f))

(define (circulate backwards? window only except proc)
  ;; not totally right... what if we just move the mouse to the
  ;; root window after focusing a window not by circulating?
  ;; things should still work right. This will require the focus 
  ;; machinery to set some appropriate Scheme variables.
  (let ((window (if window window last-circulated)))
    (if window
      (let* ((wl (list-all-windows))
	     (rotwl ((if backwards? reverse id)
		    (rotate-around window wl))))
	(cond
	 ((filter-only-except rotwl only (cons
					  should-circulate-skip? 
					  (listify-if-atom except)))
	  => (lambda (x) (set! last-circulated (car x))
		     (proc (car x))))))))
)

(define*-public (next-window #&key (window (get-window #f #f))
			     (only '()) (except '()) (proc window-list-proc))
  (circulate #f window only except proc))


(define*-public (prev-window #&key (window (get-window #f #f))
			     (only '()) (except '()) (proc window-list-proc))
  (circulate #t window only except proc))


(define last-circulated #f)
