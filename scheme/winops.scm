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



(define-module (app scwm winops)
  :use-module (app scwm optargs))



(define*-public ((make-toggling-winop pred neg pos) 
		 #&optional (w (get-window)))
  (if w (if (pred w)
	    (neg w)
	    (pos w))))

(define*-public (close-window #&optional (w (get-window #t)))
  (if w (if (window-deletable? w)
	    (delete-window w)
	    (destroy-window w))))
 
(define-public toggle-raise
  (make-toggling-winop raised? lower-window raise-window))

(define-public toggle-iconify
  (make-toggling-winop iconified? deiconify iconify))

(define-public toggle-stick 
  (make-toggling-winop sticky? unstick stick))

(define-public toggle-window-shade 
  (make-toggling-winop window-shaded? un-window-shade window-shade))

(define-public toggle-on-top
  (make-toggling-winop kept-on-top? un-keep-on-top keep-on-top))

(define-public toggle-titlebar
  (make-toggling-winop titlebar-shown? hide-titlebar show-titlebar))

(define-public toggle-border
  (make-toggling-winop border-normal? plain-border normal-border))

(define-public toggle-stick-icon
  (make-toggling-winop icon-sticky? unstick-icon stick-icon))

(define*-public (maximize nw nh #&optional (w (get-window)))
  (if w (let* ((pos (window-position w))
	       (size (window-size w))
	       (x (car pos))
	       (y (cadr pos))
	       (width (car size))
	       (height (cadr size)))
	  (if (not (maximized? w))
	      (set-object-property! w 'maximized 
				    (list x y width height)))
	  (move-to (if (> nw 0) 0 x)
		   (if (> nh 0) 0 y) w)
	  (resize-to (if (> nw 0) nw width)
		     (if (> nh 0) nh height) w))))

(define*-public (maximized? #&optional (w (get-window)))
  (->bool (object-property w 'maximized)))

(define*-public (unmaximize #&optional (w (get-window)))
  (if w (let ((max-prop (object-property w 'maximized)))
	  (cond
	   (max-prop (move-to (car max-prop)
			      (cadr max-prop) w)
		     (resize-to (caddr max-prop)
				(cadddr max-prop) w)
		     (set-object-property! w 'maximized #f))))))

(define*-public (toggle-maximize nw nh #&optional (w (get-window)))
  (if w (if (maximized? w)
	    (unmaximize w)
	    (maximize nw nh w))))

(define*-public (print-window #&optional (w (get-window)))
  (if w (execute (string-append "xwd -id " 
				(number->string (window-id w))
				" | xpr | lpr"))))





