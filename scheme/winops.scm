;;;; $Id$
;;;; Copyright (C) 1997-1998 Maciej Stachowiak and Greg J. Badros
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


;;; FIXMS: disgusting hack for now to get these in the root module.


(define-public opaque-move-percent 
;;;**VAR
;;; Percent of display area below which windows are moved opaquely.
  50)

(define-public opaque-resize-percent 
;;;**VAR
;;; Percent of display area below which windows are resized opaquely.
  35)


(define-module (app scwm winops)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm style-options))



(define*-public ((make-toggling-winop pred neg pos) 
		 #&optional (w (get-window)))
  (if w (if (pred w)
	    (neg w)
	    (pos w))))

(define*-public (close-window #&optional (win (get-window #t)))
  "Close WIN either by deleting it or destroying it.
WIN is only destroyed if it is not deleteable."
  (if w (if (window-deletable? win)
	    (delete-window win)
	    (destroy-window win))))
 
(define-public toggle-raise
  (make-toggling-winop raised? lower-window raise-window))

(define-public toggle-iconify
  (make-toggling-winop iconified? deiconify iconify))

(define-public toggle-stick 
  (make-toggling-winop sticky? unstick stick))

(define-public toggle-window-shade 
  (make-toggling-winop window-shaded? un-window-shade window-shade))

(define-public (window-shade-animated win)
  "Make WIN be window-shaded animatedly.
This rolls the client window up into the titlebar, much as
snapping up a window shade on a window."
  (window-shade win #t))

;; FIXGJB: rename to window-unshade-animated

(define-public (un-window-shade-animated win)
  "Make WIN be not window-shaded animatedly.
This rolls the client window down out of the titlebar, much as
pulling down a window shade on a window."
  (un-window-shade win #t))

(define-public toggle-window-shade-animated
  (make-toggling-winop window-shaded? 
		       un-window-shade-animated 
		       window-shade-animated))

(define-public toggle-on-top
  (make-toggling-winop kept-on-top? un-keep-on-top keep-on-top))

(define-public toggle-titlebar
  (make-toggling-winop titlebar-shown? hide-titlebar show-titlebar))

(define-public toggle-border
  (make-toggling-winop border-normal? plain-border normal-border))

(define-public toggle-stick-icon
  (make-toggling-winop icon-sticky? unstick-icon stick-icon))

(define*-public (maximize nw nh #&optional (win (get-window)))
  "Maximize WIN to new width NW and new height NH.
If NW or NH is 0, that dimension is not changed."
  (if win (let* ((pos (window-position win))
	       (size (window-frame-size win))
	       (x (car pos))
	       (y (cadr pos))
	       (width (car size))
	       (height (cadr size)))
	  (if (not (maximized? win))
	      (set-object-property! win 'maximized 
				    (list x y width height)))
	  (move-to (if (> nw 0) 0 x)
		   (if (> nh 0) 0 y) win)
	  (resize-frame-to (if (> nw 0) nw width)
			   (if (> nh 0) nh height) win))))

(define*-public (maximized? #&optional (win (get-window)))
  "Return #t if WIN is maximized, #f otherwise."
  (->bool (object-property win 'maximized)))

;; FIXGJB: use client units
(define*-public (unmaximize #&optional (win (get-window)))
  "Unmaximize WIN so it returns to its size before maximization.
This should use client units, but currently uses frame-size in pixels."
  (if win (let ((max-prop (object-property win 'maximized)))
	  (cond
	   (max-prop (move-to (car max-prop)
			      (cadr max-prop) win)
		     (resize-frame-to (caddr max-prop)
				      (cadddr max-prop) win)
		     (set-object-property! win 'maximized #f))))))

(define-public (window-frame-area win)
  "Return the number of square pixels of area that WIN is."
  (let* ((frame-size (window-frame-size win))
	 (width (car frame-size))
	 (height (cadr frame-size)))
    (* width height)))

(define display-area (* display-width display-height))

(define-public (resize-opaquely? win)
  "Return #t if WIN has area < opaque-resize-percent of the screen, else #f."
  (< (window-frame-area win) 
     (* display-area (/ (scwm-user-var opaque-resize-percent) 100))))

(define-public (move-opaquely? win)
  "Return #t if WIN has area < opaque-move-percent of the screen, else #f."
  (< (window-frame-area win)
     (* display-area (/ (scwm-user-var opaque-move-percent) 100))))

(define*-public (interactive-move-maybe-opaque #&optional (win (get-window)))
  "Move WINDOW interactively and possibly opaquely.
Calls `move-opaquely?' and moves opaquely if that returns #,
uses a rubberband if it returns #f."
  (if win (interactive-move win (move-opaquely? win))))

(define*-public (interactive-resize-maybe-opaque #&optional (win (get-window)))
  "Move WINDOW interactively and opaquely.
Calls `resize-opaquely?' and moves opaquely if that returns #,
uses a rubberband if it returns #f."
  (if win (interactive-resize win (resize-opaquely? win))))

;; (move-opaquely? (select-window-interactively))
;; (resize-opaquely? (select-window-interactively))
;; (interactive-move-maybe-opaque)

;;; Some functions for decoration bindings
(define-public (resize-or-raise-maybe-opaque)
  "Perform a resize, raise, or lower based on the mouse-event-type.
To be bound to a window decoration: click does `raise-window',
motion does `interactive-resize-maybe-opaque', and double-click does
`lower-window'."
  (case (mouse-event-type)
    ((click) (raise-window))
    ((motion) (interactive-resize-maybe-opaque))
    ((double-click) (lower-window))))

(define-public (move-or-raise-maybe-opaque)
  "Perform a move, raise, or lower based on the mouse-event-type.
To be bound to a window decoration: click does `raise-window',
motion does `interactive-move-maybe-opaque', and double-click does
`lower-window'."
  (case (mouse-event-type)
    ((click) (raise-window))
    ((motion) (interactive-move-maybe-opaque))
    ((double-click) (lower-window))))


(define*-public (opaque-interactive-move #&optional (win (get-window)))
  "Move WINDOW interactively and opaquely."
  (if win (interactive-move win #t)))
  
(define*-public (opaque-interactive-resize #&optional (win (get-window)))
  "Resize WINDOW interactively and opaquely."
  (if win (interactive-resize win #t)))


(define*-public (toggle-maximize nw nh #&optional (win (get-window)))
  "Maximize to width NW, height NH if not maximized, or unmaximize."
  (if win (if (maximized? win)
	    (unmaximize win)
	    (maximize nw nh win))))

;; add a style option for maximizing
(add-window-style-option #:start-maximized 
			  (lambda (arg w) 
			    (if arg
				(apply maximize (append arg (list w)))
				(unmaximize w))))



(define*-public (print-window #&optional (win (get-window)))
  "Print WIN using xpr and lpr."
  (if win (execute (string-append "xwd -id " 
				(number->string (window-id win))
				" | xpr | lpr"))))


(define*-public (animated-move-to x y #&optional (win (get-window)))
  "Move WIN to viewport position x, y animatedly.
If X or Y is 'x or 'y, respectively (or #f), then do not change
that coordinate during the move.  At least one of X and Y must be
a number."
  (let* ((size (window-frame-size win))
	 (width (car size))
	 (height (cadr size)))
    (if (equal? x 'x) (set! x #f)
	(if (and (number? x) (< x 0)) (set! x (x- width))))
    (if (equal? y 'y) (set! y #f)
	(if (and (number? y) (< y 0)) (set! y (y- height))))
    (raise-window win)
    (move-to x y win 'animated 'move-pointer-too)))

;; (animated-move-to -1 'y)
;; (animated-move-to 0 'y)
;; (animated-move-to 'x -1 (current-window-with-pointer))
;; (animated-move-to #f -1 (current-window-with-pointer))


;; FIXGJB: the resize-to primitive should just be renamed
(define*-public (resize-window w h #&optional (win (get-window)))
  "Resize WIN's client area to a size of W by H in pixels. 
The size does not include the window decorations -- only the client
application size. WIN defaults to the window context in the usual way
if not specified."
  (resize-to w h win))
