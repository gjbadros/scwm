;;;; $Id$
;;;; Copyright (C) 1998 Sam Steingold, Greg J. Badros, and Maciej Stachowiak
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



(define-module (app scwm virtual)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm wininfo))



;;;
;;; Operations to find aligned viewports that are near given coordinates
;;; or contain a given window
;;;

(define-public (containing-aligned-viewport pos)
  "Compute the viewport-position of the viewport that contains POS.
POS is a list of x and y virtual coordinates in that order. The
viewport coordinates returned will be in the same format and will be a
integral multiples of the desk size. If POS is not contained in any
valid viewport, for instance if it includes coordinates greater than
zero or greater than the desk size times the display size, the nearest
viewport will be returned instead."
  (map (lambda (vc ds dds)
	 (max 0 (min (* (- dds 1) ds) (- vc (modulo vc ds)))))
       pos (display-size) (desk-size)))


(define-public (nearest-aligned-viewport pos)
  "Compute the nearest valid integral viewport position to POS.
POS is a list of x and y virtual coordinates in that order. The
viewport coordinates returned will be in the same format and will be a
integral multiples of the desk size."
  (map (lambda (vc ds dds)
	 (max 0 (min (* ds (- dds 1)) 
		     (let ((mvcds (modulo vc ds)))
		       (if (> mvcds (/ ds 2))
			   (+ ds (- vc mvcds))
			   (- vc mvcds))))))
       pos (display-size) (desk-size)))

(define-public (window-aligned-viewport win)
  "Return the viewport that contains the center of WIN.
The return value is an integral multiple of the viewport size which
contains the center of the window, or the closest possible if none
does."
  (containing-aligned-viewport (window-center-position win)))



;;;
;;; Higher-level operations for changing a window's viewport.
;;;

(define*-public (move-window-to-viewport xx yy #&optional win)
  "Move WIN to the viewport numbered (XX,YY).
The (0,0) viewport is the starting viewport.  XX and YY are
full display-size increments (e.g., (1,0) is the viewport
just to the right of the home (0,0) viewport)."
  (let ((d-s (desk-size)))
    (if (or (> xx (- (car d-s) 1)) (> yy (- (cadr d-s) 1)))
	(error "viewport position outside range of desk-size")))
  (let ((pos (window-position win)))
    (move-window (+ (* xx display-width)
		    (modulo (car pos) display-width))
		 (+ (* yy display-height)
		    (modulo (cadr pos) display-height)) win)))

(define-public (move-inside-viewport win x y)
  "Ensure that WIN is entirely inside the X, Y viewport if possible.
X and Y are given in pixels."
  (let ((final-position (map (lambda (wp vp ds ws)
			       (cond 
				((< wp vp) vp)
				((> (+ wp ws) (+ vp ds)) (- (+ vp ds) ws))
				(else wp)))
			     (window-virtual-position win) (list x y) 
			     (display-size) (window-frame-size win))))
    (move-window (car final-position) (cadr final-position) win)))

(define-public (move-inside-aligned-viewport win xx yy)
  "Ensure that WIN is entirely inside the XX, YY viewport if possible.
XX and YY are given in units of the display size."
  (apply move-inside-viewport win (map * (list xx yy) (display-size))))

(define-public (move-inside-current-viewport win)
  "Ensure that WIN is entirely inside the current viewport, if possible."
  (apply move-inside-viewport win (viewport-position)))

(define-public (move-inside-own-viewport win)
  "Ensure that WIN is entirely inside the closest viewport to its center.
The viewport selected will be an integral multiple of the desk size."
  (apply move-inside-viewport win (window-aligned-viewport win)))


;;;
;;; Higher-level operations for setting the current viewport.
;;;

(define-public (move-to-viewport xx yy)
    "Move to the viewport numbered (XX,YY).
The (0,0) viewport is the starting viewport.  XX and YY are
full display-size increments (e.g., (1,0) is the viewport
just to the right of the home (0,0) viewport)."
    (apply set-viewport-position! (map * (list xx yy) display-size)))

(define-public (align-viewport)
  "Set the viewport position to the nearest multiple of the desk size."
  (apply set-viewport-position! 
	 (nearest-aligned-viewport (viewport-position))))

(define-public (warp-to-window-viewport win)
  "Change to the desk and viewport of WIN.
The viewport selected is an integral multiple of the viewport size
which contains the center of the window, or the closest possible if
none does."
  (set-current-desk! (window-desk win))
  (apply set-viewport-position! 
	 (window-aligned-viewport win)))

;;;
;;; Remembering and restoring the virtual location.
;;;

(defmacro-public save-virtual-excursion body
  "Execute the statements in BODY and restore the virtual position.
The current desk and viewport position are saved on each entry to
BODY, and restored on each exit, including non-local exits."
  (let ((desk (gensym))
	(vpos (gensym))
	(sdesk (gensym))
	(svpos (gensym)))
    `(let ((,desk #f)
	   (,vpos #f)
	   (,sdesk (current-desk))
	   (,svpos (viewport-position)))
       (dynamic-wind 
	(lambda ()
	  (set! ,desk (current-desk))
	  (set! ,vpos (viewport-position))
	  (set-current-desk! ,sdesk)
	  (apply set-viewport-position! ,svpos))
	(lambda ()
	  ,@body)
	(lambda ()
	  (set! ,sdesk (current-desk))
	  (set! ,svpos (viewport-position))
	  (set-current-desk! ,desk)
	  (apply set-viewport-position! ,vpos))))))

