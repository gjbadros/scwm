;;;; $Id$
;;;; Copyright (C) 1998, 1999, 2000 Sam Steingold, Greg J. Badros, and Maciej Stachowiak
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



(define-module (app scwm placement)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm window-locations)
  :use-module (app scwm winops)
  :use-module (app scwm virtual))




;;;
;;; -place versions
;;;

(define-public (place-in-viewport win xx yy)
  "Move WIN to the (XX, YY) viewport.
XX and YY are full display-size increments (e.g., (1,0) is the
viewport just to the right of the home (0,0) viewport).
This is equivalent to `move-window-to-viewport', just named
differently for clarity and convenience."
  (move-window-to-viewport xx yy win))

(define-public (place-on-desk win desk)
  "Move window WIN to desk DESK.
This is equivalent to `move-window-to-desk', just named
differently for clarity and convenience."
  (move-window-to-desk desk win))

(define*-public (place-interactively win #&key (resize #f))
  "Place WIN interactively.

Firts WIN is moved interactively with a rubber-band style move, then,
if the optional RESIZE argument is provided, it is resized
interactively immediately after it is placed."
  (interactive-place-internal win resize))


(define*-public (place-at-point win #&key (offset '(0 0))
			       (proportional-offset '(-0.5 -0.5)))
  "Place WIN at the pointer position.

If the keyword argument OFFSET is specified, it is interpreted as a
list of x and y offsets to add to the pointer position.

If the keyword argument PROPORTIONAL-OFFSET is specified, it is
interpreted as a list of numbers to multiply by the window's width and
height, and is treated as an extra offset.

The defaults are (0 0) for OFFSET and (-0.5 -0.5) for
PROPORTIONAL-OFFSET, with the result that by default the window is
centered at the mouse pointer position."
  (place-at-point-internal win offset proportional-offset))


(define-public (place-at-center win)
  "Place window in the center of the current viewport."
  (let* ((sz (window-size win))
	 (w (car sz))
	 (h (cadr sz))
	 (x (- (half display-width) (half w)))
	 (y (- (half display-height) (half h))))
    (move-to x y win)))

;;;
;;; -placement versions
;;;

(define-public (at-virtual-offset-placement x y)
  "Return a procedure that takes a window and places it at virtual offset X, Y.
The procedure will act just like calling `move-window' on the
window with the same X and Y arguments."
  (lambda (win) (move-window x y win)))

(define-public (at-vp-offset-placement x y)
  "Return a procedure that takes a window and places it at viewport offset X, Y.
The procedure will act just like calling `move-window-viewport-position' on the
window with the same X and Y arguments."
  (lambda (win) (move-window-viewport-position x y win)))
;; (move-window-viewport-position 50 50 (get-window))

(define-public (in-viewport-placement xx yy)
  "Return a procedure that takes a window and places it in viewport (XX, YY).
The procedure will act just like calling `place-in-viewport' on the
window with the same XX and YY arguments."
  (lambda (win) (place-in-viewport win xx yy)))

(define-public (on-desk-placement desk)
  "Return a procedure that takes a window and places it on DESK.
The procedure will act just like calling `place-on-desk' on the
window with the same DESK argument."
  (lambda (win) (place-on-desk win desk)))

(define*-public (interactive-placement #&key (resize #f) (switch #t)
  (return #f))
  "Return a procedure that takes a window and places it interactively.
If the RESIZE argument is true, the resulting procedure will
interactively resize the window immediately after placing it. If
SWITCH is true, the returned procedure will switch to the virtual desk
and viewport of its window argument before placing it. This is the
default. If RETURN is false, the returned procedure will switch back
to the previous desk and viewport after placing the window. The
default is false.

See also the related `interactive-place' procedure which directly
places a window interactively."
  (wrap-switch-return switch return
   (lambda (win) (interactive-place-internal win resize))))

(define*-public (at-point-placement #&key (offset '(0 0))
			       (proportional-offset '(-0.5 -0.5))
			       (switch #t) (return #f))
  "Return a procedure that places a window at the pointer position.
If the keyword argument OFFSET is specified, it is interpreted as a
list of x and y offsets to add to the pointer position. If the keyword
argument PROPORTIONAL-OFFSET is specified, it is interpreted as a list
of numbers to multiply by the window's width and height, and is
treated as an extra offset.The defaults are (0 0) for OFFSET and (-0.5
-0.5) for PROPORTIONAL-OFFSET, with the result that by default the
window is centered at the mouse pointer position by the returned
procedure.

If SWITCH is true, the returned procedure will switch to the virtual
desk and viewport of its window argument before placing it. This is
the default. If RETURN is false, the returned procedure will switch
back to the previous desk and viewport after placing the window. The
default is false.

See also the related `place-at-point' procedure which directly places
a window at the pointer position."
  (wrap-switch-return switch return
   (lambda (win)
     (place-at-point-internal win offset proportional-offset))))

(define*-public (auto-accept-dialog-placement #&optional (delay 500))
  "Return a procedure that auto-accepts a dialog box window.
DELAY is the number of msec to delay before sending the \"Return\"
keystroke to accept the dialog."
  (lambda (win)
    (add-timer-hook! delay (lambda () (send-key "Return" win)))))

(define*-public (near-window-placement window-getter #&key (offset '(0 0))
				       (proportional-offset '(-0.5 -0.5))
				       (relative-to 'center))
  "Return a procedure that places a window near the window returned by WINDOW-GETTER.

If RELATIVE-TO is specified, it gives a symbolic location in the
existing window returned by WINDOW-GETTER to use as the control point
for the window placement.  RELATIVE-TO may be any of northwest north
northeast west center east southwest south southeast.

If OFFSET is specified, it is interpreted as a list of x and y offsets
to add to the control point.

If PROPORTIONAL-OFFSET is specified, it is interpreted as a list of
numbers to multiply by the being-placed window's width and height, and
is treated as an extra offset added to the control point.

The defaults are (0 0) for OFFSET and (-0.5 -0.5) for
PROPORTIONAL-OFFSET, with the result that by default the window is
centered at the control point of the existing window."
  (lambda (win)
    (let* ((nearwin (window-getter))
	   (ns-pos (window-viewport-position-of relative-to nearwin))
	   (final-pos
	    (map (lambda (pp ws o po)
		   (+ pp o (inexact->exact (round (* ws po)))))
		 ns-pos (window-frame-size win)
		 offset proportional-offset)))
      (with-window win (apply move-to final-pos)))))

;; conveniences

(define*-public (virtual-switch-placement proc #&key (switch #t) (return #f))
  "Wrap placement procedure PROC with virtual switching code.
PROC is a procedure that takes a single window argument. The return
value is also a procedure of one one window argument which has the
same effect, except that it may switch to the desk and viewport of the
passed window before executing PROC, and may return to the previous
desk and viewport, depending on the values of SWITCH and RETURN
respectively."
  (wrap-switch-return switch return proc))

(define-public (make-keep-winclass-centered class)
  "Return a procedure that keeps windows of CLASS centered in the viewport.
The resulting procedure should be used put in the `X-ConfigureRequest-hook'."
  (lambda (win icon? x y width height)
    (if (and (not icon?)
	     (string=? (window-class win) class))
	(begin
	  (resize-window width height win 
			 (- (viewport-center-x) (half width))
			 (- (viewport-center-y) (half height)))
	  (set! configure-request-handled #t)))))


;; implementation internals

(define (wrap-switch-return switch return proc)
  ((if return save-place-excursion id)
   (lambda (win)
     (if switch
	 (warp-to-window-viewport win))
     (proc win))))

(define (save-place-excursion proc)
  (lambda (win)
    (save-virtual-excursion
     (proc win))))

(define (place-at-point-internal win offset proportional-offset)
  (let ((final-pos
	 (map (lambda (pp ws o po wav)
		(+ wav pp o (inexact->exact (round (* ws po)))))
              (pointer-position) (window-frame-size win)
              offset proportional-offset (window-aligned-viewport win))))
    (move-window (car final-pos) (cadr final-pos) win)
    (move-inside-current-viewport win)))

(define (interactive-place-internal win resize)
  (let ((pp (pointer-position)))
    (move-to (car pp) (cadr pp) win))
  (rubber-band-move win)
  (if resize
      (rubber-band-resize win)))
