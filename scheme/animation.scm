;;;; $Id$
;;;; Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
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



(define-module (app scwm animation)
  :use-module (app scwm base)
  :use-module (app scwm winops)
  :use-module (app scwm c-animation)   ;; other modules should use this module, not c-animation
  :use-module (app scwm optargs))



(export animate-windows)
(export animated-move-window)
(export animated-window-shade)
(export animated-window-unshade)
(export animated-resize-window)
(export animated-resize-frame)
(export set-animation!)
(export animation-delay)

(define-public animated-toggle-window-shade
  (make-toggling-winop shaded-window?
		       animated-window-unshade
		       animated-window-shade))


;; MS:FIXME:: Figure out why this is needed (as well as move-to in base)
(define*-public (animated-move-to x y #&optional (win (get-window))
				  (move-pointer-too? #t))
  "Move WIN to viewport coordinates X, Y with animation. 
If X or Y is #f, then do not change that coordinate during 
the move. At least one of X and Y must be a number. This 
moves the pointer with the window unless MOVE-POINTER-TOO? 
is #f."
  (let ((pos (viewport-position)))
    (if (not (sticky-window? win))
	(begin
	  (if x (set! x (+ x (car pos))))
	  (if y (set! y (+ y (cadr pos)))))))
  (animated-move-window x y win move-pointer-too?))

;; (animated-move-window 0 0 (window-with-pointer))
;; (animated-move-to -1 #f)
;; (animated-move-to 0 #f)
;; (animated-move-to #f -1 (window-with-pointer))
