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



(define-module (app scwm animation)
  :use-module (app scwm base)
  :use-module (app scwm winops)
  :use-module (app scwm c-animation)
  :use-module (app scwm optargs))



(define-public animated-move-window animated-move-window)
(define-public animated-window-shade animated-window-shade)
(define-public animated-window-unshade animated-window-unshade)
(define-public set-animation! set-animation!)

(define-public animated-toggle-window-shade
  (make-toggling-winop window-shaded?
		       animated-window-unshade
		       animated-window-shade))


;; MS:FIXME:: Figure out why this is needed (as well as move-to in base)
(define*-public (animated-move-to x y #&optional (win (get-window))
				  (move-pointer-too? #f))
  (let* ((sticky (sticky? win))
	 (pos (viewport-position))
	 (x (if x
		(if sticky
		    (modulo x display-width)
		    (+ x (car pos)))
		x))
	 (y (if y
		(if sticky
		    (modulo y display-height)
		    (+ y (cadr pos)))
		y)))
    (animated-move-window x y win move-pointer-too?)))


;; (animated-move-to -1 'y)
;; (animated-move-to 0 'y)
;; (animated-move-to 'x -1 (current-window-with-pointer))
;; (animated-move-to #f -1 (current-window-with-pointer))
