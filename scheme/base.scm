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



(define-module (app scwm base)
  :use-module (app scwm optargs))



(define-public (%x x)
  (inexact->exact (truncate (/ (* x (car (display-size))) 100))))

(define-public (%y y)
  (inexact->exact (truncate (/ (* y (cadr (display-size))) 100))))

(define-public (x- x)
  (- (car (display-size)) x))

(define-public (y- y)
  (- (cadr (display-size)) y))

(define-public (%x- x)
  (inexact->exact (truncate (/ (* (- 100 x) (car (display-size))) 100))))

(define-public (%y- y)
  (inexact->exact (truncate (/ (* (- 100 y) (cadr (display-size))) 100))))

(define*-public (w%x x #&optional (w (get-window)))
  (inexact->exact (truncate (/ (* x (car (window-size w))) 100))))

(define*-public (w%y y #&optional (w (get-window)))
  (inexact->exact (truncate (/ (* y (cadr (window-size w))) 100))))

(define-public (execute command) 
  (system (string-append "exec " command " &")))

(define-public (set-menu-foreground! fg) (set-menu-colors! fg))
(define-public (set-menu-background! bg) (set-menu-colors! #f bg))
(define-public (set-menu-stipple! st) (set-menu-colors! #f #f st))

(define*-public (set-window-foreground! fg #&optional (w (get-window)))
  (set-window-colors! fg #f w))

(define*-public (set-window-background! bg #&optional (w (get-window))) 
  (set-window-colors! #f bg w))


(define-public (move-pointer x y)
  (let ((pos (pointer-position)))
    (move-pointer-to (+ x (car pos)) (+ y (cadr pos)))))

(define-public (move-viewport x y)
  (let ((pos (viewport-position))
    (set-viewport-position! (+ x (car pos)) (+ y (cdr pos))))))

(define*-public (menu-style #&key 
		     (fg #f) (foreground #f)
		     (bg #f) (background #f)
		     (stipple #f) font mwm mwm-style)
  (set-menu-colors! (or fg foreground) (or bg backgroung) stipple)
  (if (bound? font)
      (set-menu-font! font))
  (if (bound? mwm)
      (set-menu-mwm-style! mwm))
  (if (bound? mwm-style)
      (set-menu-mwm-style! mwm-style)))

(define*-public (title-style #&key font height justify)
  (if (bound? font)
      (set-window-font! font))
  (if (bound? height) 
      (set-title-height! height))
  (if (bound? justify)
      (set-title-justify! justify)))


