;;;; $Id$
;;;; Copyright (C) 1999 Jeff W. Nichols
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

;; This file is the beginnings of a gdk-based drawing module for the
;; ui-constraints interface.  The goal is to have inter-changeable
;; libraries so that the user can decide between using xlib, gdk and
;; possibly other graphics libraries to draw the constraint 
;; representations.

;; WARNING!!
;; This file depends on an addition to guile-gtk that is currently
;; only available through cvs.  Please consult your guile-gtk 
;; documentation to determine how to obtain the latest version.

(define-module (app scwm ui-constraints-gdk-drawing)
  :use-module (app scwm ui-constraints-buttons)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (gtk gdk))

;; module variables

(define ui-constraints-gc #f)
(define ui-constraints-drawable #f)

;; initialize

(define-public (gdk-drawing-initialize)
"Setup the draw functions for drawing onto the root window.
Requires the ui-constraints-buttons window be initialized and
made visible, otherwise initialization will not occur."
  (let* ((gtkwin ui-constraints-buttons-window)
	 (gdkwin (gtk-widget-window gtkwin)))
    (if gdkwin
	(let* ((root (gdk-window-get-parent gdkwin))
	       (gc (gdk-gc-new root)))
	  (gdk-gc-set-subwindow gc 'include-inferiors)
	  (set! ui-constraints-gc gc)
	  (set! ui-constraints-drawable root)))))


;; member functions

(define-public (gdk-drawing-initialized?)
"Returns whether or not the module has been initialized."
  (if ui-constraints-gc #t #f))

(define-public (gdk-drawing-get-gc)
"Returns the gc used to draw on the root window."
  ui-constraints-gc)

(define-public (gdk-drawing-get-drawable)
"Returns the drawable object used to draw on the root window."
  ui-constraints-drawable)


;; drawing functions
;; these match the format of the xlib-drawing calls

(define-public (gdk-drawing-set-line-width! width)
"Set the width of the line to draw with."
  (gdk-gc-set-line-attributes ui-constraints-gc width 'solid 'butt 'miter))

(define-public (gdk-draw-rectangle! top-left width height)
"Draw a rectangle on the root window."
  (let ((drawable ui-constraints-drawable)
	(gc ui-constraints-gc)
	(left (car top-left))
	(top (cdr top-left)))
    (gdk-draw-rectangle drawable gc left top width height)))

(define-public (gdk-draw-line! p1 p2)
"Draw a line on the root window."
  (let ((drawable ui-constraints-drawable)
	(gc ui-constraints-gc)
	(x1 (car p1))
	(y1 (cdr p1))
	(x2 (car p2))
	(y2 (cdr p2)))
    (gdk-draw-line drawable gc x1 y1 x2 y2)))

(define-public (gdk-draw-arc! top_left width height angle1 angle2)
"Draw an arc on the root window."
  (let ((drawable ui-constraints-drawable)
	(gc ui-constraints-gc)
	(x (car top_left))
	(y (cdr top_left))
	(a1 (* angle1 64))
	(a2 (* angle2 64)))
    (gdk-draw-arc drawable gc x y width height angle1 angle2)))


;; (define win (gtk-window-new 'toplevel))
;; (define area (gtk-drawing-area-new))
;; (gtk-container-add win area)
;; (define style (gtk-widget-style win))
;; (define wnd (gtk-widget-window win))
;; (define gc (gdk-gc-new wnd))


