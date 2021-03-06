;;;; Copyright (C) 2006 Dale P. Smith
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



(define-module (app scwm xlib-drawing)
  #:export (xlib-set-drawing-mask!
	    xlib-draw-rectangle!
	    xlib-draw-line!
	    xlib-draw-arc!
	    xlib-set-line-attributes!
	    xlib-set-fill-style!))

(eval-when (eval load compile)
  (load-extension "scwm-xlib-drawing" "init_drawing"))
