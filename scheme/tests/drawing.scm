;; $Id$

(use-modules (app scwm xlib-drawing))

(xlib-set-drawing-mask! 29)
(xlib-draw-rectangle! 20 20 40 50)
(xlib-draw-line! 20 20 40 50)
(xlib-draw-arc! 20 20 40 50 20 90)
(xlib-set-line-width! 4)
