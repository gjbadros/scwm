;; $Id$

(use-modules (app scwm xlib-drawing))

(xlib-set-drawing-mask! 2147483647)  ;; 2^31-1, nice for 32 bit displays
(xlib-set-line-width! 4)
(xlib-draw-rectangle! '(20 . 20) 40 50)
(xlib-draw-line! '(20 . 20) '(40 . 50))
(xlib-draw-arc! '(20 . 20) 40 50 20 90)
