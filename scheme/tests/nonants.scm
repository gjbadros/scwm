;; $Id$

(use-scwm-modules nonants)

(scwm-module-loaded? '(app scwm nonants))

(define (motion-handler-debug x_root y_root state win dx dy)
  (for-each (lambda (v) (display v) (display " "))
	    (list x_root y_root state win dx dy))
  (if win
      (display (nonant->string (window-and-offsets->nonant win dx dy))))
  (newline))

(add-motion-handler! motion-handler-debug)

(remove-motion-handler! motion-handler-debug)

(reset-motion-handlers!)


(define w (select-window-interactively))

(get-window-nonant (list w 79 78))

(window-and-offsets->nonant w 79 78)
(window-and-offsets->nonant w 537 243)
