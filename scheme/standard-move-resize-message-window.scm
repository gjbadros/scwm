;; $Id$
(define-module (app scwm standard-move-resize-message-window)
  :use-module (app scwm resize-with-message-window)
  :use-module (app scwm move-with-message-window)
  :use-module (app scwm move-resize-message-window))

(define-public (set-move-resize-message-window-attributes! FONT FG-COLOR BG-COLOR)
  "Set attributes for the move/resize message window.
FONT is a font object or a string naming the font,
FG-COLOR and BG-COLOR are color objects or strings naming the colors."
  (let ((mwn move-resize-message-window))
    (message-window-set-font! mwn FONT)
    (message-window-set-colors! mwn FG-COLOR BG-COLOR)))

(define-public (gravity->alignments gravity)
  "Return a list of two numeric alignments corresponding to a GRAVITY.
GRAVITY can be one of 'nw, 'n, 'ne, 'w, 'center, 'e, 'sw, 's, 'se
(or spelled-out versions of these)."
   (case gravity
     ((nw northwest north-west) '(  0    0))
     ((n  north)                '(-.5    0))
     ((ne northeast north-east) '( -1    0))
     ((w  west)                 '(  0  -.5))
     ((center)                  '(-.5  -.5))
     ((e  east)                 '( -1  -.5))
     ((sw southwest south-west) '(  0   -1))
     ((s  south)                '(-.5   -1))
     ((se southeast south-east) '( -1   -1))
     (else (error "Invalid gravity specified."))))

(define-public (position-move-resize-message-window! x y gravity)
  "Move the move/resize message window's GRAVITY point to (X,Y).
GRAVITY can be one of 'nw, 'n, 'ne, 'w, 'center, 'e, 'sw, 's, 'se
(see `gravity->alignments')."
  (apply
   (lambda (xa ya)
     (message-window-set-position! move-resize-message-window x y xa ya))
   (gravity->alignments gravity)))
