;;; $Id$
;;; key-mover.scm
;;; Copyright (C) 1999, 2000 Greg J. Badros
;;;
;;; Permit easy binding of keystrokes to pointer and viewport
;;; movement commands.



(define-module (app scwm key-mover)
  :use-module (app scwm optargs)
  :use-module (app scwm base))

(define-public (key-mouse-moves modifiers pct-of-screen left down up right)
  "Bind four keys to move the mouse in compass directions by PCT-OF-SCREEN.
MODIFIERS specifies which modifiers must be depressed for the bindings
to be active.
LEFT, DOWN, UP, and RIGHT are the four keysym names to use for each
of the directions."
  (bind-key 'all (string-append modifiers "-" left)
	    (lambda* () "" (interactive) (move-pointer (%x (- pct-of-screen)) 0)))
  (bind-key 'all (string-append modifiers "-" down)
	    (lambda* () "" (interactive) (move-pointer 0 (%y pct-of-screen))))
  (bind-key 'all (string-append modifiers "-" up)
	    (lambda* () "" (interactive) (move-pointer 0 (%y (- pct-of-screen)))))
  (bind-key 'all (string-append modifiers "-" right)
	    (lambda* () "" (interactive) (move-pointer (%x pct-of-screen) 0))))

(define-public (key-viewport-moves modifiers pct-of-screen left down up right)
  "Bind four keys to move the viewport in compass directions by PCT-OF-SCREEN.
MODIFIERS specifies which modifiers must be depressed for the bindings
to be active.
LEFT, DOWN, UP, and RIGHT are the four keysym names to use for each
of the directions."
  (bind-key 'all (string-append modifiers "-" left)
	    (lambda* () "" (interactive) (move-viewport (%x (- pct-of-screen)) 0)))
  (bind-key 'all (string-append modifiers "-" down)
	    (lambda* () "" (interactive) (move-viewport 0 (%y pct-of-screen))))
  (bind-key 'all (string-append modifiers "-" up)
	    (lambda* () "" (interactive) (move-viewport 0 (%y (- pct-of-screen)))))
  (bind-key 'all (string-append modifiers "-" right)
	    (lambda* () "" (interactive) (move-viewport (%x pct-of-screen) 0))))
