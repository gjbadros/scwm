;;; $Id$
;;; titlebar-buttons
;;; (C) 1999, 2000 Greg J. Badros
;;; 

;;; I'm not changing these for now to use the new style
;;; because of the way add-left-button and add-right-button work and
;;; such; I think in general, though, setting the button appearance
;;; should be more separated from setting its function. Will look
;;; over this more. --MS
;;; GJB:FIXME:MS: I think actions and conceptual appearance should go together
;;; but that conceptual appearance and actual appearance should be separated.
;;; Now that pixmaps are real objects, I get this by making, e.g.,
;;; `sticky-button-face' and `(add-left-button sticky-button-face toggle-stick)'
;;; I never want to say, `the third button from the right should do this action'.
;;; Ideally, buttons should have some identifier which can be tested by the
;;; event handling code for the callbacks --gjb

(define-module (app scwm titlebar-buttons)
  :use-module (app scwm optargs)
  :use-module (app scwm face))

(define left-button-number 1)
(define right-button-number 1)

(define-public (reset-buttons!)
  "Resets button numbers.
This makes `add-left-button' and `add-right-button' start
from the edges again."
  (set! left-button-number 1)
  (set! right-button-number 1))

(define*-public (add-left-button button-face hook #&optional (immed-hook #f))
  "Add a left button to the current decor."
  (set-left-button-face! left-button-number button-face)
  (bind-mouse (string->symbol 
	       (string-append "left-button-" 
			      (number->string left-button-number)))
	      1 hook immed-hook)
  (set! left-button-number (+ 1 left-button-number)))


(define*-public (add-right-button button-face hook #&optional (immed-hook #f))
  "Add a right button to the current decor."
  (set-right-button-face! right-button-number button-face)
  (bind-mouse (string->symbol 
	       (string-append "right-button-" 
			      (number->string right-button-number)))
	      1 hook immed-hook)
  (set! right-button-number (+ 1 right-button-number)))
