;;; $Id$
;;; synth-events.scm

(define-module (app scwm synth-events)
  :use-module (app scwm optargs))

(define*-public (send-button-press-1)
  "Send a mouse-1 button press and release event."
  (interactive)
  (send-button-press 1))

(define*-public (send-button-press-2)
  "Send a mouse-2 button press and release event."
  (interactive)
  (send-button-press 2))

(define*-public (send-button-press-3)
  "Send a mouse-3 button press and release event."
  (interactive)
  (send-button-press 3))
