;;; $Id$
;;; synth-events.scm

(define-module (app scwm synth-events)
  :use-module (app scwm optargs))

(define*-public (send-button-1)
  "Send a mouse-1 button press and release event.
Note: this sends synthetic events which some applications ignore."
  (interactive)
  (send-button 1))

(define*-public (send-button-2)
  "Send a mouse-2 button press and release event.
Note: this sends synthetic events which some applications ignore."
  (interactive)
  (send-button 2))

(define*-public (send-button-3)
  "Send a mouse-3 button press and release event.
Note: this sends synthetic events which some applications ignore."
  (interactive)
  (send-button 3))
