;;; $Id$
;;; Copyright (C) 1999, 2000 Greg J. Badros


;;; N.B.: We may want to accumulate xforms and call
;;; animate-scwm-resolve only once since when we click
;;; "enable all" in the constraint investigator, the
;;; the constraints are enabled one at a time, each time
;;; causing a resolve.


(define-module (app scwm constraint-animation)
  :use-module (app scwm c-animation))

(define (animate-scwm-resolve solver)
  (let ((xforms (cl-resolve-xforms 50)))
    (if (not (null? xforms))
	(animate-windows xforms))))

(define-public (start-animating-scwm-resolves)
  "Turn on animating windows to new positions following re-solves."
  (add-hook! scwm-resolve-hook animate-scwm-resolve))

(define-public (stop-animating-scwm-resolves)
  "Turn off animating windows to new positions following re-solves."
  (remove-hook! scwm-resolve-hook animate-scwm-resolve))
