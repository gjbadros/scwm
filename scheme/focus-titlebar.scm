;;; $Id$
;;; (C) 1999 Greg J. Badros
;;;

(define-module (app scwm focus-titlebar)
  :use-module (app scwm defoption)
  :use-module (app scwm optargs)
  :use-module (app scwm winops))

(define (show-titlebar-in-place-focussed-window w)
  (and w (window-valid? w) (show-titlebar-in-place w)))

;; GJB:FIXME::
(define-public (turn-on-only-focus-window-has-titlebar)
  "Start displaying a titlebar only on the focussed window."
  (add-hook! window-focus-change-hook show-titlebar-in-place-focussed-window)
  (add-hook! window-focus-lost-hook hide-titlebar-in-place)
  (for-each hide-titlebar-in-place (list-all-windows)))

(define-public (turn-off-only-focus-window-has-titlebar)
  "Do not display titlebars on only the focuessed window."
  (remove-hook! window-focus-change-hook show-titlebar-in-place-focussed-window)
  (remove-hook! window-focus-lost-hook hide-titlebar-in-place)
  (for-each show-titlebar-in-place-focussed-window (list-all-windows)))
  
(define-scwm-option *only-focus-window-has-titlebar* #t
  "If #t, no windows except the focus window will have a titlebar."
  #:type 'boolean
  #:group 'focus
  #:setter (lambda (v)
	     (or
	      (and v (not *only-focus-window-has-titlebar*) (turn-on-only-focus-window-has-titlebar))
	      (and (not v) *only-focus-window-has-titlebar* (turn-off-only-focus-window-has-titlebar)))
	     (set! *only-focus-window-has-titlebar* v))
  #:getter (lambda () *only-focus-window-has-titlebar*))
