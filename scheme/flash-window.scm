;;; $Id$
;;; flash-window.scm
;;; Copyright (C) 1999 Greg J. Badros

(define-module (app scwm flash-window)
  :use-module (app scwm optargs)
  :use-module (app scwm time-convert))

(define*-public (flash-window-on #&optional (win (get-window))
				 (color "red"))
  "Flash WIN's titlebar and boundary color to COLOR indefinitely.
Returns the window changed.  Use `unflash-window' to rever the
window to its normal colors."
  (interactive)
  (flash-window win #:color color #:unflash-delay #f)
  win)

(define*-public (flash-window #&optional (win (get-window)) #&key
			      (color "red")
			      (unflash-delay .5)
			      (continually #f))
  "Flash WIN's titlebar and boundary color to COLOR for UNFLASH-DELAY seconds.
UNFLASH-DELAY may be #f to not automatically revert back to the original
color.  See `unflash-window'."
  (interactive)
  (if (and (window-valid? win)
	   (eq? (object-property win 'old-bg) #f)
	   (eq? (object-property win 'old-hi-bg) #f))
      (begin
	(if (string? color) (set! color (make-color color)))
	(if (not (color? color)) (set! color (make-color "red")))
	(set-object-property! win 'old-bg (cadr (get-window-colors win)))
	(set-object-property! win 'old-hi-bg (cadr (get-window-highlight-colors win)))
	(if continually (set-object-property! win 'flashing #t))
	(set-window-background! color win)
	(set-window-highlight-background! color win)
	(if (number? unflash-delay)
	    (add-timer-hook! (sec->msec unflash-delay)
			     (lambda ()
			       (unflash-window win)
			       (if (object-property win 'flashing)
				   (add-timer-hook! 
				    (sec->msec unflash-delay)
				    (lambda ()
				      (flash-window win
						    #:color color
						    #:unflash-delay unflash-delay
						    #:continually #f))))))))))
;; (flash-window (get-window) #:continually #t)
;; (stop-flashing-window)

(define*-public (window-flashing? #&optional (win (get-window)))
  "Return #t iff WIN is currently flashing, #f otherwise."
  (object-property win 'flashing))

(define*-public (unflash-window #&optional (win (get-window)))
  "Revert WIN's titlebar and boundary color to state before a `flash-window'.
Return the window changed."
  (interactive)
  (let ((old-bg (object-property win 'old-bg))
	(old-hi-bg (object-property win 'old-hi-bg)))
    ;; set-window-background! only takes colors
    (if (color? old-bg)
	(set-window-background! old-bg win))
    ;; set-window-highlight-background! can take #f, too, so just do it
    (set-window-highlight-background! old-hi-bg win)
    (set-object-property! win 'old-bg #f)
    (set-object-property! win 'old-hi-bg #f))
  win)

(define*-public (stop-flashing-window #&optional (win (get-window)))
  (interactive)
  "Turn off window flashing of WIN.
Has no effect if WIN is not flashing.
N.B. flashing and highlight-selection of windows currently use the
same mechanism, so turning off flashing will also visually un-select
WIN."
  (set-object-property! win 'flashing #f)
  (unflash-window win)
  win)

(define*-public (raise-and-stop-flashing #&optional (win (get-window)))
  "Turn off window flashing of WIN and raise it.
Also return #f so that this can be used as an bound IMMEDIATE-PROC."
  (interactive)
  (if (window-flashing? win)
      (stop-flashing-window win))
  (raise-window win)
  #f)

;;; Debugging, testing code.
;; (flash-window (get-window) #:unflash-delay #f)
;; (unflash-window)
;; (object-properties (get-window))
;; (get-window-highlight-colors)
;; (set-window-highlight-background! "navyblue")
;; (set-window-background! "grey75")
;; (set-window-highlight-background! (object-property win 'old-hi-bg) win)
;; (color? (object-property win 'old-hi-bg))
;; (define win (get-window))
