;;; $Id$
;;; flash-window.scm
;;; Copyright (C) 1999 Greg J. Badros

(define-module (app scwm flash-window)
  :use-module (app scwm optargs)
  :use-module (app scwm time-convert))

(define*-public (flash-window-on #&optional (win (get-window))
				 (color (make-color "red")))
  (flash-window win #:color color #:unflash-delay #f))

(define*-public (flash-window #&optional (win (get-window)) #&key
			      (color (make-color "red"))
			      (unflash-delay .5)
			      (continually #f))
  "Flash WIN's titlebar and boundary color to COLOR for UNFLASH-DELAY seconds.
UNFLASH-DELAY may be #f to not automatically revert back to the original
color.  See `unflash-window'."
  (if (and (eq? (object-property win 'old-bg) #f)
	   (eq? (object-property win 'old-hi-bg) #f))
      (begin
	(set-object-property! win 'old-bg (cadr (get-window-colors win)))
	(set-object-property! win 'old-hi-bg (cadr (get-window-highlight-colors win)))
	(if continually (set-object-property! win 'flashing #t))
	(set-window-background! color win)
	(set-window-highlight-background! color win)
	(if (number? unflash-delay)
	    (add-timer-hook! (sec->usec unflash-delay)
			     (lambda ()
			       (unflash-window win)
			       (if (object-property win 'flashing)
				   (add-timer-hook! (sec->usec unflash-delay)
						    (lambda ()
						      (flash-window win #:color color
								    #:unflash-delay unflash-delay
								    #:continually #f))))))))))
;; (flash-window (get-window) #:continually #t)
;; (stop-flashing-window)

(define*-public (unflash-window #&optional (win (get-window)))
  "Revert WIN's titlebar and boundary color to state before a `flash-window'."
  (let ((old-bg (object-property win 'old-bg))
	(old-hi-bg (object-property win 'old-hi-bg)))
    ;; set-window-background! only takes colors
    (if (color? old-bg)
	(set-window-background! old-bg win))
    ;; set-window-highlight-background! can take #f, too, so just do it
    (set-window-highlight-background! old-hi-bg win)
    (set-object-property! win 'old-bg #f)
    (set-object-property! win 'old-hi-bg #f)))

(define*-public (stop-flashing-window #&optional (win (get-window)))
  (set-object-property! win 'flashing #f)
  (unflash-window win))

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
