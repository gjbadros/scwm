;;;; $Id$
;;;; auto-shrink.scm
;;;; Copyright (C) 2000 Greg J. Badros <gjb@cs.washington.edu>
;;;;

(define-module (app scwm auto-shrink)
  :use-module (app scwm optargs)
  :use-module (app scwm defoption)
  :use-module (app scwm style)
  :use-module (app scwm style-options))

;;(window-last-focus-time (get-window))
;;(current-time)
;;(define w (select-window))
;;(shrink-window #:win w)

(define-public auto-shrink-delay-seconds 5)

(define*-public (shrink-window #&key (frac .99) (min-width 30) (min-height 45) (win (get-window)))
  ""
  (interactive)
  (let* ((sz (window-size win))
	 (wid (car sz))
	 (ht (cadr sz)))
    (let* ((new-wid (inexact->exact (* frac wid)))
	   (new-ht (inexact->exact (* frac ht))))
      (if (< new-wid min-width)
	  (set! new-wid min-width))
      (if (< new-ht min-height)
	  (set! new-ht min-height))
      (resize-frame new-wid new-ht win))))

(define*-public (not-focussed-for? seconds #&optional (win (get-window)))
  (let ((lf (window-last-focus-time win))
	(ct (current-time)))
    (> (- ct lf) seconds)))

(define*-public (shrink-inactive-windows delay-seconds)
  (for-each 
   (lambda (win) (shrink-window #:win win))
   (list-windows #:only (lambda (win) (and
				       (not (iconified-window? win))
				       (not (eq? (window-with-focus) win))
				       (not-focussed-for? 10 win))))))


(define reinstall-autoshrink #t)

(define (shrink-and-reinstall)
  (shrink-inactive-windows auto-shrink-delay-seconds)
  (if reinstall-autoshrink
      (add-timer-hook! (sec->msec auto-shrink-delay-seconds) shrink-and-reinstall)))

(define*-public (enable-timed-autoshrink-windows)
  ""
  (interactive)
  (set! reinstall-autoshrink #t)
  (shrink-and-reinstall))

(define*-public (disable-timed-autoshrink-windows)
  ""
  (interactive)
  (remove-timer-hook! shrink-and-reinstall)
  (set! reinstall-autoshrink #f))
