;;;; $Id$
;;;; Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 



(define-module (app scwm auto-raise)
  :use-module (app scwm optargs)
  :use-module (app scwm defoption)
  :use-module (app scwm style)
  :use-module (app scwm style-options)
  :use-module (app scwm module-types))



;;;; auto-raise
;;;;
;;;; This module provides a convenient wrapper for the timer and
;;;; broadcast hook functionality to get auto-raise and similar
;;;; effects.  To get all your windows to do the default auto-raise,
;;;; just do the following:
;;;;
;;;;   (window-style "*" #:auto-raise #t)
;;;;
;;;; This will cause windows to be raise 300 milliseconds after they
;;;; get focus, unless they lose focus first.
;;;;
;;;; A slightly more complicated example:
;;;;
;;;;   (window-style "*" #:auto-raise #t #:auto-raise-delay 400
;;;;    #:auto-raise-focus-proc raise-window)
;;;;
;;;; You can also set a separate action to occur with a delay after
;;;; the window is unfocused. This action will be cancelled if the
;;;; window receives focus again before the timer times out. An
;;;; example:
;;;;
;;;;   (window-style "xterm" #:auto-raise #t #:auto-raise-delay 300
;;;;    #:auto-raise-focus-proc (lambda (w) (maximize 0 (%y 100) w))
;;;;    #:auto-raise-unfocus-delay 400 #:auto-raise-unfocus-proc unmaximize)
;;;;
;;;; This will make xterm windows that get focus become vertically
;;;; maximized with a 300 millisecond delay, and unmaximized 400
;;;; millisecnds after they lose focus.
;;;;
;;;; The unfocus delay defaults to be the same as the regular delay,
;;;; and the unfocus proc defualts to noop, that is, do nothing.
;;;;
;;;; Auto-raise probably works best with mouse or sloppy focus, since
;;;; it is based on the window getting focus, not on the window being
;;;; entered. That may change in the future.

(define-scwm-group focus "Focus")

(define-scwm-option *default-auto-raise-delay* 300
  "Number of ms to delay before raising the window the mouse pointer entered.
This can be overridden on a per-window basis using `set-auto-raise-delay!'"
  #:type 'integer
  #:group 'focus
  #:range '(0 . 10000)
  #:favorites '(0 100 300 500 1000 2000 3000))

(define-scwm-option *default-auto-raise-unfocus-delay* 300
  "Number of ms to delay before raising the unfocs proc for a raised window.
This can be overridden on a per-window basis using 
`set-auto-raise-unfocus-delay!'"
  #:type 'integer
  #:group 'focus
  #:range '(0 . 10000)
  #:favorites '(0 100 300 500 1000 2000 3000))

(define-scwm-option *default-auto-raise-focus-proc* raise-window
  "The default procedure to be used for auto-raising a window.
Can be overriden on a per-window basis using #:auto-raise-focus-proc window style,
or `set-auto-raise-focus-proc!'."
  #:type 'proc
  #:group 'focus)

(define-scwm-option *default-auto-raise-unfocus-proc* noop
  "The default procedure to be used for un-raising a window.
Can be overriden on a per-window basis using #:auto-raise-unfocus-proc window style,
or `set-auto-raise-unfocus-proc!'."
  #:type 'proc
  #:group 'focus)


(define*-public (set-auto-raise! auto-raise? #:optional (win (get-window)))
  "Turn auto-raise on (#t) or off (#f) for WIN.
Auto-raise makes a window automatically raise when the mouse pointer
enters the window frame.  See `set-auto-raise-delay!' for controlling
the delay before the window raises."
  (if win (set-object-property! win 'auto-raise auto-raise?)))

(define*-public (set-auto-raise-delay! delay #:optional (win (get-window)))
  "Set the auto-raise delay to DELAY (in ms) for WIN.
DELAY is the number of milliseconds after the pointer enters
WIN that WIN will be raised.  See `set-auto-raise!' to turn
auto-raise on or off for a given window."
  (if win (set-object-property! win 'auto-raise-delay delay)))

(define*-public (set-auto-raise-unfocus-delay! delay #:optional 
					       (win (get-window)))
  "Set the timeout to DELAY (in ms) for the unfocus-proc of WIN.
After DELAY milliseconds after the pointer leaves WIN's frame,
the auto-raise-unfocus-proc will be called."
  (if win (set-object-property! win 'auto-raise-unfocus-delay delay)))

(define*-public (set-auto-raise-focus-proc! fproc #:optional (win (get-window)))
  "Set the auto-raise-focus-proc for WIN.
The auto-raise-focus-proc is the procedure which is invoked
after the auto-raise-delay after the pointer enters WIN's frame."
  (if win (set-object-property! win 'auto-raise-focus-proc fproc)))

(define*-public (set-auto-raise-unfocus-proc! ufproc 
					     #:optional (win (get-window)))
  "Set the auto-raise-unfocus-proc for WIN.
The auto-raise-unfocus-proc is the procedure which is invoked
after the auto-raise-unfocus-delay after the pointer leaves WIN's frame."
  (if win (set-object-property! win 'auto-raise-unfocus-proc ufproc)))


(add-window-style-option #:auto-raise set-auto-raise!)
(add-window-style-option #:auto-raise-delay set-auto-raise-delay!)
(add-window-style-option #:auto-raise-unfocus-delay 
			 set-auto-raise-unfocus-delay!)
(add-window-style-option #:auto-raise-focus-proc set-auto-raise-focus-proc!)
(add-window-style-option #:auto-raise-unfocus-proc 
			 set-auto-raise-unfocus-proc!)


(define last-focus-handle '())
(define last-focus-window #f)

(define (make-auto-focus-func win)
  (let ((proc (or (object-property win 'auto-raise-focus-proc)
		  (optget *default-auto-raise-focus-proc*))))
    (lambda ()
      (proc win))))

(define (make-auto-unfocus-func win)
  (let ((proc (or (object-property win 'auto-raise-unfocus-proc)
		  (optget *default-auto-raise-unfocus-proc*))))
    (lambda ()
      (proc win))))


(define (auto-delay win)
  (or (object-property win 'auto-raise-delay)
      *default-auto-raise-delay*))

(define (auto-unfocus-delay win)
  (or (object-property win 'auto-raise-unfocus-delay)
      *default-auto-raise-unfocus-delay*))
  
(define (auto-raise-hook-proc window)
  (remove-timer-hook! last-focus-handle)
  (cond
   (window 
    (remove-timer-hook! 
     (object-property window 'window-last-unfocus-handle))
    (set-object-property! window 'window-last-unfocus-handle #f)
    (cond
     ((object-property window 'auto-raise)
      (let ((delay (auto-delay window)))
	(if (= delay 0)
	    ((make-auto-focus-func window))
	    (set! last-focus-handle 
		  (add-timer-hook! 
		   delay
		   (make-auto-focus-func window)))))))))
  (if (and last-focus-window (object-property last-focus-window 
					      'auto-raise))
	(let ((delay (auto-unfocus-delay last-focus-window))
	      (func (make-auto-unfocus-func last-focus-window)))
	  (if (= delay 0)
	      (func)
	      (set-object-property! last-focus-window 
				  'window-last-unfocus-handle
				  (add-timer-hook! delay func)))))
  (set! last-focus-window window))
	  
(add-hook! window-focus-change-hook auto-raise-hook-proc)

;; (popup-option-menu '*auto-raise*)
;; (scwm-option-name '*auto-raise*)
(define-scwm-option *auto-raise* #f
  "Whether to auto-raise windows"
  #:type 'boolean
  #:group 'focus
  #:setter (lambda (auto-raise?)
	     (if auto-raise? (window-style "*" #:auto-raise #t)
		 (if (provided? 'scwm-auto-raise) ; feature?
		     (begin
		       (use-modules (app scwm style))
		       (window-style "*" #:auto-raise #f))))
	     (set! *auto-raise* auto-raise?)))

;; GJB:FIXME:MS: Is there a better way to see if a module has been loaded?
(provide 'scwm-auto-raise)
