;;;; 	Copyright (C) 1997 Maciej Stachowiak
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
;;;;    #:auto-raise-focus-proc 
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

(define*-public (set-auto-raise! auto-raise? #&optional (w (get-window)))
  (if w (set-object-property! w 'auto-raise auto-raise?)))

(define*-public (set-auto-raise-delay! delay #&optional (w (get-window)))
  (if w (set-object-property! w 'auto-raise-delay delay)))

(define*-public (set-auto-raise-unfocus-delay! delay #&optional 
					       (w (get-window)))
  (if w (set-object-property! w 'auto-raise-unfocus-delay delay)))

(define*-public (set-auto-raise-focus-proc! fproc #&optional (w (get-window)))
  (if w (set-object-property! w 'auto-raise-focus-proc fproc)))

(define*-public (set-auto-raise-unfocus-proc! ufproc 
					     #&optional (w (get-window)))
  (if w (set-object-property! w 'auto-raise-unfocus-proc ufproc)))


(add-window-style-option #:auto-raise set-auto-raise!)
(add-window-style-option #:auto-raise-delay set-auto-raise-delay!)
(add-window-style-option #:auto-raise-unfocus-delay 
			 set-auto-raise-unfocus-delay!)
(add-window-style-option #:auto-raise-focus-proc set-auto-raise-focus-proc!)
(add-window-style-option #:auto-raise-unfocus-proc 
			 set-auto-raise-unfocus-proc!)


(define-public default-auto-raise-delay 300)
(define-public default-auto-raise-focus-proc raise-window)
(define-public default-auto-raise-unfocus-proc noop)

(define last-focus-handle '())
(define last-focus-window #f)

(define (make-auto-focus-func win)
  (let ((proc (or (object-property win 'auto-raise-focus-proc)
		  default-auto-raise-focus-proc)))
    (lambda ()
      (proc win))))

(define (make-auto-unfocus-func win)
  (let ((proc (or (object-property win 'auto-raise-unfocus-proc)
		  default-auto-raise-unfocus-proc)))
    (lambda ()
      (proc win))))


(define (auto-delay win)
  (* 1000 
     (or (object-property win 'auto-raise-delay)
	 default-auto-raise-delay)))

(define (auto-unfocus-delay win)
  (* 1000 
     (or (object-property win 'auto-raise-unfocus-delay)
	 (object-property win 'auto-raise-delay)
	 default-auto-raise-delay)))
  
(define (auto-raise-hook-proc event num-datum a1 a2 a3 a4 a5 a6 a7)
  (cond
   ((= event M_FOCUS_CHANGE)
    (remove-timer-hook! last-focus-handle)
    (let ((window (window-from-window-id a1)))
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
		       (make-auto-focus-func window)))))))
	(if (and last-focus-window (object-property window 'auto-raise))
	  (let ((delay (auto-unfocus-delay window))
		(func (make-auto-unfocus-func last-focus-window)))
	    (if (= delay 0)
		(func)
		(set-object-property! last-focus-window 
				      'window-last-unfocus-handle
				      (add-timer-hook! delay func)))
	(set! last-focus-window window)))))))))
	  

(add-hook! broadcast-hook auto-raise-hook-proc)
