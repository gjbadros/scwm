;;;; $Id$
;;;; Copyright 1999 John Kodis, kodis@jagunet.com
;;;; The hover-focus module is based on auto-raise.scm, which is
;;;; Copyright (C) 1997-1998 Maciej Stachowiak and Greg J. Badros
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 2, or
;;;; (at your option) any later version.
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


(define-module (app scwm hover-focus)
  :use-module (app scwm flux)
  :use-module (app scwm defoption)
  :use-module (app scwm module-types)
  :use-module (app scwm optargs)
  :use-module (app scwm style-options) )


;;; hover-focus
;;;
;;; The hover-focus module provides a new window focus mechanism.  It
;;; allows a window to receive focus when the mouse pointer enters a
;;; window and remains ("hovers") over the window for a brief period
;;; of time.  The window can also be focused by clicking on the window
;;; just as with the "#:focus 'click" policy on which hover-focus is
;;; built.
;;;
;;; To make hover-focus the focus policy requires that hover-focus be
;;; included in a use-modules list, that the #:focus policy be set to
;;; 'click, and that the #:hover-focus option be set to #t.  The
;;; #:auto-raise option with no auto-raise-delay also works well with
;;; hover-focus.  A typical hover-focus window-style would begin:
;;;
;;; (window-style "*"
;;;    #:focus 'click
;;;    #:auto-raise #t #:auto-raise-delay 0
;;;    #:hover-focus #t  #:hover-focus-delay 350
;;;    ; plus other options to taste...
;;; )


;;; hover-focus-delay: default, setter, and style options

(define-scwm-option *default-hover-focus-delay* 300
  "Number of ms to delay before focusing the window that the pointer enters.
This can be overridden on a per-window basis using `set-hover-focus-delay!'."
  :type 'integer
  :group 'focus
  :range '(0 . 10000)
  :favorites '(0 100 300 500 1000 2000 3000))

(define*-public (set-hover-focus-delay! delay #&optional (win (get-window)))
  "Set the hover-focus delay to DELAY (in ms) for WIN.
DELAY is the number of milliseconds after the pointer enters
WIN that WIN will be focused.  See `set-hover-focus!' to turn
hover-focus on or off for a given window."
  (if win (set-object-property! win 'hover-focus-delay delay)))

(add-window-style-option #:hover-focus-delay set-hover-focus-delay!)


;;; hover-focus: setter and style options

(define*-public (set-hover-focus! hover-focus? #&optional (win (get-window)))
  "Turn hover-focus on (#t) or off (#f) for WIN.
hover-focus makes a window automatically get focus when the mouse pointer
remains in a window frame.  See `set-hover-focus-delay!' for controlling
the delay before the window gets focus."
  (if win (set-object-property! win 'hover-focus hover-focus?)))

(add-window-style-option #:hover-focus set-hover-focus!)


;;; private hover-focus variables and functions

(define hover-focus-timer '()) ; expires on hover delay expiration

;;; window-is-hoverable? tests a window's suitability for receiving
;;; focus when hovered over long enough.

(define (window-is-hoverable? window)
  (and window
       (not (iconified? window))
       (not (eq? (get-window-focus window) 'none))
       (object-property window 'hover-focus)))


;;; hover-focus-enter-proc runs whenever the mouse first enters a
;;; window.  It kills any active hover focus timer, and if the window
;;; is hoverable starts a timer that will focus the window if the timer
;;; expires.

(define (hover-focus-enter-proc window)
  (remove-timer-hook! hover-focus-timer)
  (cond ((window-is-hoverable? window)
        (set! hover-focus-window window)
        (let ((delay (* 1000 (or (object-property window 'hover-focus-delay)
                                 default-hover-focus-delay))))
          (set! hover-focus-timer
                (add-timer-hook! delay (lambda () (focus window))))))))

(add-hook! window-enter-hook hover-focus-enter-proc)


;;; hover-focus-leave-proc runs whenever the mouse pointer leaves a
;;; window.  It kills any active focus timer.

(define (hover-focus-leave-proc window)
  (remove-timer-hook! hover-focus-timer)
  (set! hover-focus-window #f))

(add-hook! window-leave-hook hover-focus-leave-proc)
