;;;; $Id$
;;;; quote.scm
;;;; Copyright (C) 1999, 2000 Greg J. Badros
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


;;; (use-scwm-modules quote)
;;; (use-scwm-modules scwmxtest)
(define-module (app scwm quote)
  :use-module (app scwm optargs)
  :use-module (app scwm describe)
  :use-module (app scwm scwmxtest))



;;; These are the two main features of this module:
;;; `quote-key-event' and `quote-mouse-event'

(define*-public (quote-key-event)
  "BROKEN: Quote the next key event and let it pass to the application.
This is broken since it relies on 'undo-passive-grab' which is currently broken
for keys specified with the context 'all."
  (interactive)
  (let ((k (get-key-event)))
    (if (lookup-key 'window (car k))
        (dynamic-wind
         (lambda () (apply undo-passive-grab      (cdr k)))
         (lambda () (apply xtest-fake-modmask-key (cdr k)))
         (lambda () (apply redo-passive-grab      (cdr k))))
        (apply xtest-fake-modmask-key (cdr k)))))

(define*-public (quote-mouse-event)
  "BROKEN: Quote the next mouse event and let it pass to the application.
Note: Dragging motions are mutliple X events and therefore are not quoted
correctly by this function.  Broken for mouse events specified via the
context 'all."
  (interactive)
  (let ((m (get-mouse-event)))
    (if (lookup-mouse 'window (car m))
        (dynamic-wind
         (lambda () (apply undo-passive-grab         (cdr m)))
         (lambda () (apply xtest-fake-modmask-button (cdr m)))
         (lambda () (apply redo-passive-grab         (cdr m))))
        (apply xtest-fake-modmask-button (cdr m)))))

(define*-public (quote-next-event)
  "BROKEN: Quote the next mouse or keyboard event and let it pass to the application.
See 'quote-key-event' and 'quote-mouse-event' for details."
  (interactive)
  (let ((m (get-next-event)))
    (if (is-mouse-event? m)
	(dynamic-wind
	 (lambda () (apply undo-passive-grab         (cdr m)))
	 (lambda () (apply xtest-fake-modmask-button (cdr m)))
	 (lambda () (apply redo-passive-grab         (cdr m))))
	(dynamic-wind
	 (lambda () (apply undo-passive-grab      (cdr m)))
	 (lambda () (apply xtest-fake-modmask-key (cdr m)))
	 (lambda () (apply redo-passive-grab      (cdr m)))))))

;; (bind-key 'all "H-q" quote-key-event)
;; (quote-key-event)
;; (quote-mouse-event)
;; (get-mouse-event)



;; quote
;; (mask->keycodes 13)

(define-public (mask->keycodes mask)
  "Return a list of keycodes corresponding to keys that generate the modifiers in MASK."
  (let ((answer '())
	(table `((,mod-mask-control . "Control_L")
		 (,mod-mask-alt     . "Alt_L")
		 (,mod-mask-hyper   . "Hyper_L")
		 (,mod-mask-super   . "Super_L")
		 (,mod-mask-shift   . "Shift_L")
		 (,mod-mask-meta    . "Meta_L"))))
    (for-each (lambda (item)
		(let ((modifier-value ((car item)))
		      (keysym (cdr item)))
		  (if (and modifier-value (not (= (logand mask modifier-value) 0)))
		      (set! answer 
			    (cons (car (keysym->keycode keysym)) answer)))))
	      table)
    answer))


(define-public (xtest-fake-modmask mask delay press?)
  "Send key events for the modifier mask to MASK with DELAY before each key.
PRESS? indicates whether the keys should be pressed or released.
In almost all cases, a call with (eq press? #f) should follow one with
(eq press? #t)."
  (for-each 
   (lambda (keycode)
     (xtest-fake-key-event keycode press? delay))
   (mask->keycodes mask)))

(define*-public (xtest-fake-modmask-key mask keycode #:optional (delay #f))
  "Send key events to simulate the pressing of KEYCODE with MASK active.
KEYCODE is the key code of the key to fake pressing.
MASK is a modifier mask specifying which modifier keys to press before
pressing KEYCODE and release after pressing it.
DELAY is a delay to wait before each simulated key press/release.  Default
is #f for no delay."
  (dynamic-wind
   (lambda () (xtest-fake-modmask mask delay #t))
   (lambda () 
     (xtest-fake-key-event keycode #t delay)
     (xtest-fake-key-event keycode #f delay))
   (lambda () (xtest-fake-modmask mask delay #f))))

(define*-public (xtest-fake-modmask-button mask button #:optional (delay #f))
  "Send key/button events to simulate the pressing of BUTTON with MASK active.
BUTTON is the button number of the button to fake pressing.
MASK is a modifier mask specifying which modifier keys to press before
pressing BUTTON and release after pressing it.
DELAY is a delay to wait before each simulated key/button press/release.  Default
is #f for no delay."
  (dynamic-wind
   (lambda () (xtest-fake-modmask mask delay #t))
   (lambda () 
     (xtest-fake-button-event button #t delay)
     (xtest-fake-button-event button #f delay))
   (lambda () (xtest-fake-modmask mask delay #f))))
