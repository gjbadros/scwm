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
  "Quote the next key event and let it pass to the application."
  (interactive)
  (let ((k (get-key-event)))
    (dynamic-wind
     (lambda () (apply undo-passive-grab (cdr k)))
     (lambda () (apply xtest-fake-modmask-key (cdr k)))
     (lambda () (apply redo-passive-grab (cdr k))))))

(define*-public (quote-mouse-event)
  "Quote the next mouse event and let it pass to the application."
  (interactive)
  (let ((m (get-mouse-event)))
    (dynamic-wind
     (lambda () (apply undo-passive-grab (cdr m)))
     (lambda () (apply xtest-fake-modmask-button (cdr m)))
     (lambda () (apply redo-passive-grab (cdr m))))))

(define*-public (quote-next-event)
  "Quote the next mouse or keyboard event and let it pass to the application."
  (interactive)
  (let ((m (get-next-event)))
    (if (is-mouse-event? m)
	(dynamic-wind
	 (lambda () (apply undo-passive-grab (cdr m)))
	 (lambda () (apply xtest-fake-modmask-button (cdr m)))
	 (lambda () (apply redo-passive-grab (cdr m))))
	(dynamic-wind
	 (lambda () (apply undo-passive-grab (cdr m)))
	 (lambda () (apply xtest-fake-modmask-key (cdr m)))
	 (lambda () (apply redo-passive-grab (cdr m)))))))

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
		 (,mod-mask-alt . "Alt_L")
		 (,mod-mask-hyper . "Hyper_L")
		 (,mod-mask-super . "Super_L")
		 (,mod-mask-shift . "Shift_L")
		 (,mod-mask-meta . "Meta_L"))))
    (for-each (lambda (item)
		(let ((modifier-value ((car item)))
		      (keysym (cdr item)))
		  (if (and modifier-value (not (= (logand mask modifier-value) 0)))
		      (set! answer 
			    (cons (car (keysym->keycode keysym)) answer)))))
	      table)
    answer))


(define-public (xtest-fake-modmask mask delay press?)
  (for-each 
   (lambda (keycode)
     (xtest-fake-key-event keycode press? delay))
   (mask->keycodes mask)))

(define*-public (xtest-fake-modmask-key mask keycode #&optional (delay #f))
  (dynamic-wind
   (lambda () (xtest-fake-modmask mask delay #t))
   (lambda () 
     (xtest-fake-key-event keycode #t delay)
     (xtest-fake-key-event keycode #f delay))
   (lambda () (xtest-fake-modmask mask delay #f))))

(define*-public (xtest-fake-modmask-button mask button #&optional (delay #f))
  (dynamic-wind
   (lambda () (xtest-fake-modmask mask delay #t))
   (lambda () 
     (xtest-fake-button-event button #t delay)
     (xtest-fake-button-event button #f delay))
   (lambda () (xtest-fake-modmask mask delay #f))))
