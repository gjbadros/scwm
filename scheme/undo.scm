;;;; $Id$
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


(define-module (app scwm undo)
  :use-module (app scwm wininfo)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm window-configuration)
  :use-module (app scwm winlist))

(define-public undo-list '())
(define-public undo-num-entries 0)
(define-public undo-max-entries 100)
(define-public undo-index 0)

(define*-public (reset-undo!)
  "Reset the undo system.
Clears the list of states."
  (interactive)
  (set! undo-list '())
  (set! undo-num-entries 0)
  (set! undo-index 0))

;; high is inside range
(define-public (increment-in-range v high)
  "Return v+1 but no more than high."
  (set! v (+ v 1))
  (if (> v high) (- high 1) v))

;; low is inside range
(define-public (decrement-in-range v low)
  "Return v-1 but no less than low."
  (set! v (- v 1))
  (if (< v low) low v))

(define-public (decrement-undo-index)
  "Decrement the undo-index."
  (set! undo-index (decrement-in-range undo-index 0)))

(define-public (increment-undo-index)
  "Increment the undo-index."
  (set! undo-index (increment-in-range undo-index undo-num-entries)))

(define*-public (push-undo-global)
  "Push the global state onto the undo list.
See also `insert-undo-global' which honours
the current position in the undo-list, undo-index."
  (interactive)
  (push-undo-state (global-window-configuration)))

(define*-public (insert-undo-global)
  "Insert the global state into the undo list at undo-index.
See also `push-undo-global'."
  (interactive)
  (insert-undo-state (global-window-configuration)))

(define-public (push-undo-state state)
  "Push STATE onto the undo list."
  (set! undo-list (cons state undo-list))
  (set! undo-num-entries (+ undo-num-entries 1))
  (set! undo-index 0))

(define-public (insert-undo-state state)
  "Insert STATE into the undo list at undo-index."
  (if (<= 0 undo-index)
      (push-undo-state state)
      (begin
	(set-cdr! (list-ref undo-list (- undo-index 1)) 
		  (cons state (list-tail undo-list undo-index)))
	(set! undo-num-entries (+ undo-num-entries 1)))))

(define-public (undo-state-at-index index)
  "Return the state from undo-list at INDEX, or #f if bad index."
  (if (and (>= index 0) (< index undo-num-entries))
      (list-ref undo-list index)
      #f))

(define last-not-redo #t)

(define*-public (undo)
  "Undo the last operation that was undoable.
Undoable operations save changed state using `insert-undo-global'
before they perform their action."
  (interactive)
  (if (and last-not-redo (= 0 undo-index))
      (begin (push-undo-global)
	     (set! undo-index 1))) ;; save the state to redo the undo
  (set! last-not-redo #t)
  (let ((state (undo-state-at-index undo-index)))
    (increment-undo-index)
    (if state
	(restore-global-window-configuration state)))
  undo-index)

(define*-public (redo)
  "Redo the last undone operation.
This re-applyiesthe state at undo-index - 1."
  (interactive)
  (if (= undo-num-entries undo-index)
      (decrement-undo-index))  ;; do not redo the same state we just undid
  (set! last-not-redo #f)
  (decrement-undo-index)
  (let ((state (undo-state-at-index undo-index)))
    (if state
	(restore-global-window-configuration state)))
  undo-index)

(define-public (undo-use index)
  "Apply the state from the undo-list contained at INDEX."
  (let ((state (undo-state-at-index index)))
    (if state
	(restore-global-window-configuration state)))
  index)

;; (reset-undo!)
;; (push-undo-global)
;; (insert-undo-global)
;; (undo)
;; (redo)
;; undo-num-entries
;; undo-index
;; (undo)
;; (redo)
;; (undo-use 0)
;; (undo-use 1)
;; (undo-use 2)
;; (undo-use 3)
;; (undo-use 4)
