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

;;; The list of undo states
(define-public undo-list '())
;;; The number of elements in undo-list (i.e. (length undo-list))
(define-public undo-num-entries 0)
;;; The maximum number of undo entries to keep.
(define-public undo-max-entries 100)
;;; The index in undo-list of the state to copy on the next undo
(define-public undo-index 0)

;;; SRL:FIXME:: Change so it doesn't use negative logic as double negatives are
;;;   confusing.
(define last-not-redo #t)

(define*-public (reset-undo!)
  "Reset the undo system.
Clears the list of states."
  (interactive)
  (set! undo-list '())
  (set! undo-num-entries 0)
  (set! undo-index 0)
  (set! last-not-redo #t))

(define-public (truncate-undo-list num-entries)
  "Truncate 'undo-list' such that it has no more than NUM-ENTRIES."
  (let ((extra-entries (- undo-num-entries num-entries)))
    (if (> extra-entries 0)
        (begin 
          (if (> num-entries 0)
              (set-cdr! (list-tail undo-list (- num-entries 1)) '())
              (set! undo-list '()))
          (set! undo-num-entries num-entries)))))

;;; high is inside range
(define-public (increment-in-range v high)
  "Return v+1 but no more than high."
  (set! v (+ v 1))
  (if (> v high) high v))

;;; low is inside range
(define-public (decrement-in-range v low)
  "Return v-1 but no less than low."
  (set! v (- v 1))
  (if (< v low) low v))

(define-public (decrement-undo-index)
  "DEPRECATED: Decrement the undo-index.
Will become a private function."
  (set! undo-index (decrement-in-range undo-index 1)))

(define-public (increment-undo-index)
  "DEPRECATED: Increment the undo-index.
Will become a private function."
  (set! undo-index (increment-in-range undo-index undo-num-entries)))

(define*-public (push-undo-global)
  "Push the global state onto the undo list.
See also 'push-undo-state' and 'global-window-configuration'."
  (interactive)
  (push-undo-state (global-window-configuration)))

(define*-public (insert-undo-global)
  "DPRECATED: BROKEN: Same as 'push-undo-global'.
Was documented as: Insert the global state into the undo list at undo-index.
This function does not work as previously documented and has
bizarre semantics which seem useless and therefor will not be
fixed and is deprecated."
  (interactive)
  (insert-undo-state (global-window-configuration)))

;;; SRL:FIXME:: This shouldn't assume that global window configurations
;;;   have this particular format.
(define-public (push-undo-state state)
  "Push STATE onto the undo list.
STATE should be a list of window configurations to save in stacking order
from top to bottom.  See also 'push-undo-global'."
  (set! undo-list (cons state undo-list))
  (set! undo-num-entries (+ undo-num-entries 1))
  (truncate-undo-list undo-max-entries)
  (set! undo-index 0)
  (set! last-not-redo #t))

(define-public (insert-undo-state state)
  "DEPRECATED: BROKEN: Same as 'push-undo-state'
Was documented as:Insert STATE into the undo list at undo-index.
This function does not work as previously documented and has
bizarre semantics which seem useless and therefor will not be
fixed and is deprecated."
  (if (<= 0 undo-index)
      (push-undo-state state)
      (begin
	(set-cdr! (list-ref undo-list (- undo-index 1)) 
		  (cons state (list-tail undo-list undo-index)))
	(set! undo-num-entries (+ undo-num-entries 1)))))

(define-public (undo-state-at-index index)
  "Return the state from undo-list at INDEX, or #f if bad index.
INDEX of 0 means the state before the last undoable action (i.e. the state
as of the last time push-undo-global was called).  Each additonal 1 added
to INDEX goes back one more undoable operation.  See also 'undo-use'."
  (if (and (>= index 0) (< index undo-num-entries))
      (list-ref undo-list index)
      #f))

(define*-public (undo)
  "Undo the last operation that was undoable.
Undoable operations save changed state using `push-undo-global'
before they perform their action.  Undo is an undoable action
if 'undo' or 'push-undo-state' was called since the last 'redo'."
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
  "Redo the last undone operation."
  (interactive)
  (set! last-not-redo #f)
  (decrement-undo-index)
  (let ((state (undo-state-at-index (- undo-index 1))))
    (if state
	(restore-global-window-configuration state)))
  undo-index)

(define-public (undo-use index)
  "Apply the state from the undo-list contained at INDEX.
INDEX of 0 means the state before the last undoable action (i.e. the state
as of the last time push-undo-global was called).  Each additonal 1 added
to INDEX goes back one more undoable operation.  See also
'undo-state-at-index'."
  (let ((state (undo-state-at-index index)))
    (if state
	(restore-global-window-configuration state)))
  index)

;; (reset-undo!)
;; (push-undo-global)
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
