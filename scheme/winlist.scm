;;;; $Id$
;;;; Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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



(define-module (app scwm winlist)
  :use-module (app scwm optargs)
  :use-module (app scwm wininfo)
  :use-module (app scwm base)
  :use-module (app scwm animated-iconify)
  :use-module (app scwm menus-extras)
  :use-module (app scwm style-options)
  :use-module (app scwm flash-window)
  :use-module (app scwm listops))



(define-public (listify-if-atom l)
  "Make L into a list if it is not."
  (if (or (pair? l) (null? l)) l (list l)))

(define-public (filter-only-except l only except)
  "Return a filtered list of the elements from L.
The returned list will contain only elements from L
that have the predicate ONLY accepts but EXCEPT
does not accept."
  (filter (lambda (item)
	    (and
	     (and-map (lambda (pred) (pred item)) 
		      (listify-if-atom only))
	     (not (or-map (lambda (pred) (pred item)) 
			  (listify-if-atom except)))))
	  l))

(define local-reverse reverse)

(define*-public (list-windows #&key (only '()) (except '())
			      (by-stacking #f)
			      (by-focus #f)
			      (reverse #f))
  "Return the list of windows matching ONLY and not matching EXCEPT.
The windows are returned their stacking order (top first) if
BY-STACKING is #t (and that option omits iconified windows).
They are returned sorted by their last focussed
time (most recently focussed first) if BY-FOCUS is #t. If REVERSE is
true, they are returned in the reverse of the usual order. ONLY and
EXCEPT each are procedures which take a single window argument and
returns #t if the window should be included (for ONLY) or 
excluded (for EXCEPT), or #f otherwise."
  ((if reverse local-reverse id)
   (filter-only-except 
    (if by-stacking
	(list-stacking-order)
	(if by-focus
	    (list-focus-order)
	    (list-all-windows))) only except)))

(define*-public (winlist-hit #&optional (win (get-window)))
  "Permit WIN to be displayed in the window list by default.
This resets the 'winlist-skip property of WIN.  See also `winlist-skip'."
  (if win (set-object-property! win 'winlist-skip #f)))

(define*-public (winlist-skip #&optional (win (get-window)))
  "Do not show WIN in the window list by default.
This sets the 'winlist-skip property of WIN.  See also `winlist-hit'."
  (if win (set-object-property! win 'winlist-skip #t)))

(define*-public (winlist-skip? #&optional (win (get-window)))
  "Return #t if WIN is skipped in the window list, #f otherwise."
  (if win (object-property win 'winlist-skip) #f))

;; add style options for #:winlist-skip
(add-boolean-style-option #:winlist-skip winlist-skip winlist-hit)


(define*-public (circulate-hit #&optional (win (get-window)))
  "Include WIN among the windows in the circulate list.
This resets the 'circulate-skip property of WIN.  See also `circulate-skip'."
  (if win (set-object-property! win 'circulate-skip #f)))

(define*-public (circulate-skip #&optional (win (get-window)))
  "Do not include WIN among the windows in the circulate list.
This sets the 'circulate-skip property of WIN.  See also `circulate-hit'."
  (if win (set-object-property! win 'circulate-skip #t)))

(define*-public (circulate-skip? #&optional (win (get-window)))
  "Return #t if WIN is not among the windows in the circulate list.
Otherwise return #f."
  (if win (object-property win 'circulate-skip) #f))

(define*-public (circulate-hit-icon #&optional (win (get-window)))
  "Include WIN's icon among the windows in the circulate list.
This resets the 'circulate-skip-icon property of WIN.  
See also `circulate-skip-icon'."
  (if win (set-object-property! win 'circulate-skip-icon #f)))

(define*-public (circulate-skip-icon #&optional (win (get-window)))
  "Do not include WIN's icon among the windows in the circulate list.
This sets the 'circulate-skip-icon property of WIN.  
See also `circulate-hit-icon'."
  (if win (set-object-property! win 'circulate-skip-icon #t)))

(define*-public (circulate-skip-icon? #&optional (win (get-window)))
  "Return #t if WIN's icon is not among the windows in the circulate list.
Otherwise return #f."
  (if win (object-property win 'circulate-skip-icon) #f))

;; CRW:FIXME:MS: Shouldn't this be:
;; ((if (iconified-window? win) circulate-skip? circulate-skip-icon?) win)
(define*-public (should-circulate-skip? #&optional (win (get-window)))
  "Return #t if WIN should now be skipped when circulating, #f otherwise.
Uses the current state of WIN (whether it is iconified or not) in
determining the result."
  (if win 
      (or (circulate-skip? win) 
	  (and (iconified-window? win) (circulate-skip-icon? win)))
      #f))

;; add style options for #:circulate-skip and #:circulate-skip-icon
(add-boolean-style-option #:circulate-skip circulate-skip circulate-hit)
(add-boolean-style-option #:circulate-skip-icon 
			  circulate-skip-icon circulate-hit-icon)

;; GJB:FIXME:MS: What the heck does this do?  Why does it remove the element, too?
;; (rotate-around 3 '(1 2 3 4 5)) => (4 5 1 2) --09/19/99 gjb
(define (rotate-around w wl)
  (append (cond
	   ((memq w wl) => cdr)
	   (else wl))
	  (cond
	   ((memq w (reverse wl)) 
	    => (lambda (x)
		 (reverse (cdr x))))
	   (else '()))))

;; MS:FIXME:: these docs may not be the greatest... can you
;; double check and document circulate a bit better? --09/05/98 gjb
(define last-circulated #f)

(define (circulate backwards? window only except proc)
  (let* ((window (or window last-circulated))
	 (wl (if window
		 ((if backwards? reverse id)
		  (rotate-around window (list-all-windows)))
		 (list-all-windows))))
    (cond
     ((filter-only-except wl only (cons
				   should-circulate-skip? 
				   (listify-if-atom except)))
      => (lambda (x) 
	   (cond
	    ((pair? x)
	     (set! last-circulated (car x))
	     (proc (car x)))))))))

(define*-public (next-window #&key (window (get-window #f #f))
			     (only '()) (except '()) (proc window-list-proc))
  "Switch to the next matching window.
If WINDOW is given, switch to that window.
ONLY and EXCEPT control which windows match --- see `list-windows' for 
details.
PROC is a procedure of one argument which does the work after the
window list is re-ordered.  PROC defaults to `window-list-proc'.
By specifiying, e.g., \"#:proc focus-change-warp-pointer\" the
new window will be raised, focussed, and the pointer will be warped
to the window.
See also `prev-window'."
  (circulate #f window only except proc))


(define*-public (prev-window #&key (window (get-window #f #f))
			     (only '()) (except '()) (proc window-list-proc))
  "Circulate to the previous matching window.
If WINDOW is given, circulate to that window.
ONLY and EXCEPT control which windows match --- see `list-windows' for 
details.
PROC is a procedure of one argument which does the work after the
windows are circulated.  PROC defaults to `window-list-proc'.
See also `next-window'."
  (circulate #t window only except proc))
