;;;; $Id$
;;;; Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
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
  :use-module (app scwm menus-extras)
  :use-module (app scwm style-options)
  :use-module (app scwm listops))



(define*-public (default-winlist-proc #&optional (win (get-window)))
  "The default behaviour when WIN is selected from the window list."
  (cond
   (win (deiconify win)
	(focus win)
	(raise-window win)
	(warp-to-window win)
	(move-pointer (w%x 20 win) (w%y 20 win)))))

(define (listify-if-atom l)
  (if (or (pair? l) (null? l)) l (list l)))

(define-public window-list-proc default-winlist-proc)

(define (filter-only-except l only except)
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
BY-STACKING is #t.  They are returned sorted by their last focussed
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

(define*-public (show-window-list-menu #&key (only '()) (except '())
				       (by-stacking #f)
				       (by-focus #f)
				       (by-resource #f)
				       (reverse #f)
				       (proc window-list-proc)
				       (flash-window-proc #f)
				       (unflash-window-proc #f)
				       (hover-delay #f)
				       (popup-delay #f)
				       (show-geometry #f)
				       (show-last-focus-time #f)
				       (warp-to-first #f)
				       (ignore-winlist-skip #f)
				       (show-mini-icon #t))

  "Popup a window list menu and permit a selection to be made.
ONLY and EXCEPT are procedures that control which windows will appear
in the list -- see `list-windows' for details. BY-STACKING, BY-FOCUS
and REVERSE control the order in which windows appear. See
`list-windows' for more on these as well.

PROC is the procedure which will be called on the selected window. 

FLASH-WINDOW-PROC and UNFLASH-WINDOW-PROC are set as the hover-action
and unhover-action (respectively) of the items in the menu.  (See
`menuitem'.)

If SHOW-GEOMETRY is #t, the geometries of the windows will be listed
in each menuitem.  

If SHOW-LAST-FOCUS-TIME is #t, the last focus time of the windows will be listed
in each menuitem.

If SHOW-MINI-ICON is #t, the mini-icon of the windows will be
displayed with each menuitem.

If WARP-TO-FIRST is #t, the mouse pointer will be warped to the first
menuitem (see `popup-menu').  

If BY-RESOURCE is #t, the window list is split into sublists by the
window resource name (this is also the behaviour if too many windows
exist to fit vertically on the menu).
"
  (let* 
      ((lw (list-windows #:only only #:except 
			 (if ignore-winlist-skip
			     except
			     (cons 
			      winlist-skip?
			      (listify-if-atom except)))
			 #:by-stacking by-stacking
			 #:by-focus by-focus
			 #:reverse reverse))
       (split-by-resource (or by-resource (> (length lw) (menu-max-fold-lines))))
       (menuitems-with-window-resource
	((if split-by-resource sorted-by-car-string noop)
	 (map (lambda (x)
		(let ((extra-label-string 
		       (if show-geometry (window-geometry-string x) #f)))
		  (if show-last-focus-time
		      (set! extra-label-string 
			    (string-append (or extra-label-string "")
					   (if extra-label-string ", " "")
					   (window-last-focus-time-string x))))
		  (cons (window-resource x)
			(make-menuitem (window-title x)
				       (lambda () (proc x))
				       extra-label-string
				       #f 
				       (if show-mini-icon
					   (window-mini-icon x) #f)
				       (if flash-window-proc
					   (lambda () (flash-window-proc x))
					   #f)
				       (if unflash-window-proc
					   (lambda () (unflash-window-proc x))
					   #f)
				       #f))))
	      lw))))
    (popup-menu (menu
		 (append 
		  (list 
		   (make-menuitem "Window list" #f (if show-geometry "Geometry" #f)
				  #f #f #f #f #f)
		   menu-title)
		  (if split-by-resource
		      (fold-menu-list-by-group menuitems-with-window-resource)
		      (map (lambda (x) (cdr x)) menuitems-with-window-resource)))
		 #:popup-delay popup-delay
		 #:hover-delay hover-delay)
		warp-to-first)))
  
(define (rotate-around w wl)
  (append (cond
	   ((memq w wl) => cdr)
	   (else wl))
	  (cond
	   ((memq w (reverse wl)) 
	    => (lambda (x)
		 (reverse (cdr x))))
	   (else '()))))


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
;; ((if (iconified? win) circulate-skip? circulate-skip-icon?) win)
(define*-public (should-circulate-skip? #&optional (win (get-window)))
  "Return #t if WIN should now be skipped when circulating, #f otherwise.
Uses the current state of WIN (whether it is iconified or not) in
determining the result."
  (if win 
      (or (circulate-skip? win) (and (iconified? win) (circulate-skip-icon? win)))
      #f))

;; add style options for #:circulate-skip and #:circulate-skip-icon
(add-boolean-style-option #:circulate-skip circulate-skip circulate-hit)
(add-boolean-style-option #:circulate-skip-icon 
			  circulate-skip-icon circulate-hit-icon)

;; MSFIX: these docs may not be the greatest... can you
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
  "Circulate to the next matching window.
If WINDOW is given, circulate to that window.
ONLY and EXCEPT control which windows match --- see `list-windows' for 
details.
PROC is a procedure of one argument which does the work after the
windows are circulated.  PROC defaults to `window-list-proc'.
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
