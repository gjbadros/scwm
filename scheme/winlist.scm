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
  :use-module (app scwm flash-window)
  :use-module (app scwm listops))



  
;; warp-placement is from Ken Pizzini
(define-public (warp-placement win)
  "Return a list, (%x %y), for the desired pointer placement for WIN.
The percentages are of the window size, and are gotten by using
the 'warp-placement object-property of WIN;  they default to (20 20)
if no such property is set. To change the default for all your windows
you can do something like:
  (add-hook! after-new-window-hook 
    (lambda (win) 
      (set-object-property! win 'warp-placement '(80 20))))"
  (let ((p (object-property win 'warp-placement))) 
    (if (and p (pair? p) (= (length p) 2) (number? (car p)) (number? (cadr p)))
	p '(20 20))))

;;(set-object-property! (select-window-interactively) 'warp-placement '(80 25))
;;(warp-placement (select-window-interactively))

(define*-public (focus-change-warp-pointer #&optional (win (get-window)))
  "Deiconify, focus, raise, and warp-to WIN.
This is initially the default behaviour when WIN is selected from the window list."
  (cond
   (win (deiconify win)
	(focus win)
	(raise-window win)
	(warp-to-window win)
	(let ((p (warp-placement win))) 
	  (move-pointer (w%x (car p) win) (w%y (cadr p) win))))))

(define (listify-if-atom l)
  (if (or (pair? l) (null? l)) l (list l)))

(define-public window-list-proc focus-change-warp-pointer)

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

(define*-public (make-window-list-menu #&key (only '()) (except '())
				       (by-stacking #f)
				       (by-focus #f)
				       (by-resource #f)
				       (reverse #f)
				       (proc window-list-proc)
				       (flash-window-proc flash-window-on)
				       (unflash-window-proc unflash-window)
				       (hover-delay 0)
				       (popup-delay #f)
				       (show-geometry #f)
				       (show-last-focus-time #f)
				       (ignore-winlist-skip #f)
				       (show-mini-icon #t)
				       (enumerate-hotkeys #t))
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

If ENUMERATE-HOT-KEYS is #t, then add alpha-numeric hot keys for the window-list.
For the hotkey, the characters 1 through 9 are used first, followed by
the letters a through z.  Currently this is turned off if BY-RESOURCE is #t.
"
  (if by-resource (set! enumerate-hotkeys #f))
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
;;       (foo-just-for-printing (begin (display lw)  (newline)))
       (hotkeys "1234567890abcdefghijklmnopqrstuvwxyz")
       (hk-len (string-length hotkeys))
       (count -1)
       (split-by-resource (or by-resource (> (length lw) (menu-max-fold-lines))))
       (menuitems-with-window-resource
	((if split-by-resource sorted-by-car-string noop)
	 (map (lambda (x)
		(set! count (+ count 1))
		(let* ((extra-label-string 
			(if show-geometry (window-geometry-string x) #f))
		       (hotkey (cond 
				((not enumerate-hotkeys) "")
				((< count hk-len) (substring hotkeys count (+ count 1)))
				(else "")))
		       (item-label (if enumerate-hotkeys 
				       (string-append hotkey ".\t" (window-title x))
				       (window-title x))))
		  (if show-last-focus-time
		      (set! extra-label-string 
			    (string-append (or extra-label-string "")
					   (if extra-label-string ", " "")
					   (window-last-focus-time-string x))))
		  (cons (window-resource x)
			(make-menuitem item-label
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
				       (if enumerate-hotkeys
					   hotkey #f)))))
	      lw))))
;;    (display menuitems-with-window-resource) (newline)
    (menu
     (append 
      (list 
       (menu-title
	(if enumerate-hotkeys
	    "	Window list"
	    "Window list") #:extra-label (if show-geometry "Geometry" #f))
       menu-separator)
      (if split-by-resource
	  (fold-menu-list-by-group menuitems-with-window-resource 
				   #:hover-delay hover-delay
				   #:popup-delay popup-delay)
	  (map (lambda (x) (cdr x)) menuitems-with-window-resource)))
     #:popup-delay popup-delay
     #:hover-delay hover-delay)))


(define*-public (show-window-list-menu warp-to-index permit-alt-release-selection? #&rest rest)
  "Popup a window list menu.
Warps the pointer to the first menu item iff WARP-TO-FIRST is #t.
Accepts all keyword arguments that `make-window-list-menu' takes."
  (popup-menu (apply make-window-list-menu rest) warp-to-index #f #f #f permit-alt-release-selection?))
  
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


(define*-public (select-window-from-window-list #&key
						(only '()) (except '())
						(ignore-winlist-skip #f))
  "Permit selecting a window from a window list.
Return the selected window object, or #f if none was selected"
  (show-window-list-menu #f #f 
			 #:only only #:except except
			 #:flash-window-proc
			 (lambda (w) (flash-window w #:unflash-delay #f))
			 #:unflash-window-proc
			 (lambda (w) (unflash-window w))
			 #:hover-delay 0
			 #:ignore-winlist-skip ignore-winlist-skip #:proc (lambda (w) w)))

;; e.g.
;; (let ((w (select-window-from-window-list #:only iconified?)))
;;  (deiconify w) (move-to 0 0 w))
;; (select-window-from-window-list)
;; (unflash-window (get-window))


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

