;;;; $Id$
;;;; Copyright (C) 1998-1999 Maciej Stachowiak
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



(define-module (app scwm tile)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm flash-window)
  :use-module (app scwm wininfo)
  :use-module (app scwm undo)
  :use-module (app scwm winlist)
  :use-module (app scwm window-selection)
  :use-module (app scwm rectangle)
  :use-module (app scwm winops)
  :use-module (app scwm animation))

(define (integer-step start range steps step)
  (inexact->exact 
   (round (+ start (/ (* range step) steps)))))

(define*-public (tile-windows
		 windows #&key (start-pos '(0 0)) 
		 (end-pos (display-size)) (resize 'always)
		 (raise 'restack-only)
		 (max-windows #f)
		 (order 'horizontal))
  "Tile WINDOWS according to several parameters.  

Tiling places the windows according to a grid from START-POS, (0 0) by
default, to END-POS, the size of the display by default. 

If MAX-WINDOWS is specified and not #f, at most MAX-WINDOWS elements
of WINDOWS will be operated on.

If ORDER is 'horizontal, the windows will be tiled in row-major order;
if it is 'vertical they will be tiled in column-major oder.

RESIZE may be #f, 'shrink-only or 'always, indicating that the windows
should never be resized, that they should be resized to the max-size
only if they are bigger, or that they should always be resized to the
max size. The default is 'shrink-only.

RAISE may be #f, #t, or 'restack-only, indicating that the windows
should not be moved in the stacking order; that they should be raised
on top of other windows and placed in the tile order with the upper
left window lowest in the stacking order; or that they should be
restacked as for #t but not raised above other windows,
respectively. The default is 'restack-only."
    (let* ((num-windows (if max-windows
			    (min (length windows) max-windows)
			    (length windows)))
	   (windows (list-head windows num-windows)))
      (cond 
       ((not (null? windows))
	(cond
	 (raise (if (not (eq? raise 'restack-only))
		    (raise-window (car (reverse windows))))
		(restack-windows (reverse windows))))
	(let* ((num-major (inexact->exact (ceiling (sqrt num-windows))))
	       (num-minor (inexact->exact (ceiling (/ num-windows num-major))))
	       (num-per-row (case order
			      ((horizontal) num-major)
			      ((vertical) num-minor)))
	       (num-per-column (case order
				 ((horizontal) num-minor)
				 ((vertical) num-major)))
	       (start-x (car start-pos))
	       (start-y (cadr start-pos))
	       (range-x (- (car end-pos) start-x))
	       (range-y (- (cadr end-pos) start-y)))
	  (let loop ((windows (list-head windows num-windows))
		     (row 0)
		     (column 0)
		     (cur-x start-x)
		     (cur-y start-y)
		     (next-x (integer-step start-x range-x num-per-row 1))
		     (next-y (integer-step start-y range-y num-per-column 1)))
	    (cond
	     ((not (null? windows))
	      (let* ((win (car windows))
		     (fs (window-frame-size win))
		     (new-size (case resize
				 ((always) (list (- next-x cur-x) (- next-y cur-y)))
				 ((shrink-only)
				  (list (min (car fs) (- next-x cur-x))
					(min (cadr fs) (- next-y cur-y))))
			   ;;; MS:FIXME:: check for bad values
				 (else fs))))
		(if (not (equal? new-size fs))
		    (resize-frame (car new-size) (cadr new-size) win))
		(move-window cur-x cur-y win))
	      (cond
	       ((and (eq? order 'horizontal) (= column (- num-per-row 1)))
		(loop (cdr windows) (+ row 1) 0 start-x next-y
		      (integer-step start-x range-x num-per-row 1)
		      (integer-step start-y range-y num-per-column (+ row 2))))
	       ((eq? order 'horizontal)
		(loop (cdr windows) row (+ column 1) next-x cur-y 
		      (integer-step start-x range-x num-per-row (+ column 2))
		      next-y))
	       ((and (eq? order 'vertical) (= row (- num-per-column 1)))
		(loop (cdr windows) 0 (+ column 1) next-x start-y
		      (integer-step start-x range-x num-per-row (+ column 2))
		      (integer-step start-y range-y num-per-column 1)))
	       ((eq? order 'vertical)
		(loop (cdr windows) (+ row 1) column cur-x next-y
		      next-x
		      (integer-step start-y range-y num-per-column 
				    (+ row 2)))))))))))))    
			  
(define*-public (tile #&key (only ()) (except ()) 
			 (by-stacking #f) (by-focus #f)
			 (reverse #f)
			 (all-viewports #f) (desk (current-desk))
			 (ignore-default-exceptions #f)
			 (start-pos '(0 0)) 
			 (end-pos (display-size)) (resize 'always)
			 (raise 'restack-only)
			 (max-windows #f)
			 (order 'horizontal))
  "Tile the windows on the specified desk.
The DESK option, defaulting to the current desk, specifies which desk;
ALL-VIEWPORTS, when true indicates that the windows in all viewports
of this desk should be tiled, otherwise only the current viewport
is tiled. 

The options ONLY, EXCEPT, BY-STACKING, BY-FOCUS and REVERSE indicate
the windows to use and the order to use them in, as with
`list-windows'. However, unless IGNORE-DEFAULT-EXCEPTIONS is #t,
transient, maximized, sticky and iconified windows will be always be
excluded.

START-POS, END-POS, RESIZE, RAISE, MAX-WINDOWS and ORDER
control the tiling options as for `tile-windows'."
  (tile-windows 
   (list-windows 
    #:only (cons (on-desk-n? desk) 
		 (append (if all-viewports '() 
			     (list in-viewport-any-desk?))
			 only))
    #:except (append (if ignore-default-exceptions
			 ()
			 (list transient? maximized? 
			       sticky-window? iconified-window?
			       (lambda (w) 
				 (= (window-title-height w) 0))))
		     except)
    #:by-stacking by-stacking #:by-focus by-focus #:reverse reverse)
   #:start-pos start-pos #:end-pos end-pos #:raise raise 
   #:resize resize #:max-windows max-windows #:order order))


(define*-public (tile-windows-interactively #&optional (order 'horizontal))
  "Tile a set of selected windows either vertically or horizontally.
ORDER can be either 'horizontal or 'vertical.
The windows used are selected either by `selected-windows-list' or 
`select-window-group'.
If `selected-windows-list' is empty, then `select-window-group' is used.
See also the undo module and `push-undo-global' to save the window 
configuration before executing this in case the effect is not what you
expected."
  (interactive)
  (let* ((winlist (reverse (selected-windows-list)))
	 (wins (if (pair? winlist) winlist (select-window-group)))
	 (r (enclosing-rectangle wins)))
    (if (pair? winlist)
	(unselect-all-windows)
	(for-each unflash-window wins))
    (push-undo-global)
    (tile-windows wins
		  #:start-pos (rect-nw r)
		  #:end-pos (rect-se r)
		  #:order order)))
