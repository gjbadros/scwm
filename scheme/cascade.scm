;;;; $Id$
;;;; Copyright (C) 1998 Maciej Stachowiak
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



(define-module (app scwm cascade)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm flash-window)
  :use-module (app scwm flux)
  :use-module (app scwm window-selection)
  :use-module (app scwm wininfo)
  :use-module (app scwm undo)
  :use-module (app scwm winlist)
  :use-module (app scwm winops))



(define-public (meta-apply proc . procs)
  "Make procedure to combine results of several procedures.
Makes a procedure which uses PROC to combine the results of applying
PROCS to ARGS."
  (lambda args (apply proc (map (lambda (p) (apply p args)) procs))))

(define (num-or-apply proc-or-num win)
  (if (number? proc-or-num)
      proc-or-num
      (proc-or-num win)))

(define*-public (cascade-windows 
		 windows #&key (start-pos '(0 0)) 
		 (max-size (display-size)) (resize 'shrink-only)
		 (raise 'restack-only)
		 (x-increment (meta-apply + window-title-height
					  window-frame-border-width))
		 (y-increment (meta-apply + window-title-height
					  window-frame-border-width)))
  "Cascade WINDOWS according to several parameters.  
Cascading places the windows in list order in a diagonal order starting 
at START-POS, (0 0) by default. Each window's size is limited to
MAX-SIZE, by default the display size.

RESIZE may be #f, 'shrink-only or 'always, indicating that the windows
should never be resized, that they should be resized to the max-size
only if they are bigger, or that they should always be resized to the
max size. The default is 'shrink-only.

RAISE may be #f, #t, or 'restack-only, indicating that the windows
should not be moved in the stacking order; that they should be raised
on top of other windows and placed in the cascade order with the upper
left window lowest in the stacking order; or that they should be
restacked as for #t but not raised above other windows,
respectively. The default is 'restack-only.

X-INCREMENT may be an integer or a procedure that takes a window and
returns an integer, and which will be applied to a window to get the
horizontal offset for the next in the cascade order. The default is
the sum of the window's border width and title height.

Y-INCREMENT may be an integer or a procedure that takes a window and
returns an integer, and which will be applied to a window to get the
vertical offset for the next in the cascade order. The default is the
sum of the window's border width and title height."
  (cond 
   ((not (null? windows))
    (cond
     (raise (if (not (eq? raise 'restack-only))
		(raise-window (car (reverse windows))))
	    (restack-windows (reverse windows))))))
  (let loop ((windows windows)
	     (cur-x (car start-pos))
	     (cur-y (cadr start-pos)))
    (if (not (null? windows))
	(let* ((win (car windows))
	       (fs (window-frame-size win))
	       (new-size (case resize
			   ((always) max-size)
			   ((shrink-only)
			    (list (min (car fs) (car max-size))
				  (min (cadr fs) (cadr max-size))))
			   ;;; MS:FIXME:: check for bad values
			   (else fs))))
	  (if (not (equal? new-size fs))
	      (resize-frame (car new-size) (cadr new-size) win))
	  (move-to cur-x cur-y win)
	  (loop (cdr windows) (+ cur-x (num-or-apply x-increment win))
		(+ cur-y (num-or-apply y-increment win)))))))
  
(define*-public (cascade #&key (only ()) (except ()) 
			 (by-stacking #f) (by-focus #f)
			 (reverse #t)
			 (all-viewports #f) (desk (current-desk))
			 (ignore-default-exceptions #f)
			 (start-pos '(0 0)) (max-size (display-size))
			 (resize #f) (raise #t) 
			 (x-increment (meta-apply + window-title-height
						  window-frame-border-width))
			 (y-increment (meta-apply + window-title-height
						  window-frame-border-width)))
  "Cascade the windows on the specified desk.
The DESK option, defaulting to the current desk, specifies which desk;
ALL-VIEWPORTS, when true indicates that the windows in all viewports
of this desk should be cascaded, otherwise only the current viewport
is cascaded. 

The options ONLY, EXCEPT, BY-STACKING, BY-FOCUS and REVERSE indicate the
windows to use and the order to use them in, as with
`list-windows'. However, unless IGNORE-DEFAULT-EXCEPTIONS is #t, transient,
maximized, sticky and iconified windows will be always be excluded. Note that
REVERSE default to true, because the list of windows is usually passed in
top to bottom order.

START-POS, MAX-SIZE, RESIZE, RAISE, X-INCREMENT and Y-INCREMENT
control the cascading options as for `cascade-windows'."
  (cascade-windows 
   (list-windows 
    #:only (cons (on-desk-n? desk) 
		 (append (if all-viewports '() 
			     (list in-viewport-any-desk?))
			 (listify-if-atom only)))
    #:except (append (if ignore-default-exceptions
			 ()
			 (list transient? maximized? 
			       sticky-window? iconified-window?
			       (lambda (w) 
				 (= (window-title-height w) 0))))
		     (listify-if-atom except))
    #:by-stacking by-stacking #:by-focus by-focus #:reverse reverse)
   #:start-pos start-pos #:max-size max-size #:raise raise 
   #:resize resize #:x-increment x-increment #:y-increment y-increment))



;; (cascade-windows-interactively)
(define*-public (cascade-windows-interactively . args)
  "Cascade a set of selected windows.
The windows used are selected either by `selected-windows-list' or 
`select-window-group'.  Accepts all options that 'cascade-windows' accepts.
If `selected-windows-list' is empty, then `select-window-group' is used.
See also the undo module and `push-undo-global'.  This is an undoable
operation that saves the window configuration before execution in case
the effect is not what you expected."
  (interactive)
  (let* ((winlist (reverse (selected-windows-list)))
	 (wins (if (pair? winlist) winlist (select-window-group))))
    (if (pair? winlist)
	(unselect-all-windows)
	(for-each unflash-window wins))
    (push-undo-global)
    (apply cascade-windows (cons wins args))))
