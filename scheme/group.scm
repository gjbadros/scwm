;;;; $Id$
;;;; Copyright (C) 1999 Robert Bihlmeyer and Greg J. Badros
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

(define-module (app scwm group)
  :use-module (app scwm winops)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm winlist))

(define WindowGroupHint 64)		; window group present in WM_HINTS
(define FlagsHintIndex 0)		; position of flags in WM_HINTS
(define GroupHintIndex 8)		; position of window group in WM_HINTS

(define (group-leader-id group)
  "Returns the id of the leader window of GROUP.
GROUP can be any window belonging to the group."
  (let ((hints (car (X-property-get group "WM_HINTS"))))
    (if (not (zero? (logand (vector-ref hints FlagsHintIndex)
			    WindowGroupHint)))
	(vector-ref hints GroupHintIndex)
	#f)))

(define-public (group-window group)
  "Returns a distinguished window of GROUP.
Since a group is represented either by a single window or by a
list, this returns either GROUP or the `car' of the list."
  (cond ((list? group) (car group))
	((window? group) group)
	(else (error "Group is not a window or a list."))))

(define-public (group->windows group)
  "Returns a list of windows belonging to GROUP.
GROUP can be any window belonging to the group. All its peers, including
itself, are returned.  If GROUP is a list, it is treated
as the list of windows and is just returned.  This permits
all the group action procedures to be used on lists of windows, too."
  (if (list? group)
      group
      (let ((leader (group-leader-id group)))
	(if leader
	    (list-windows #:only (lambda (w) (equal? (group-leader-id w) leader)))
	    (list group)))))

(for-each (lambda (group-op window-op)
	    (eval `(define*-public (,group-op #&optional (group (get-window)))
		     ,(string-append "Apply `" window-op "' to all members "
				     "of GROUP.")
		     (for-each ,window-op (group->windows group)))))
	  '(iconify-group-individually window-shade-group window-unshade-group
	    stick-group unstick-group keep-group-on-top un-keep-group-on-top
	    close-group delete-group destroy-group)
	  '(iconify window-shade window-unshade
	    stick unstick keep-on-top un-keep-on-top
	    close-window delete-window destroy-window))

(define (seperate-group-windows windows leader members non-members)
  (if (null? windows)
      (cons members non-members)
      (if (= (group-leader-id (car windows)) leader)
	  (seperate-group-windows (cdr windows) leader
				  (cons (car windows) members) non-members)
	  (seperate-group-windows (cdr windows) leader
				  members (cons (car windows) non-members)))))

(define*-public (raise-group #&optional (group (get-window)))
  "Raise members of GROUP above all other windows.
Keeps the relative stacking order of the members intact."
  (let ((res (seperate-group-windows (list-windows #:by-stacking #t
						   #:reverse #t)
				     (group-leader-id test-group) () ())))
    (restack-windows (append (car res) (cdr res)))))

(define*-public (lower-group #&optional (group (get-window)))
  "Raise members of GROUP above all other windows.
Keeps the relative stacking order of the members intact."
  (let ((res (seperate-group-windows (list-windows #:by-stacking #t
						   #:reverse #t)
				     (group-leader-id test-group) () ())))
    (restack-windows (append (cdr res) (car res)))))

(define*-public (move-group-relative dx dy #&optional (group (get-window)))
  "Move all members of GROUP by DX, DY pixels."
  (for-each (lambda (w) (move-window-relative dx dy w))
	    (group->windows group)))

(define*-public (move-group x y #&optional (group (get-window)))
  "Move GROUP to virtual coordinates X, Y.
Move the window GROUP represents to X, Y, and keep the other windows in GROUP
in the same relative positions to this window."
  (let ((pos (window-viewport-position (group-window group))))
    (move-group-relative (- x (car pos)) (- y (cadr pos)) group)))

(define*-public (move-group-to-desk desk #&optional (group (get-window)))
  "Move all members of GROUP to DESK.
See `move-window-to-desk'."
  (for-each (lambda (w) (move-window-to-desk desk w)) (group->windows group)))

(delete 1 '(1 2 3 4))

(define*-public (interactive-move-group #&optional (group (get-window #f #t #f)))
  "Move GROUP interactively.
You can drag around the window GROUP represents. The other windows in GROUP
will move along."
  
  (let* ((gwin (group-window group))
	 (others (delete! gwin (group->windows group))))
    (if (null? others)
	(interactive-move gwin)
	(let* ((last-pos (window-viewport-position gwin))
	       (drag-others-along
		(lambda (win x y)
		  (let ((dx (- x (car last-pos)))
			(dy (- y (cadr last-pos))))
		    (for-each (lambda (w) (move-window-relative dx dy w))
			      others))
		  (set! last-pos (list x y)))))
	  (dynamic-wind
	   (lambda () 
	     (add-hook! interactive-move-new-position-hook drag-others-along))
	   (lambda ()
	     (interactive-move gwin))
	   (lambda ()
	     (remove-hook! interactive-move-new-position-hook drag-others-along)))))))

(define*-public (deiconify-group #&optional (group (get-window)) x y)
  "Deiconify all members of GROUP."
  (for-each (lambda (w)
	      (deiconify w x y)
	      (set-show-icon! #t w)
	      (set-object-property! w 'group-deiconify #f))
	    (group->windows group)))

(define*-public (deiconify-group-or-window #&optional (win (get-window)) x y)
  "Deiconify WIN, and perhaps all members of its group.
If WIN's icon was the result of an `iconify-group', all members of the group
are deiconified; otherwise, only WIN is affected."
  (cond ((object-property win 'group-deiconify)
	 (deiconify-group win x y))
	(else (deiconify win x y))))

(define*-public (iconify-group #&optional (group (get-window)))
  "Iconify GROUP into one icon.
The icon is that of the window GROUP represents.
`deiconify-group-or-window' will deiconify this icon into the whole GROUP."
  (set-object-property! group 'group-deiconify #t)
  (for-each (lambda (w)
	      (set-show-icon! (equal? w group) w)
	      (iconify w))
	    (group->windows group)))
