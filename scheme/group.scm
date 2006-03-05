;;;; $Id$
;;;; Copyright (C) 1999, 2000 Robert Bihlmeyer and Greg J. Badros
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
  :use-module (app scwm wininfo)
  :use-module (app scwm optargs)
  :use-module (app scwm listops)
  :use-module (app scwm window-selection)
  :use-module (app scwm tile)
  :use-module (app scwm cascade)
  :use-module (app scwm base)
  :use-module (app scwm winlist))

(define WindowGroupHint 64)		; window group present in WM_HINTS
(define FlagsHintIndex 0)		; position of flags in WM_HINTS
(define GroupHintIndex 8)		; position of window group in WM_HINTS

(define (group-leader-id group)
  "Returns the id of the leader window of GROUP.
GROUP can be any window belonging to the group."
  (let ((xprop (X-property-get group "WM_HINTS")))
    (if xprop
        (let ((hints (car xprop)))
          (if (not (zero? (logand (vector-ref hints FlagsHintIndex)
                                  WindowGroupHint)))
              (vector-ref hints GroupHintIndex)
              #f))
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
	    (eval `(define*-public (,group-op #:optional (group (get-window)))
		     ,(string-append "Apply `" window-op "' to all members "
				     "of GROUP.")
		     (for-each ,window-op (group->windows group)))))
	  '(iconify-group-individually window-shade-group window-unshade-group
	    stick-group unstick-group keep-group-on-top un-keep-group-on-top
	    close-group delete-group destroy-group)
	  '(iconify-window shade-window unshade-window
	    stick-window unstick-window keep-on-top un-keep-on-top
	    close-window delete-window destroy-window))

;;; Returns result of adding windows from WINDOWS with group-leader-id matching LEADER
;;;    to MEMBERS and other windows to NON-MEMBERS.  Result is returned as a cons cell.
;;;    List of members first; list of non-members second.
;;; MEMBERS and NON-MEMBERS are not modified.
;;; Does not preserve the order of WINDOWS in the members and non-members return values.
(define (seperate-group-windows-id windows leader-id members non-members)
  (if leader-id
      (if (null? windows)
          (cons members non-members)
          (let ((other-gli (group-leader-id (car windows))))
            (if (and other-gli (= other-gli leader-id))
                (seperate-group-windows-id (cdr windows) leader-id
                                           (cons (car windows) members) non-members)
                (seperate-group-windows-id (cdr windows) leader-id
                                           members (cons (car windows) non-members)))))
      (cons members (append windows non-members))))

;;; Returns result of adding windows from WINDOWS in GROUP-LIST
;;;    to MEMBERS and other windows to NON-MEMBERS.  Result is returned as a cons cell.
;;;    List of members first; list of non-members second.
;;; GROUP-LIST, MEMBERS, and NON-MEMBERS are not modified.
;;; Does not preserve the order of WINDOWS in the members and non-members return values.
(define (seperate-group-windows-list windows group-list members non-members)
  (if (null? group-list)
      (cons members (append windows non-members))
      (if (null? windows)
          (cons members non-members)
          (if (member (car windows) group-list)
              (seperate-group-windows-list (cdr windows) group-list
                                           (cons (car windows) members) non-members)
              (seperate-group-windows-list (cdr windows) group-list
                                           members (cons (car windows) non-members))))))

;;; Returns result of adding windows from WINDOWS in GROUP
;;;    to MEMBERS and other windows to NON-MEMBERS.  Result is returned as a cons cell.
;;;    List of members first; list of non-members second.
;;; GROUP, MEMBERS, and NON-MEMBERS are not modified.
;;; Does not preserve the order of WINDOWS in the members and non-members return values.
;;; GROUP can be a window, a list of windows, or a group-leader-id
(define (seperate-group-windows windows group members non-members)
  (if (list? group)
      (seperate-group-windows-list windows group members non-members)
      (if (window? group)
          (let ((glid (group-leader-id group)))
            (if glid
                (seperate-group-windows-id windows glid members non-members)
                (seperate-group-windows-list windows (list group) members non-members)))
          (seperate-group-windows-id windows group members non-members))))

(define*-public (raise-group #:optional (group (get-window)))
  "Raise members of GROUP above all other windows.
Keeps the relative stacking order of the members intact."
  (let ((res (seperate-group-windows (list-windows #:by-stacking #t
						   #:reverse #t)
				     group '() '())))
    (restack-windows (append (car res) (cdr res)))))

(define*-public (lower-group #:optional (group (get-window)))
  "Raise members of GROUP above all other windows.
Keeps the relative stacking order of the members intact."
  (let ((res (seperate-group-windows (list-windows #:by-stacking #t
						   #:reverse #t)
				     group '() '())))
    (restack-windows (append (cdr res) (car res)))))

(define*-public (move-group-relative dx dy #:optional (group (get-window)))
  "Move all members of GROUP by DX, DY pixels."
  (for-each (lambda (w) (move-window-relative dx dy w))
	    (group->windows group)))

(define*-public (move-group x y #:optional (group (get-window)))
  "Move GROUP to viewport coordinates X, Y.
Move the window GROUP represents to X, Y, and keep the other windows in GROUP
in the same relative positions to this window."
  (let ((pos (window-viewport-position (group-window group))))
    (move-group-relative (- x (car pos)) (- y (cadr pos)) group)))

(define*-public (move-group-to-desk desk #:optional (group (get-window)))
  "Move all members of GROUP to DESK.
See `move-window-to-desk'."
  (for-each (lambda (w) (move-window-to-desk desk w)) (group->windows group)))

(define*-public (interactive-move-group #:optional (group (get-window #t #f #f)))
  "Move GROUP interactively.
You can drag around the window GROUP represents. The other windows in GROUP
will move along."
  (let* ((gwin (group-window group))
	 (others (delete! gwin (group->windows group))))
    (if (null? others)
	(interactive-move gwin)
	(let* ((last-pos (window-position gwin))
	       (drag-others-along
		(lambda (win x y)
		  (let* ((win-pos (window-position win))
                         (dx (- (car win-pos) (car last-pos)))
                         (dy (- (cadr win-pos) (cadr last-pos))))
                    (display last-pos)(display win-pos)(newline)
		    (for-each (lambda (w) 
				;; GJB:FIXME:: must also run the 
				;; interactive-move-new-position-hook for w
				;; and should do the start/end hooks for it, too
				(move-window-relative dx dy w))
			      others)
                    (set! last-pos win-pos)))))
	  (dynamic-wind
	   (lambda () 
	     (add-hook! interactive-move-new-position-hook drag-others-along))
	   (lambda ()
	     (interactive-move gwin))
	   (lambda ()
	     (remove-hook! interactive-move-new-position-hook drag-others-along)))))))

;;; SRL:FIXME:: Broken.  Can't test because set-show-icon! is deadly.
(define*-public (deiconify-group #:optional (group (get-window)) x y)
  "BROKEN: Deiconify all members of GROUP."
  (for-each (lambda (w)
	      (deiconify-window w x y)
	      (set-show-icon! #t w)
	      (set-object-property! w 'group-deiconify #f))
	    (group->windows group)))

;;; SRL:FIXME:: Broken.  Can't test because set-show-icon! is broken.
(define*-public (deiconify-group-or-window #:optional (win (get-window)) x y)
  "BROKEN: Deiconify WIN, and perhaps all members of its group.
If WIN's icon was the result of an `iconify-group', all members of the group
are deiconified; otherwise, only WIN is affected."
  (cond ((object-property win 'group-deiconify)
	 (deiconify-group win x y))
	(else (deiconify-window win x y))))

;;; SRL:FIXME:: If group is a list of windows, then no icon is actually displayed
;;; SRL:FIXME:: Some windows will not iconify here that will iconify singly
;;; SRL:FIXME:: Broken.  Can't test because set-show-icon! is broken.
(define*-public (iconify-group #:optional (group (get-window)))
  "BROKEN: Iconify GROUP into one icon.
The icon is that of the window GROUP represents.
`deiconify-group-or-window' will deiconify this icon into the whole GROUP."
  (set-object-property! group 'group-deiconify #t)
  (for-each (lambda (w)
	      (set-show-icon! (equal? w group) w)
	      (iconify-window w))
	    (group->windows group)))


(define-public (make-window-group-menu w)
  "Return a menu for window group operations.
W must be a window or #f.  The resulting menu can be used for
manipulating the current group of selected windows or selecting
windows based on comparison to properties of W."
  (let* ((swl (selected-windows-list))
	 (wla? (pair? swl)) ;; wla? -- winlist-active?
	 (n (length swl))
	 (nstr (number->string n))
	 (sel? (window-is-selected? w))
	 (wop (if sel? "Unselect" "Select"))
	 (title (if w (window-title w) #f))
	 (resource (if w (window-resource w) #f))
	 (class (if w (window-class w) #f))
         (group-size (if w (length (group->windows w)) 0))
         (group-size-str (number->string group-size)))
    (menu
     (append
      (filter-map 
       noop
       (list
	(menu-title "Window Group") menu-separator
	(menuitem 
	 (string-append "&" wop (if w " this" " a") " window")
	 #:action select-window-toggle) 
	(if (> group-size 1)
	    (menuitem
	     (string-append wop " " group-size-str " windows in this windows &group")
	     #:action (lambda () (map
                                  (if sel? select-window-remove select-window-add)
				  (group->windows w))))
	    #f)
	(if title
	    (menuitem
	     (string-append wop " windows with &title `" title "'")
	     #:action (lambda () ((if sel? unselect-matching-windows select-matching-windows)
				  (title-match?? title))))
	    #f)
	(if resource
	    (menuitem
	     (string-append wop " windows &named `" resource "'")
	     #:action (lambda () ((if sel? unselect-matching-windows select-matching-windows)
				  (resource-match?? resource))))
	    #f)
	(if class
	    (menuitem
	     (string-append wop " windows of &class `" class "'")
	     #:action (lambda () ((if sel? unselect-matching-windows select-matching-windows)
				  (class-match?? class))))
	    #f)
	(if wla? (menuitem "Unselect &all windows" 
			   #:action unselect-all-windows)
	    #f)))
      (if (and wla? (>= n 1))
	  (list
	   (menuitem (string-append "&Tile " nstr " windows") 
		     #:action tile-windows-interactively)
	   (menuitem (string-append "&Cascade " nstr " windows") 
		     #:action cascade-windows-interactively)
	   (menuitem (string-append "M&ove " nstr " windows")
		     #:action (lambda () (interactive-move-group swl)))
	   (menuitem (string-append "&Iconify " nstr " windows")
                     ;; SRL:FIXME:: Change back to iconify-group when it is fixed.
		     #:action (lambda () (iconify-group-individually swl)))
	   (menuitem (string-append "&Shade " nstr " windows")
		     #:action (lambda () (window-shade-group swl)))
	   (menuitem (string-append "&Unshade " nstr " windows")
		     #:action (lambda () (window-unshade-group swl)))
	   (menuitem (string-append "Stic&k " nstr " windows")
		     #:action (lambda () (stick-group swl)))
	   (menuitem (string-append "U&nstick " nstr " windows")
		     #:action (lambda () (unstick-group swl)))
	   (menuitem (string-append "Kee&p on top " nstr " windows")
		     #:action (lambda () (keep-group-on-top swl)))
	   (menuitem (string-append "Unk&eep on top " nstr " windows")
		     #:action (lambda () (un-keep-group-on-top swl)))
	   (menuitem (string-append "C&lose " nstr " windows")
		     #:action (lambda () (close-group swl) (unselect-all-windows)))
	   (menuitem (string-append "Destroy group " nstr " windows")
		     #:action (lambda () (destroy-group swl) (unselect-all-windows))))
	  '()
	  )))))

(define*-public (popup-window-group-menu)
  "Popup the window group menu."
  (interactive)
  (popup-menu (make-window-group-menu (get-window))) #t)

(define*-public (popup-window-group-menu-no-warp)
  "Popup the window group menu without warping to the first menu item."
  (interactive)
  (popup-menu (make-window-group-menu (get-window))))

(define*-public (interactive-move-selected-group-or-window)
  "Interactively move either the selected windows or the current window.
The current window alone is moved if no windows are selected.
All windows are unselected after this operation."
  (interactive)
  (let ((wingroup (selected-windows-list)))
    (if (pair? wingroup)
	(interactive-move-group (selected-windows-list))
	(interactive-move (get-window))))
  (unselect-all-windows))

