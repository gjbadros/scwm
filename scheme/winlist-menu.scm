;;; $Id$
;;; winlist-menu.scm
;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm winlist-menu)
  :use-module (app scwm optargs)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm base)
  :use-module (app scwm winops)
  :use-module (app scwm animated-iconify)
  :use-module (app scwm menus-extras)
  :use-module (app scwm style-options)
  :use-module (app scwm flash-window)
  :use-module (app scwm listops))


(define*-public (make-window-list-menu #:key (only '()) (except '())
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


(define*-public (show-window-list-menu warp-to-index permit-alt-release-selection? #:rest rest)
  "Popup a window list menu.
Warps the pointer to the WARP-TO-INDEX menu item iff it is an integer.
If PERMIT-ALT-RELEASE-SELECTION? is #t, then the release of the Alt/Meta
modifier will select the item (this is a bit of a hack to make a nicer
task-switching menu).
Accepts all keyword arguments that `make-window-list-menu' takes."
  (popup-menu (apply make-window-list-menu rest) warp-to-index #f #f #f permit-alt-release-selection?))


(define*-public (select-window-from-window-list #:key
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
;; (let ((w (select-window-from-window-list #:only iconified-window?)))
;;  (deiconify-window w) (move-to 0 0 w))
;; (select-window-from-window-list)
;; (unflash-window (get-window))


(define*-public (show-icon-list-menu)
  "Show a window list of only iconfied programs.
The selection procedure deiconifies the window and gives it focus."
  (interactive)
  (show-window-list-menu 1 #f
			 #:only iconified-window?
			 #:proc animated-deiconify-to-vp-focus))

(define*-public (show-xterm-window-list-menu)
  "Show a window list of only xterms.
The selection procedure deiconifies the window and gives it focus."
  (interactive)
  (show-window-list-menu 1 #f
			 #:only (lambda (w)
				  (or 
				   (string=? (window-class w) "XTerm")
				   (string=? (window-class w) "NXTerm")))
			 #:proc animated-deiconify-to-vp-focus))

(define*-public (show-netscape-window-list-menu)
  "Show a window list of only Netscape windows.
The selection procedure deiconifies the window and gives it focus."
  (interactive)
  (show-window-list-menu 1 #f
			 #:only (class-match?? "Netscape")
			 #:proc animated-deiconify-to-vp-focus))

