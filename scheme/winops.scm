;;;; $Id$
;;;; Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros and Jeff W. Nichols
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



(define-module (app scwm winops)
  :use-module (app scwm optargs)
  :use-module (app scwm defoption)
  :use-module (app scwm c-animation)
  :use-module (app scwm base)
  :use-module (app scwm winlist)
  :use-module (app scwm wininfo)
  :use-module (app scwm nonants)
  :use-module (app scwm path-cache)
  :use-module (app scwm style-options)
  :use-module (app scwm window-selection)
  :use-module (app scwm sort))



(define-scwm-group winops "Window Operations")

;;; Toggling operations

(define-public (make-toggling-winop pred neg pos)
  "Returns a procedure which takes a window WIN and toggles a property of it.
PRED, NEG, and POS should be functions which take a window and
check whether the property holds for the window, reset the property
on the window, and set the property on the window, respectively."
  (lambda* (#&optional (win (get-window)))
	   "A toggling window operation.
See `make-toggling-winop'."
    (interactive)
    (if win (if (pred win)
		(neg win)
		(pos win)))))

(define*-public (close-window #&optional (win (get-window #t #t #t)))
  "Close WIN either by deleting it or destroying it.
WIN is only destroyed if it is not deleteable."
  (interactive)
  (if win (if (window-deletable? win)
	      (delete-window win)
	      (destroy-window win))))

(define*-public (focus-or-toggle-raise #&optional (win (window-with-pointer)))
  "Focus on WIN if it does not have the focus, else toggle-raise WIN."
  (if (equal? (window-with-focus) win)
      (toggle-raise win)
      (focus-window win)))

(define-public toggle-raise
  (make-toggling-winop raised? lower-window raise-window))

(define-public toggle-iconify
  (make-toggling-winop iconified-window? deiconify-window iconify-window))

(define-public toggle-stick 
  (make-toggling-winop sticky-window? unstick-window stick-window))

(define-public toggle-window-shade 
  (make-toggling-winop shaded-window? unshade-window shade-window))

(define-public toggle-on-top
  (make-toggling-winop kept-on-top? un-keep-on-top keep-on-top))

(define-public toggle-titlebar
  (make-toggling-winop titlebar-shown? hide-titlebar show-titlebar))

(define*-public (hide-titlebar-in-place #&optional (win (get-window)))
  "Turn off display of the titlebar for WIN without moving the client window.
This may move the frame to keep the application client window area in
the same position as before the call."
  (hide-titlebar win #t))

(define*-public (show-titlebar-in-place #&optional (win (get-window)))
  "Turn on display of the titlebar for WIN without moving the client window.
This may move the frame to keep the application client window area in
the same position as before the call."
  (show-titlebar win #t))

(define-public toggle-titlebar-in-place
  (make-toggling-winop titlebar-shown? hide-titlebar-in-place show-titlebar-in-place))

(define-public toggle-border
  (make-toggling-winop border-normal? plain-border normal-border))

(define-public toggle-stick-icon
  (make-toggling-winop icon-sticky? unstick-icon stick-icon))


(define-scwm-option *maximize-animatedly* #t
  "Whether to use animation when maximizing and unmaximizing."
  #:type 'boolean
  #:group 'winops)
;;(set! *maximize-animatedly* #f)

;;; Maximization
(define*-public (maximize nw nh #&optional (win (get-window)))
  "Maximize WIN to new pixel width NW and new pixel height NH.
If NW or NH is 0, that dimension is not changed."
  (if win (let* ((pos (window-viewport-position win))
		 (virt-pos (window-virtual-position win))
		 (x (car pos))
		 (y (cadr pos))
		 (frame-size (window-frame-size win))
		 (pix-width (car frame-size))
		 (pix-height (cadr frame-size))
		 (cli-size (window-size win))
		 (nx (cond
		      ((> 0 x) 0)
		      ((> display-width (+ x nw)) x)
		      ((> display-width nw) (- display-width nw))
		      (#t 0)))
		 (ny (cond
		      ((> 0 y) 0)
		      ((> display-height (+ y nh)) y)
		      ((> display-height nh) (- display-height nh))
		      (#t 0))))
	    (let ((args (list
			 (if (> nw 0) nw pix-width)
			 (if (> nh 0) nh pix-height) win (vpx->vx nx) (vpy->vy ny))))
;;	      (write args) (newline)  ;; debugging
	      (apply (if *maximize-animatedly* 
			 animated-resize-frame resize-frame) args))
	    (if (not (maximized? win))
		(begin
;;		  (set-window-gravity! 'northwest win)
		  (set-window-property!
		   win 'maximized (list virt-pos cli-size frame-size)))))))

(define*-public (maximized? #&optional (win (get-window)))
  "Return #t if WIN is maximized, #f otherwise."
  (->bool (window-property win 'maximized)))

;; uses client units
(define*-public (unmaximize #&optional (win (get-window)))
  "Unmaximize WIN so it returns to its size/position before maximization."
  (if win (let ((max-prop (window-property win 'maximized)))
	       (cond
		(max-prop
		 (let* ((pos (car max-prop))
			(cli-size (cadr max-prop))
			(frame-size (caddr max-prop)))
		   ((if *maximize-animatedly*
			animated-resize-frame resize-frame)
		    (car frame-size) (cadr frame-size)
		    win (car pos) (cadr pos))
		   (set-window-property! win 'maximized #f)))))))


(define*-public (toggle-maximize nw nh #&optional (win (get-window)))
  "Maximize to width NW, height NH if not maximized, or unmaximize."
  (if win (if (maximized? win)
	      (unmaximize win)
	      (maximize nw nh win))))

;; add a style option for maximizing
(add-window-style-option #:start-maximized 
			  (lambda (arg w) 
			    (if arg
				(apply maximize (append arg (list w)))
				(unmaximize w))))


;;; Interactive Moves and Resizes
;;; (with automatic selection of opaque or rubber-band move)


(define-scwm-option *opaque-move-percent* 50
  "Percent of display area below which windows are moved opaquely."
  #:type 'percent  ;; #GJB:FIXME:: 'percent-or-on-off
  #:group 'winops)

(define-scwm-option *opaque-resize-percent* 35
  "Percent of display area below which windows are resized opaquely."
  #:type 'percent  ;; #GJB:FIXME:: 'percent-or-on-off
  #:group 'winops)

(define-public (window-frame-area win)
  "Return the area of WIN's frame in square pixels."
  (let* ((frame-size (window-frame-size win))
	 (width (car frame-size))
	 (height (cadr frame-size)))
    (* width height)))

(define display-area (* display-width display-height))

(define-public (default-resize-opaquely? win)
  "Return #t if WIN has area <= *opaque-resize-percent* of the screen, else #f.
If *opaque-resize-percent* is a boolean, not a number, just return it."
  (let ((p *opaque-resize-percent*))
    (if (boolean? p)
	p
	(<= (window-frame-area win) 
	    (* display-area (/ p 100))))))

(define-public (default-move-opaquely? win)
  "Return #t if WIN has area <= *opaque-move-percent* of the screen, else #f.
If *opaque-move-percent* is a boolean, not a number, just return it."
  (let ((p *opaque-move-percent*))
    (if (boolean? p)
	p
	(<= (window-frame-area win)
	    (* display-area (/ p 100))))))

(define-scwm-option *move-opaquely-proc* default-move-opaquely?
  "User-settable predicate to determine if windows should be moved opaquely.
The procedure should take a single argument, the window."
  #:type 'proc
  #:group 'winops)

(define-scwm-option *resize-opaquely-proc* default-resize-opaquely?
  "User-settable predicate to determine if windows should be resized opaquely..
The procedure should take a single argument, the window."
  #:type 'proc
  #:group 'winops)


(define*-public (interactive-move 
		 #&optional (win (get-window #t #f #f))
		 (opaquely? (if win ((optget *move-opaquely-proc*) win))))
  "Move WINDOW interactively and possibly opaquely. 
If OPAQUELY? is specified, it is used to determine if the window
should be moved opaquely, or using a rubber-band. If it is not
spcified, `interactive-move' calls `*move-opaquely-proc*' on WIN and moves
opaquely if that returns #t and uses a rubber-band if it returns #f."
  (interactive)
  (if win ((if opaquely? opaque-move rubber-band-move) win)))

(define*-public (interactive-resize 
		 #&optional (win (get-window #t #f #f))
		 (opaquely? (if win ((optget *resize-opaquely-proc*) win))))
  "Resize WINDOW interactively and possibly opaquely. 
If OPAQUELY? is specified, it is used to determine if the window
should be resized opaquely, or using a rubber-band. If it is not
spcified, `interactive-resize' calls `*resize-opaquely-proc*' on WIN and
moves opaquely if that returns #t and uses a rubber-band if it returns
#f."
  (interactive)
  (if win ((if opaquely? opaque-resize rubber-band-resize) win)))

;;; hack to work with minimal.scm
(set! hack-interactive-move interactive-move)
(set! hack-interactive-resize interactive-resize)


;; Printing
(if (cached-program-exists? "xpr")
    (define*-public (print-window #&optional (win (get-window)))
      "Print WIN using xpr and lpr."
      (if win (execute (string-append "xwd -id " 
				      (number->string (window-id win))
				      " | xpr | lpr")))))

(define*-public (resize-window w h #&optional (win (get-window)) x y)
  "Resize WIN's client area to a size of W by H in pixels. 
The size does not include the window decorations -- only the client
application size. WIN defaults to the window context in the usual way
if not specified."
  (let* ((decor-sizes (window-decoration-size win))
	 (dw (car decor-sizes))
	 (dh (cadr decor-sizes)))
    (resize-frame (+ dw w) (+ dh h) win x y)))


;; Window sorting

;; Sort windows by position

(define*-public (sort-windows-by-middle-pos winlist #&key (horiz #t) (ascending #t))
  "Sort WINLIST (a list of windows) by their middle positioins.
Sort on horizontal position (x coordinate) if HORIZ is #t, otherwise
sort on vertical position (y coordiate) otherwise.  Sort in 
ascending order if ASCENDING is #t, otherwise descending."
  (let* ((assoclist (map (lambda (w) 
			   (let* ((pos (window-viewport-position w))
				  (size (window-frame-size w)))
			     (list (if horiz 
				       (+ (car pos) (car size)) 
				       (+ (cadr pos) (cadr size))) w)))
			 winlist))
	 (compare (if ascending < >))
	 (sortedlist (sort assoclist (lambda (l1 l2) (compare (car l1) (car l2))))))
    (map (lambda (t) (cadr t)) sortedlist)))


(define*-public (next-visible-non-iconified-window)
  "Switch focus to the next visible and not iconified window."
  (interactive)
  (next-window #:only (lambda (win) 
			(and (visible? win) 
			     (focussable-window? win)
			     (not (shaded-window? win))))
	       #:except iconified-window?))

(define*-public (prev-visible-non-iconified-window)
  "Switch focus to the previous visible and not iconified window."
  (interactive)
  (prev-window #:only (lambda (win) 
			(and (visible? win) (focussable-window? win)
			     (not (shaded-window? win))))
	       #:except iconified-window?))

(define*-public (resize-quarterscreen)
  "Resize the current window with the pointer to 1/4 of the screen."
  (interactive)
  (let ((w (window-with-pointer)))
    (animated-resize-window (%x 49) (%y 49))))

(define*-public (resize-halfscreen)
  "Resize the current window with the pointer to full height and half the screen size in width."
  (interactive)
  (let ((w (window-with-pointer)))
    (animated-resize-window (%x 49) (%y 90))))

(define*-public (resize-fullscreen)
  "Resize the current window with the pointer to 90% of the full screen size."
  (interactive)
  (let ((w (window-with-pointer)))
    (animated-resize-window (%x 90) (%y 90))))

(define anchor-cursor #f)
(let ((acimage (make-image "anchor-cursor.xpm")))
  (if acimage (set! anchor-cursor (create-pixmap-cursor acimage))))

(define*-public (interactive-set-window-gravity!)
  "Permit user to click on an area of a window and anchor that nonant.
E.g., if the user clicks on the northeast corner of a window, that
window will be set to have northeast gravity so future resizes keep
that corner fixed."
  (interactive)
  (let* ((win-pos (select-viewport-position anchor-cursor))
	 (win (car win-pos)))
    (if win
	(set-window-gravity! 
	 (nonant->gravity (get-window-nonant win-pos))
	 win))))

(define gravities #(northwest north northeast west center
			      east southwest south southeast))

(define-public (nonant->gravity nonant)
  "Return a gravity symbol given NONANT in [0,8].
0 is northwest, 1 is north, 2 is northeast, etc.
See also `get-window-nonant'."
  (if (array-in-bounds? gravities nonant)
      (array-ref gravities nonant)
      #f))

(define*-public (interactive-move-window-with-focus)
  "Interactively move the window which currently has the focus.
`*move-opaquely-proc*' is used to control whether a rubberband
outline or the window itself is moved."
  (interactive)
  (let ((w (window-with-focus))) (and w (interactive-move w))))

(define*-public (interactive-resize-window-with-focus)
  "Interactively resize the window which currently has the focus.
`*resize-opaquely-proc*' is used to control whether a rubberband
outline or the window itself is resized."
  (interactive)
  (let ((w (window-with-focus))) (and w (interactive-resize w))))

(define*-public (interactive-move-window-with-pointer)
  "Interactively move the window which currently contains the pointer.
`move-opaquely?' is used to control whether a rubberband
outline or the window itself is moved."
  (interactive)
  (let ((w (window-with-pointer))) (and w (interactive-move w))))

(define*-public (interactive-resize-window-with-pointer)
  "Interactively resize the window which currently contains the pointer.
`resize-opaquely?' is used to control whether a rubberband
outline or the window itself is resized."
  (interactive)
  (let ((w (window-with-pointer))) (and w (interactive-resize w))))

(define*-public (toggle-maximize-vertical #&optional (win (get-window)))
  "Toggle the current window's maximized-vertically state."
  (interactive)
  (toggle-maximize 0 (%y 100) win))

(define*-public (toggle-maximize-horizontal #&optional (win (get-window)))
  "Toggle the WIN's maximized-horizontally state."
  (toggle-maximize (%x 100) 0 win))

(define*-public (toggle-maximize-both #&optional (win (get-window)))
  "Toggle the WIN's maximization (both vertically and horizontally)."
  (interactive)
  (toggle-maximize (%x 100) (%y 100) win))

(define*-public (toggle-maximize-vertical-part #&optional (win (get-window)))
  "Toggle the WIN's maximization-vertically to 95% of the screen height."
  (interactive)
  (toggle-maximize 0 (%y 95) win))

(define*-public (maximize-vertical #&optional (win (get-window)))
  "Maximize WIN vertically."
  (interactive)
  (maximize 0 (%y 100) win))

(define*-public (maximize-horizontal #&optional (win (get-window)))
  "Maximize WIN horizontally."
  (interactive)
  (maximize (%x 100) 0 win))

(define*-public (maximize-both #&optional (win (get-window)))
  "Maximize WIN both horizontally and vertically."
  (interactive)
  (maximize (%x 100) (%y 100) win))

(define*-public (focus-change-warp-pointer #&optional (win (get-window)))
  "Deiconify, focus, raise, and warp-to WIN.
This is initially the default behaviour when WIN is selected from the window list."
  (interactive)
  (cond
   (win (deiconify-window win)
	(focus-window win)
	(raise-window win)
	(warp-to-window win)
	(let ((p (warp-placement win))) 
	  (move-pointer (w%x (car p) win) (w%y (cadr p) win))))))

;; GJB:FIXME:: this is ugly, but works around a problem
;; with cyclic dependences
(set! window-list-proc focus-change-warp-pointer)

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

(define*-public (toggle-focus)
  "Focus window that had the focus before the current one."
  (interactive)
  (focus-change-warp-pointer
   (cadr (list-windows #:by-focus #t))))
