;;;; $Id$
;;;; Copyright (C) 1997-1998 Maciej Stachowiak and Greg J. Badros
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
  :use-module (app scwm base)
  :use-module (app scwm style-options))





;;; Toggling operations

(define*-public ((make-toggling-winop pred neg pos) 
		 #&optional (win (get-window)))
  "Returns a procedure which takes a window WIN and toggles a property of it.
PRED, NEG, and POS should be functions which take a window and
check whether the property holds for the window, reset the property
on the window, and set the property on the window, respectively."
  (if win (if (pred win)
	      (neg win)
	      (pos win))))

(define*-public (close-window #&optional (win (get-window #t)))
  "Close WIN either by deleting it or destroying it.
WIN is only destroyed if it is not deleteable."
  (if win (if (window-deletable? win)
	    (delete-window win)
	    (destroy-window win))))
 
(define-public toggle-raise
  (make-toggling-winop raised? lower-window raise-window))

(define-public toggle-iconify
  (make-toggling-winop iconified? deiconify iconify))

(define-public toggle-stick 
  (make-toggling-winop sticky? unstick stick))

(define-public toggle-window-shade 
  (make-toggling-winop window-shaded? window-unshade window-shade))


(define-public toggle-on-top
  (make-toggling-winop kept-on-top? un-keep-on-top keep-on-top))

(define-public toggle-titlebar
  (make-toggling-winop titlebar-shown? hide-titlebar show-titlebar))

(define-public toggle-border
  (make-toggling-winop border-normal? plain-border normal-border))

(define-public toggle-stick-icon
  (make-toggling-winop icon-sticky? unstick-icon stick-icon))


;;; Maximization

(define*-public (maximize nw nh #&optional (win (get-window)))
"Maximize WIN to new pixel width NW and new pixel height NH.
If NW or NH is 0, that dimension is not changed."
(if win (let* ((pos (window-viewport-position win))
	     (x (car pos))
	     (y (cadr pos))
	     (frame-size (window-frame-size win))
	     (pix-width (car frame-size))
	     (pix-height (cadr frame-size))
	     (cli-size (window-size win))
	     (cli-width (caddr cli-size))
	     (cli-height (cadddr cli-size)))
	(resize-frame-to (if (> nw 0) nw pix-width)
			 (if (> nh 0) nh pix-height) win)
	;; above is just a hint, get the actual...
	;; FIXGJB: race conditions?
	(let* ((new-frame-size (window-frame-size win))
	       (new-client-size (window-size win))
	       (nfw (car new-frame-size))
	       (nfh (cadr new-frame-size))
	       (ncw (caddr new-client-size))
	       (nch (cadddr new-client-size))
	       (nx (cond
		    ((> 0 x) 0)
		    ((> display-width (+ x nfw)) x)
		    ((> display-width nfw) (- display-width nfw))
		    (#t 0)))
	       (ny (cond
		    ((> 0 y) 0)
		    ((> display-height (+ y nfh)) y)
		    ((> display-height nfh) (- display-height nfh))
		    (#t 0))))
	  (move-window-viewport-position nx ny win)
	  (if (not (maximized? win))
	      (set-window-property!
	       win 'maximized (list x y cli-width cli-height
				    nx ny ncw nch)))))))


(define*-public (maximized? #&optional (win (get-window)))
  "Return #t if WIN is maximized, #f otherwise."
  (->bool (window-property win 'maximized)))

;; uses client units
(define*-public (unmaximize #&optional (win (get-window)))
  "Unmaximize WIN so it returns to its size/position before maximization."
  (if win (let ((max-prop (window-property win 'maximized)))
	       (cond
		(max-prop
		 (let* ((maxed-dims (cddddr max-prop))
			(maxed-x (car maxed-dims))
			(maxed-y (cadr maxed-dims))
			(maxed-width (caddr maxed-dims))
			(maxed-height (cadddr maxed-dims))
			(cur-pos (window-viewport-position win))
			(cur-size (window-size win))
			(cur-x (car cur-pos))
			(cur-y (cadr cur-pos))
			(cur-width (caddr cur-size))
			(cur-height (cadddr cur-size))
			(size-hints (cddr (window-size-hints win))))
		       (if (and (= cur-x maxed-x) (= cur-y maxed-y))
			   (move-window-viewport-position
			    (car max-prop) (cadr max-prop) win))
		       (resize-to
			(+ (* (if (= maxed-width cur-width)
				  (caddr max-prop)
				  cur-width)
			      (caar size-hints))
			   (caadr size-hints))
			(+ (* (if (= maxed-height cur-height)
				  (cadddr max-prop)
				  cur-height)
			      (cdar size-hints))
			   (cdadr size-hints))
			win)
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


(define-public opaque-move-percent 
;;;**VAR
;;; Percent of display area below which windows are moved opaquely.
  50)

(define-public opaque-resize-percent 
;;;**VAR
;;; Percent of display area below which windows are resized opaquely.
  35)


(define-public (window-frame-area win)
  "Return the area of WIN's frame in square pixels."
  (let* ((frame-size (window-frame-size win))
	 (width (car frame-size))
	 (height (cadr frame-size)))
    (* width height)))

(define display-area (* display-width display-height))

(define-public (default-resize-opaquely? win)
  "Return #t if WIN has area <= opaque-resize-percent of the screen, else #f."
  (<= (window-frame-area win) 
     (* display-area (/ (scwm-user-var opaque-resize-percent) 100))))

(define-public (default-move-opaquely? win)
  "Return #t if WIN has area <= opaque-move-percent of the screen, else #f."
  (<= (window-frame-area win)
     (* display-area (/ (scwm-user-var opaque-move-percent) 100))))


(define-public move-opaquely?
;;;**VAR
;;; User-settable predicate to determine if windows should be moved opaquely.
 default-move-opaquely?)

(define-public resize-opaquely?
;;;**VAR
;;; User-settable predicate to determine if windows should be resized opaquely.
 default-resize-opaquely?)

(define*-public (interactive-move #&optional (win (get-window #f #t #f))
				  (opaquely? (if win (move-opaquely? win))))
  "Move WINDOW interactively and possibly opaquely. 
If OPAQUELY? is specified, it is used to determine if the window
should be moved opaquely, or using a rubber-band. If it is not
spcified, `interactive-move' calls `move-opaquely?' on WIN and moves
opaquely if that returns #t and uses a rubber-band if it returns #f."
  (if win ((if opaquely? opaque-move rubber-band-move) win)))

(define*-public (interactive-resize #&optional (win (get-window #f #t #f))
				    (opaquely? (if win 
						   (resize-opaquely? win))))
  "Resize WINDOW interactively and possibly opaquely. 
If OPAQUELY? is specified, it is used to determine if the window
should be resized opaquely, or using a rubber-band. If it is not
spcified, `interactive-resize' calls `resize-opaquely?' on WIN and
moves opaquely if that returns #t and uses a rubber-band if it returns
#f."
  (if win ((if opaquely? opaque-resize rubber-band-resize) win)))

;;; hack to work with minimal.scm
(set! hack-interactive-move interactive-move)
(set! hack-interactive-resize interactive-resize)


;; (move-opaquely? (select-window-interactively))
;; (resize-opaquely? (select-window-interactively))
;; (interactive-move)



;; Printing
(define*-public (print-window #&optional (win (get-window)))
  "Print WIN using xpr and lpr."
  (if win (execute (string-append "xwd -id " 
				(number->string (window-id win))
				" | xpr | lpr"))))



;; FIXGJB: the resize-to primitive should just be renamed
(define*-public (resize-window w h #&optional (win (get-window)))
  "Resize WIN's client area to a size of W by H in pixels. 
The size does not include the window decorations -- only the client
application size. WIN defaults to the window context in the usual way
if not specified."
  (resize-to w h win))
