;;;; $Id$ -*-scwm-*-
;;;; Copyright (C) 1999 Glenn Trigg
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

;;;;     Example usage.
;;;;
;;;(use-scwm-modules ScwmButtons)
;;;
;;; Start a mini-button bar using ScwmButtons
;;; (run-ScwmButtons
;;;  (list
;;;   (button-item "mini-term.xpm" #:action "xterm" #:tooltip "XTerm")
;;;   (button-item "mini-calc.xpm" #:action "xcalc" #:tooltip "XCalc")
;;;   (button-item "mini-xmcd.xpm" #:action "xmcd" #:tooltip "Xmcd")
;;;   (button-item "mini-xv.xpm" #:action "xv" #:tooltip "Xv")
;;;   (button-item "mini-gv.xpm" #:action "gv" #:tooltip "gv")
;;;   (button-item "mini-nscape.xpm" #:action "netscape" #:tooltip "Netscape")
;;;   ))
;;;
;;;;  The above will produce a horizontal bar, the below a vertical bar.
;;;

;;; GJB:FIXME:GT:
;;; Bugs:
;;; Two ScwmButtons, move one near an edge, both re-orient
;;;
;;; Grab a ScwmButtons window by it's lower right, when it reorients
;;;   your grab is no longer touching the window.

#!
;; ScwmButtons

(define btns
  (run-ScwmButtons
   (list
    (button-item "mini-term.xpm" #:action "xterm" #:tooltip "XTerm")
    (button-item "mini-calc.xpm" #:action "xcalc" #:tooltip "XCalc")
    (button-item "mini-xmcd.xpm" #:action "xmcd" #:tooltip "Xmcd")
    (button-item "mini-xv.xpm" #:action "xv" #:tooltip "Xv")
    (button-item "mini-gv.xpm" #:action "gv" #:tooltip "gv")
    (button-item "mini-nscape.xpm" #:action "netscape" #:tooltip "Netscape"))
    #:orientation 'vertical #:name "MiniButtons"
    #:auto-orient #t
   ))
(btns 'orientation 'horizontal)
(btns 'orientation 'vertical)
(btns 'quit)
!#
;;;
;;;; Also the name of the buttonbar can be specified for setting specific
;;;; styles etc.
;;;
;;;(run-ScwmButtons
;;; (list
;;;  (button-item "mini-term.xpm" #:action "xterm")
;;;  (button-item "mini-calc.xpm" #:action "xcalc")
;;;  (button-item "mini-xmcd.xpm" #:action "xmcd")
;;;  (button-item "mini-xv.xpm" #:action "xv")
;;;  (button-item "mini-gv.xpm" #:action "gv")
;;;  (button-item "mini-nscape.xpm" #:action "/usr/netscape/netscape")
;;;  ) #:name "miniButtons")

(define-module (app scwm ScwmButtons)
  :use-module (app scwm base)
  :use-module (app scwm file)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app scwm optargs))

(define*-public (run-ScwmButtons button-item-list #&key (orientation 'horizontal)
				 (name "ScwmButtons") (auto-orient #t)
				 (auto-orient-margin 100))
  "Start a ScwmButtons window giving a simple toolbar button/action interface.
ORIENTATION can be either 'horizontal or 'vertical;
NAME is the name of the window.
AUTO-ORIENT is #t if you wish the window to switch orientations when
the window approaches an edge of the viewport, and AUTO-ORIENT-MARGIN
determines how wide the \"edge\" of a viewport is (in pixels)."
  (let* ((toplevel (gtk-window-new 'toplevel))
	 (toolbar (case orientation
		((horizontal) (gtk-toolbar-new 'horizontal 'icons))
		((vertical) (gtk-toolbar-new 'vertical 'icons))))
	 (current-orientation orientation)
	 )
    (gtk-window-set-title toplevel name)
    (gtk-window-set-policy toplevel #t #t #t)
    (gtk-widget-set-name toolbar name)
    (gtk-window-set-wmclass toplevel "ScwmButtons" "Scwm")
    (for-each (lambda (f)
		(let* ((button (gtk-button-new))
		       (imagepath (find-file-in-path (car f) image-load-path))
		       (tooltipstr (caddr f)))
		  (if (not (string? tooltipstr))
		      (set! tooltipstr ""))
		  (if (string? imagepath)
		      (let* ((image (gtk-pixmap-new imagepath button)))
			(gtk-container-add button image)
			(gtk-signal-connect button "clicked"
					    (cadr f))
			(gtk-toolbar-append-widget toolbar button
						   tooltipstr "")))))
	      button-item-list)
    (gtk-container-add toplevel toolbar)
    (gtk-toolbar-set-tooltips toolbar #t)
    (gtk-widget-show-all toplevel)

    (letrec ((imnph 
	      (lambda (win x y)
		(define xclose (min (abs (- x (car (display-size)))) (abs x)))
		(define yclose (min (abs (- y (cadr (display-size)))) (abs y)))
;;		(display xclose) (display ", ") (display yclose) (newline)
		(if (or (< xclose auto-orient-margin) (< yclose auto-orient-margin))
		    (if (< xclose yclose)
			(if (equal? current-orientation 'horizontal)
			    (begin
			      (gtk-toolbar-set-orientation toolbar 'vertical)
			      (set! current-orientation 'vertical)))
			(if (equal? current-orientation 'vertical)
			    (begin
			      (gtk-toolbar-set-orientation toolbar 'horizontal)
			      (set! current-orientation 'horizontal)))))
		(gdk-flush)
		(X-server-synchronize)))

	     (imfh
	      (lambda (win)
		(if (string=? (window-resource win) "ScwmButtons")
		    (begin
		      (remove-hook! interactive-move-new-position-hook imnph)))))

	     (imsh
	      (lambda (win)
		(if (string=? (window-resource win) "ScwmButtons")
		    (begin
		      (add-hook! interactive-move-new-position-hook imnph)))))

	     (handle
	      (lambda (action . args)
		(case action
		  ((quit)
		   (if (not (gtk-object-destroyed toplevel))
		       (begin
			 (gtk-widget-unmap toplevel)
			 (gtk-widget-destroy toplevel)
			 (if auto-orient
			     (begin
			       (remove-hook! interactive-move-start-hook imsh)
			       (remove-hook! interactive-move-finish-hook imfh)))
			 )))
		  ((orientation)
		   (gtk-toolbar-set-orientation toolbar (car args)))
		  ((tooltips)
		   (gtk-toolbar-set-tooltips toolbar (car args)))
		  ((add-child)
		   (gtk-toolbar-append-widget
		    toolbar
		    (car args) (if (string? (cadr args)) (cadr args) ("")) ""))
		  ((add-space)
		   (gtk-toolbar-append-space toolbar))
		  ))))
      (if auto-orient
	  (begin
	    (add-hook! interactive-move-start-hook imsh)
	    (add-hook! interactive-move-finish-hook imfh)))
      handle
      )))

(define-public (close-ScwmButtons sb)
  "Close the ScwmButtons window of SB.
SB should be a handle returned from `run-ScwmButtons'."
  (sb 'quit))

(define*-public (button-item pixmap-file #&key (action noop) (tooltip #f))
  "Create a button item for a ScwmButtons window."
  (if (string? action)	;; permit "xterm" to mean (execute "xterm")
      (let ((program-name action))
	(set! action (lambda () (execute program-name)))))
  (list pixmap-file action tooltip))
