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
;;;(use-modules (app scwm ScwmButtons))
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
;;;(run-ScwmButtons
;;; (list
;;;  (button-item "mini-term.xpm" #:action "xterm")
;;;  (button-item "mini-calc.xpm" #:action "xcalc")
;;;  (button-item "mini-xmcd.xpm" #:action "xmcd")
;;;  (button-item "mini-xv.xpm" #:action "xv")
;;;  (button-item "mini-gv.xpm" #:action "gv")
;;;  (button-item "mini-nscape.xpm" #:action "/usr/netscape/netscape")
;;;  ) #:orientation 'vertical)
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

(define*-public
  (run-ScwmButtons button-item-list #&key (orientation 'horizontal)
		   (name "ScwmButtons"))
  (let* ((toplevel (gtk-window-new 'toplevel))
	 (tooltip (gtk-tooltips-new))
	 (box (case orientation
		((horizontal) (gtk-hbox-new 0 0))
		((vertical) (gtk-vbox-new 0 0))))
	 )
    (gtk-window-set-title toplevel name)
    (gtk-window-set-wmclass toplevel name "Scwm")
    (for-each (lambda (f)
		(let* ((button (gtk-button-new))
		       (imagepath (find-file-in-path (car f) image-load-path))
		       (tooltipstr (caddr f)))
		       (if (string? imagepath)
			   (let* ((image (gtk-pixmap-new imagepath button)))
			     (gtk-widget-show image)
			     (gtk-widget-show button)
			     (gtk-container-add button image)
			     (if (string? tooltipstr)
				 (gtk-tooltips-set-tip tooltip button
						       tooltipstr ""))
			     (gtk-signal-connect button "clicked"
						 (cadr f))
			     (gtk-box-pack-start box button)))))
		button-item-list)
    (gtk-container-add toplevel box)
    (gtk-tooltips-enable tooltip)
    (gtk-widget-show box)
    (gtk-widget-show toplevel)
    (lambda () 
	(if (not (gtk-object-destroyed toplevel))
	    (gtk-widget-unmap toplevel)
	    (gtk-widget-destroy toplevel)))))

(define-public (close-ScwmButtons sb)
  (sb))

(define*-public (button-item pixmap-file #&key (action noop) tooltip)
  (if (string? action)	;; permit "xterm" to mean (execute "xterm")
      (let ((program-name action))
	(set! action (lambda () (execute program-name)))))
  (list pixmap-file action tooltip))
