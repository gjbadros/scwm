;;;; $Id$
;;;; Copyright (C) 2000  Jeff W. Nichols, Greg J. Badros
;;;; jwnichls@cs.washington.edu, gjb@cs.washington.edu
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

;; scwm-gtk-pager
;; (initialize-pager #:width 64 #:height 64)
;; (show-pager)
;; (draw-pager)
;; (hide-pager)
;; (gtk-widget-show scwm-pager-window)
;; (gtk-widget-hide scwm-pager-window)
;; (define mystery '())

(define-module (app scwm scwm-gtk-pager)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app scwm base)
  :use-module (app scwm winlist)
  :use-module (app scwm window-selection)
  :use-module (app scwm wininfo)
  :use-module (app scwm optargs))

;; (use-modules (gtk gtk) (gtk gdk) (app scwm gtk) (app scwm winlist) (app scwm wininfo) (app scwm optargs))

(define-public scwm-pager-window #f)
(define-public scwm-pager-drawarea #f)
(define-public scwm-pager-initialized? #f)
(define-public scwm-pager-sig-num 0)

(define-public scwm-pager-size-w 0)
(define-public scwm-pager-size-h 0)
(define-public scwm-pager-offscreen #f)

(define*-public (initialize-pager #:key (parent #f) (width 10) (height 10))
  "Initialize the pager."
  (set! scwm-pager-drawarea (gtk-drawing-area-new))
  (let ((drawarea scwm-pager-drawarea))
    (gtk-drawing-area-size drawarea width height)
    (if (not parent)
	(begin
	  (set! scwm-pager-window (gtk-window-new 'toplevel))
	  (gtk-window-set-title scwm-pager-window "ScwmPager")
	  (gtk-container-add scwm-pager-window drawarea))
	(parent 'add-child drawarea "Pager"))
    (gtk-widget-show drawarea)
    (if (not scwm-pager-window)
	(gtk-signal-connect scwm-pager-drawarea "delete_event"
			(lambda (args) (hide-pager)))
	(gtk-signal-connect scwm-pager-window "delete_event"
			    (lambda (args) (hide-pager))))
    (set! scwm-pager-initialized? #t)))

(define-public (show-pager)
  (if (not scwm-pager-initialized?) (initialize-pager))
  (if (not scwm-pager-window)
      (gtk-widget-show scwm-pager-drawarea)
      (gtk-widget-show scwm-pager-window))
  (really-draw-pager)
  (set-pager-hooks))

(define-public (hide-pager)
  (if scwm-pager-window
      (begin
	(gtk-widget-hide scwm-pager-window)
	(unset-pager-hooks))
      (begin
	(gtk-widget-hide scwm-pager-drawarea)
	(unset-pager-hooks))
      ))

(define-public (kill-pager)
  (if scwm-pager-initialized?
      (begin
	(if scwm-pager-window
	    (gtk-widget-destroy scwm-pager-window)
	    (gtk-widget-destroy scwm-pager-drawarea))
	(unset-pager-hooks))
      ))

(define hooks-for-pager
  (list
   broadcast-hook
   broadcast-config-hook))
;;;   desk-size-change-hook after-new-window-hook
;;;   after-viewport-position-change-hook deiconify-hook iconify-hook
;;;   interactive-move-finish-hook interactive-resize-finish-hook
;;;   interactive-move-new-position-hook interactive-resize-new-size-hook
;;;   window-close-hook window-focus-change-hook after-change-desk-hook
;;;   window-selection-add-hook window-selection-remove-hook))

(define (set-pager-hooks)
  (set! scwm-pager-sig-num (gtk-signal-connect scwm-pager-drawarea "expose-event" draw-pager))
  (for-each 
   (lambda (hook)
     (add-hook! hook draw-pager))
   hooks-for-pager))

(define (unset-pager-hooks)
  (gtk-signal-disconnect scwm-pager-drawarea scwm-pager-sig-num)
  (for-each 
   (lambda (hook)
     (remove-hook! hook draw-pager))
   hooks-for-pager))

(define-public need-to-draw-pager #f)

(define-public (draw-pager . ignore)
  (if (not need-to-draw-pager)
      (begin
	(set! need-to-draw-pager #t)
	(add-timer-hook! 0 really-draw-pager))))

;; (get-timer-hooks-list)
(define-public (really-draw-pager . ignore)
  (set! need-to-draw-pager #f)
  (let* ((focuswin (window-with-focus))
	 (gdkwin (gtk-widget-window scwm-pager-drawarea))
	 (sgc (if gdkwin (gdk-gc-new gdkwin) '()))
	 (size (gdk-window-get-size gdkwin)))
    (if (or (not (equal? (car size) scwm-pager-size-w))
	    (not (equal? (cdr size) scwm-pager-size-h)))
	(begin
	  (set! scwm-pager-size-w (car size))
	  (set! scwm-pager-size-h (cdr size))
	  (set! scwm-pager-offscreen (gdk-pixmap-new gdkwin (car size) (cdr size)))))
    (let* ((offscreen scwm-pager-offscreen)
	   (gc (if offscreen (gdk-gc-new offscreen) '()))
	   (desksize (desk-size))
	   (dispsize (map (lambda (disp desk) (* disp desk)) (display-size) desksize))
	   (listcor (list (/ (car size) (car dispsize)) (/ (cdr size) (cadr dispsize))))
	   (vptop (map (lambda (v c) (round (* v c))) (viewport-position) listcor))
	   (vpsize (map (lambda (s c) (round (* s c))) (display-size) listcor)))
      (if (and gc sgc) 
	  (letrec ((hiter 
		    (lambda (i)
		      (if (< i (car desksize))
			  (let ((iter (quotient (car size) (car desksize))))
			    (gdk-draw-line offscreen gc (* iter i) 0 (* iter i) (cdr size))
			    (hiter (+ i 1))))))
		   (viter 
		    (lambda (i)
		      (if (< i (cadr desksize))
			  (let ((iter (quotient (cdr size) (cadr desksize))))
			    (gdk-draw-line offscreen gc 0 (* iter i) (car size) (* iter i))
			    (viter (+ i 1)))))))
	    (gdk-gc-set-foreground gc "lightgrey")
	    (gdk-draw-rectangle offscreen gc #t 0 0 (car size) (cdr size))
	    (gdk-gc-set-foreground gc "yellow")
	    (gdk-draw-rectangle offscreen gc #t (car vptop) (cadr vptop) (car vpsize) (cadr vpsize))
	    (gdk-gc-set-foreground gc "black")
	    (gdk-gc-set-line-attributes gc 1 'on-off-dash 'butt 'miter)
	    (hiter 1)
	    (viter 1)
	    (gdk-gc-set-line-attributes gc 1 'solid 'butt 'miter)
	    (for-each (lambda (win)
			(if (window-valid? win)
			    (let* ((winsize (window-frame-size win))
				   (winpos (window-position win))
				   (drawpos (if (sticky-window? win)
						(let ((vwloc (viewport-position)))
						  (map (lambda (p c v) (round (* (+ p v) c))) winpos listcor vwloc))
						(map (lambda (p c) (round (* p c))) winpos listcor)))
				   (drawsize (map (lambda (s c) (round (* s c))) winsize listcor)))
			      ;;			    (gtk-show-error (string-append "window " (window-title win) "\n"))
			      (gdk-gc-set-foreground gc
						     (color->string
						      (window-background-color win)))
			      (gdk-draw-rectangle offscreen gc #t (car drawpos) 
						  (cadr drawpos) (car drawsize) (cadr drawsize))
			      (gdk-gc-set-foreground gc "black")
			      (gdk-draw-rectangle offscreen gc #f (car drawpos) 
						  (cadr drawpos) (car drawsize) (cadr drawsize))))) 
		      (list-windows #:only on-current-desk? #:by-stacking #t #:reverse #t))
	    (gdk-draw-pixmap gdkwin sgc offscreen 0 0 0 0 (car size) (cdr size)))
	  (gtk-show-error "couldn't make gc")))))
  

    
;; (set-desk-size! 4 4)
;; (set-desk-size! 3 3)
