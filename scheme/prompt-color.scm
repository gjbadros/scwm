;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros
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


(define-module (app scwm prompt-color)
  :use-module (app scwm gtk)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  :use-module (app scwm prompting-shell)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app scwm optargs))



#!
(define-scwm-option *favorite-colors*
  (list "inherit" "white" "grey20" "grey50" "grey75" "black" 
	"blue" "red" "green" "purple" "yellow" "orange")
  "The favorite colors to use in the drop-down box when picking a color."
  #:group 'theme
  #:type 'string-list
  )
!#


;;(use-modules (app scwm prompt-color))
;;(define w (prompt-color "Window color?" '(0 . 20) (lambda (v) (display v) (newline)) #:initval "navyblue"))
(define*-public (prompt-color prompt proc #&key
			      (initval #f)
			      (title "prompt-color")
			      (favorites #f))
  "Prompt using PROMPT for a color and call PROC with value if Ok is clicked.
INITVAL is a default initial color as a color object or string.
TITLE is a window title."
  (let* ((hbox-and-getter (prompt-color-hbox title initval favorites))
	 (hbox (car hbox-and-getter))
	 (getter (cadr hbox-and-getter)))
    (prompting-shell proc title hbox getter)))
  
(define (gdk-color-name color)
  (let ((r (gdk-color-red color))
	(g (gdk-color-green color))
	(b (gdk-color-blue color)))
    (string-append "rgbi:"
		   (apply string-append
			  (map (lambda (v) 
				 (string-append (number->string (/ v 65535)) "/"))
			       (list r g b))))))

;; (gdk-color-name (gdk-color-parse "red"))
;; (gdk-color-red (gdk-color-parse "red"))

(define*-public (prompt-color-hbox prompt initval #&optional favorites)
  "Create and return a color-prompting hbox, complete with link to full color dialog.
PROMPT is the prompt, INITVAL is the initial color as a color object or a string.
The returned value is a list: (hbox getter).
See also `prompt-color'."
  (or favorites
      (set! favorites 
	    (list "inherit" "white" "grey20" "grey50" "grey75" "black" 
		  "blue" "red" "green" "purple" "yellow" "orange")))
;;(optget *favorite-colors*)
  (let* ((hbox (gtk-hbox-new #f 0))
	 (cb (if (list? favorites) (gtk-combo-new) #f))
	 (entry (if cb (gtk-combo-entry cb) (gtk-entry-new)))
	 (initvalcolor (maybe-make-color initval))
	 (selbut (gtk-button-new-with-label "Pick..."))
	 (entry-init (if initvalcolor (color-property initvalcolor 'name) "inherit"))
	 (label (gtk-label-new prompt)))
    (if cb
	(gtk-combo-set-popdown-strings cb
				       (list->vector favorites)))
    (gtk-entry-set-text entry entry-init)
    (gtk-box-pack-start hbox label #f #f 10)
    (gtk-box-pack-start hbox (or cb entry) #t #t)
    (gtk-box-pack-start hbox selbut #f #f 10)
    (gtk-widget-set-usize entry (min 450 (max 100 (* 10 (string-length entry-init)))) 30)
    (gtk-widget-show-all hbox)
    (gtk-signal-connect selbut "clicked"
			(lambda ()
			  (let* ((dialog (gtk-color-selection-dialog-new
					  "Color Selection Dialog"))
				 (c (gdk-color-parse (gtk-entry-get-text entry))))
			    (if c
				(gtk-color-selection-set-color
				 (gtk-color-selection-dialog-colorsel dialog) c))
			    (gtk-signal-connect
			     (gtk-color-selection-dialog-ok-button dialog)
			     "clicked" (lambda () 
					 (gtk-entry-set-text 
					  entry 
					  (gdk-color-name
					   (gtk-color-selection-get-color 
					    (gtk-color-selection-dialog-colorsel dialog))))
					 (gtk-widget-destroy dialog)))
			    (gtk-signal-connect
			     (gtk-color-selection-dialog-cancel-button dialog)
			     "clicked" (lambda () (gtk-widget-destroy dialog)))
			    (gtk-widget-show dialog))))
    (list hbox (lambda () (maybe-make-color (gtk-entry-get-text entry))) entry)))
