;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros, Jeff W. Nichols
;;;; Modified from ScwmButtons.scm, Copyright (C) 1998 Maciej Stachowiak
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



(define-module (app scwm ui-constraints-buttons)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app scwm base)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm ui-constraints-classes)
  :use-module (app scwm ui-constraints-composition)
  :use-module (app scwm optargs))

;; (load "/home/gjb/scwm/scheme/ui-constraints-buttons.scm")
;; (load "/home/gjb/scwm/scheme/constraints.scm")
;; (set-current-module the-root-module)
;; (start-ui-constraints-buttons)


;; example usage: 
;; (window-style (resource-match?? "ScwmUIConstraintsButtons") #:use-style desk-widget-on-top-no-titlebar)
;; (use-modules (app scwm ui-constraints-buttons))
;; (start-ui-constraints-buttons)
;; other testing:
;; (use-modules (app scwm ui-constraints))
;; (draw-all-constraints)

;; define some PRIVATE variables

(define buttons-toplevel #f)
(define buttons-box      #f)

(define buttons-tooltips (gtk-tooltips-new))

(define toggle-pixmap #t)
(define toggle-vertical #f)
(define toggle-initialized #f)

(define animate-pixmaps-delay 1000)  ;; in ms
(define animating? #f)

(define-public ui-constraints-buttons-animate-pixmaps #t)
;; (set! ui-constraints-buttons-animate-pixmaps #t)

(define (animate-pixmaps button pixmap1 pixmap2)
  (define (show1) 
    (if animating?
	(begin
	  (gtk-container-remove button (car (gtk-container-children button)))
	  (gtk-container-add button pixmap1)
	  (gtk-widget-show pixmap1)))
    (add-timer-hook! animate-pixmaps-delay show2))
  (define (show2) 
    (if animating?
	(begin
	  (gtk-container-remove button (car (gtk-container-children button)))
	  (gtk-container-add button pixmap2)
	  (gtk-widget-show pixmap2)))
    (add-timer-hook! animate-pixmaps-delay show1))
  (show1))

;; (get-timer-hooks-list)
;; private function to make a class button

(define (make-class-button class pixmap?)
  (let* ((name   (ui-constraint-class-name class))
	 (description (ui-constraint-class-description class))
	 (button (gtk-button-new))
	 (label  (gtk-label-new name))
	 (pixmap-name (ui-constraint-class-pixmap-name class))
	 (pixmap2-name (ui-constraint-class-pixmap2-name class))
	 (pixmap (if (and pixmap? pixmap-name)
		     (gtk-pixmap-new-search-scwm-path pixmap-name button) #f))
	 (pixmap2 (if (and pixmap? pixmap2-name)
		     (gtk-pixmap-new-search-scwm-path pixmap2-name button) #f)))
    (gtk-tooltips-set-tip buttons-tooltips button description "")
    (gtk-container-add button (if (and pixmap? pixmap)
				  pixmap label))
    (if pixmap (gtk-widget-show pixmap) (gtk-widget-show label))
    (if (and pixmap pixmap2 ui-constraints-buttons-animate-pixmaps) (animate-pixmaps button pixmap2 pixmap))
    (set-object-property! class 'gtk-button button)
    (gtk-signal-connect button "clicked"
			(lambda ()
			  (let ((uic (make-ui-constraint-interactively class)))
			    (and uic (enable-ui-constraint uic)))))
    (gtk-container-add buttons-box button)
    (gtk-widget-show button)
    button))

(define* (add-extra-button name pixmap-name description click-action #:optional (show? #t))
  (let* ((button (gtk-button-new))
	 (label (gtk-label-new name))
	 (pixmap (gtk-pixmap-new-search-scwm-path pixmap-name button)))
    (gtk-tooltips-set-tip buttons-tooltips button description "")
    (if pixmap
	(gtk-container-add button pixmap)
	(gtk-container-add button label))
    (if click-action (gtk-signal-connect button "clicked" click-action))
    (gtk-container-add buttons-box button)
    (if show?
	(gtk-widget-show-all button))
    button))


;; the simple hook function to add new constraints
;; (defined so we can remove it later)

(define (add-hook-func class)
  (make-class-button class toggle-pixmap))

(define (remove-hook-func class)
  (let ((button (object-property class 'gtk-button)))
    (gtk-container-remove buttons-box button)
    (gtk-widget-hide button)))

;; initialize the buttons window
;; it would be nice to add a constraint class remove options

(define*-public (initialize-ui-constraints-buttons #:key (vertical #f) (pixmap #t) (show #f))
  (let* ((toplevel (gtk-window-new 'toplevel))
	 (box (if vertical (gtk-vbutton-box-new) (gtk-hbutton-box-new)))
	 (ui-constraint-classes global-constraint-class-list))
    (if toggle-initialized (gtk-widget-hide buttons-toplevel))
    (set! buttons-toplevel toplevel)
    (set! buttons-box box)
    (set! toggle-pixmap pixmap)
    (set! toggle-vertical vertical)
    (remove-hook! constraint-class-add-hook add-hook-func)
    (remove-hook! constraint-class-delete-hook remove-hook-func)
    (add-hook-once! constraint-class-add-hook add-hook-func)
    (add-hook-once! constraint-class-delete-hook remove-hook-func)
    (gtk-window-set-title toplevel "ScwmUIConstraintsButtons")
    (gtk-window-set-wmclass toplevel "ScwmUIConstraintsButtons" "Scwm")
    (gtk-button-box-set-spacing box 0)
    (gtk-button-box-set-child-ipadding box 0 0)
    (gtk-button-box-set-child-size box 32 32)
    (gtk-box-set-homogeneous box #f)
    (let ((stop-composition-button
	   (add-extra-button "Stop composition" "stop-composition.xpm" 
			     "Stop recording composition of constraints."
			     #f #f))
	  (start-composition-button
	   (add-extra-button "Start composition" "record-composition.xpm" 
			     "Begin recording composition of constraints."
			     #f)))
      (gtk-signal-connect start-composition-button "clicked"
			  (lambda () 
			    (ui-constraints-composition-begin)
			    (gtk-widget-hide start-composition-button)
			    (gtk-widget-show-all stop-composition-button)))
      (gtk-signal-connect stop-composition-button "clicked"
			  (lambda () 
			    (ui-constraints-composition-end)
			    (gtk-widget-show-all start-composition-button)
			    (gtk-widget-hide stop-composition-button))))
    (for-each (lambda (class) (make-class-button class pixmap)) ui-constraint-classes)
    (gtk-container-add toplevel box)
    (gtk-widget-show box)
    (gtk-signal-connect toplevel "delete_event" (lambda (args) (gtk-widget-hide toplevel)))
    (gtk-signal-connect toplevel "enter_notify_event" (lambda (args) (set! animating? #t)))
    (gtk-signal-connect toplevel "leave_notify_event" (lambda (args) (set! animating? #f)))
    (set! toggle-initialized #t)
    (gtk-window-set-policy toplevel #t #t #t)
    (if show (gtk-widget-show toplevel))))
	 

;; change the existing window
	 
(define*-public (change-ui-constraints-buttons #:key (vertical toggle-vertical) (pixmap toggle-pixmap))
  (initialize-ui-constraints-buttons #:vertical vertical #:pixmap pixmap #:show #t))

;; open the buttons window

(define-public (start-ui-constraints-buttons)
  (if (not buttons-toplevel)
      (initialize-ui-constraints-buttons))
  (gtk-widget-show buttons-toplevel))


;; close the buttons window

(define-public (close-ui-constraints-buttons)
  (gtk-widget-hide buttons-toplevel))


;; (use-scwm-modules constraints ui-constraints gtk optargs ui-constraints-composition constraint-investigator)
;; (use-modules (gtk gtk) (gtk gdk))

;; (set! toggle-initialized #f)
;; (start-constraints)
;; (constraint-investigator)
;; (initialize-ui-constraints-buttons #:vertical #f #:pixmap #t)
;; (start-ui-constraints-buttons)
;; (change-ui-constraints-buttons #:vertical #t)
;; (change-ui-constraints-buttons #:pixmap #f)
;; (initialize-ui-constraints-buttons #:vertical #t #:pixmap #t)
;; (close-ui-constraints-buttons)
;; (for-each (lambda (n) (execute "xlogo")) '(1 2 3))
;; (ui-constraints-composition-begin)
;; (ui-constraints-composition-end)
;; (car global-constraint-class-list)
;; (gtk-widget-hide buttons-toplevel)
;; toggle-initialized
