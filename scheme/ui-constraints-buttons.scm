;;;; $Id$
;;;; Copyright (C) 1999 Greg J. Badros
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
  :use-module (app scwm ui-constraints)
  :use-module (app scwm ui-constraints-classes)
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


;; private function to make a class button

(define (make-class-button class pixmap?)
  (let* ((name   (ui-constraint-class-name class))
	 (button (gtk-button-new))
	 (label  (gtk-label-new name))
	 (pixmap (if pixmap? (gtk-pixmap-new-search-scwm-path (ui-constraint-class-pixmap-name class) 
							      button) #f)))
    (gtk-tooltips-set-tip buttons-tooltips button name "")
    (if pixmap?
	(if pixmap (gtk-container-add button pixmap))
	(gtk-container-add button label))
    (if pixmap (gtk-widget-show pixmap) (gtk-widget-show label))
    (gtk-signal-connect button "clicked"
			(lambda ()
			  (enable-ui-constraint (make-ui-constraint-interactively class))))
    (gtk-container-add buttons-box button)
    (gtk-widget-show button)
    button))


;; the simple hook function to add new constraints
;; (defined so we can remove it later)

(define (hook-func class)
  (make-class-button class toggle-pixmap))

;; initialize the buttons window
;; it would be nice to add a constraint class remove options

(define*-public (initialize-ui-constraints-buttons #&key (vertical #f) (pixmap #t) (show #f))
  (let* ((toplevel (gtk-window-new 'toplevel))
	 (box (if vertical (gtk-vbutton-box-new) (gtk-hbutton-box-new)))
	 (ui-constraint-classes global-constraint-class-list))
    (if toggle-initialized (gtk-widget-hide buttons-toplevel))
    (set! buttons-toplevel toplevel)
    (set! buttons-box box)
    (set! toggle-pixmap pixmap)
    (set! toggle-vertical vertical)
    (remove-constraint-class-add-hook! hook-func)
    (add-constraint-class-add-hook! hook-func)
    (gtk-window-set-title toplevel "ScwmUIConstraintsButtons")
    (gtk-window-set-wmclass toplevel "ScwmUIConstraintsButtons" "Scwm")    
    (gtk-button-box-set-spacing box 0)
    (gtk-button-box-set-child-ipadding box 0 0)
    (gtk-button-box-set-child-size box 32 32)
    (for-each (lambda (class) (make-class-button class pixmap)) ui-constraint-classes)
    (gtk-container-add toplevel box)
    (gtk-widget-show box)
    (gtk-signal-connect toplevel "delete_event" (lambda (args) (gtk-widget-hide toplevel)))
    (set! toggle-initialized #t)
    (if show (gtk-widget-show toplevel))))
	 

;; change the existing window
	 
(define*-public (change-ui-constraints-buttons #&key (vertical toggle-vertical) (pixmap toggle-pixmap))
  (initialize-ui-constraints-buttons #:vertical vertical #:pixmap pixmap #:show #t))

;; open the buttons window

(define-public (start-ui-constraints-buttons)
  (if (not buttons-toplevel)
      (initialize-ui-constraints-buttons))
  (gtk-widget-show buttons-toplevel))


;; close the buttons window

(define-public (close-ui-constraints-buttons)
  (gtk-widget-hide buttons-toplevel))


;; (use-scwm-modules constraints ui-constraints gtk optargs ui-constraints-composition ui-constraints-gtk-toggle-menu)
;; (use-modules (gtk gtk) (gtk gdk))

;; (set! toggle-initialized #f)
;; (start-constraints)
;; (ui-constraints-gtk-toggle-menu)
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
