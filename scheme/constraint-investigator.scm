;;;; $Id$
;;;; Copyright (C) 1999 Greg J. Badros and Jeff W. Nichols
;;;; Based on ui-constraints-buttons.scm, Copyright (C) 1999 Greg J. Badros
;;;; Some elements copied from ui-constraints-toggle-menu.scm,
;;;;                                 also Copyright (C) 1999 Greg J. Badros
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

;; constraint-investigator.scm
;;
;; (use-scwm-modules ui-constraints-gtk-toggle-menu)
;; (ui-constraints-gtk-toggle-menu)


(define-module (app scwm constraint-investigator)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app scwm base)
  :use-module (app scwm flash-window)
  :use-module (app scwm optargs)
  :use-module (app scwm ui-constraints))


;; PRIVATE variables held for the module

(define gtk-toggle-window (gtk-window-new 'toplevel))
(define gtk-instance-box (gtk-vbox-new #t 1))

(define gtk-toggle-close? #f)

(define is-toggle-initialized? #f)

;; GJB:FIXME:: hack to fix gtk signal recursion (?)
(define allow-enable-hooks? #t)


;; window flashing code from ui-constraints-toggle-menu

(define (flash-windows-of-constraint win-list)
  (let ((color "red"))
    (for-each 
     (lambda (w) (flash-window w #:color color #:unflash-delay #f)) 
     win-list)))


(define (unflash-windows-of-constraint win-list)
  (for-each 
   (lambda (w) (unflash-window w))
   win-list))


;; new routine to make a special widget

(define (make-cn-widget n)
  (let* ((toplevel gtk-toggle-window)
	 (box gtk-instance-box)
	 (close? gtk-toggle-close?)
	 (class (ui-constraint-class n))
	 (mproc (ui-constraint-class-menuname-proc class))
	 (name (mproc n))
	 (win-list (ui-constraint-windows n))
	 (enabled? (ui-constraint-enabled? n))
	 (tbl (gtk-table-new 1 2 #f))
	 (lbl (gtk-label-new name))
	 (aln1 (gtk-alignment-new 0 .5 0 0))
	 (aln2 (gtk-alignment-new 1 .5 0 0))
	 (hbox (gtk-hbox-new #f 3))
	 (constraint-drawn #f)
	 (ebox (gtk-event-box-new))
	 (bt1 (gtk-check-button-new))  ;; the enable/disable checkbox
	 (bt2 (gtk-button-new-with-label "Delete")))
    (gtk-toggle-button-set-state bt1 enabled?)
    (ui-constraint-add-enable-hook n (lambda (e) (if allow-enable-hooks? 
						     (begin
						       (set! allow-enable-hooks? #f)
						       (gtk-toggle-button-set-state bt1 e)
						       (set! allow-enable-hooks? #t)))))
    (gtk-signal-connect bt2 "clicked"
			(lambda () 
			  (delete-ui-constraint! n)
			  ;; (refresh)
			  (if close? (gtk-widget-hide toplevel))))
    (gtk-signal-connect bt1 "clicked"
			(lambda () 
			  (if allow-enable-hooks?
			      (begin
				(set! allow-enable-hooks? #f)
				((if (ui-constraint-enabled? n)
				     disable-ui-constraint 
				     enable-ui-constraint) n)
				;; (refresh)
				(set! allow-enable-hooks? #t)))
			  (if close? (gtk-widget-hide toplevel))))
    (gtk-signal-connect ebox "enter_notify_event"
			(lambda (event)
			  ;; we need to filter out enter/leave of the click-box button
			  ;; those have a notify-detail of 'inferior, so we only
			  ;; do the work if it's not an 'inferior crossing event
			  (if (not (eq? (gdk-event-notify-detail event) 'inferior))
			      (begin
				(flash-windows-of-constraint win-list)
				(if (ui-constraint-enabled? n)
				    (begin
				      (draw-constraint n)
				      (set! constraint-drawn #t)))))))
    (gtk-signal-connect ebox "leave_notify_event"
			(lambda (event)
			  ;; we need to filter out enter/leave of the click-box button
			  ;; those have a notify-detail of 'inferior, so we only
			  ;; do the work if it's not an 'inferior crossing event
			  (if (not (eq? (gdk-event-notify-detail event) 'inferior))
			      (begin
				(if constraint-drawn
				    (begin
				      (undraw-constraint n)
				      (set! constraint-drawn #f)))
				(unflash-windows-of-constraint win-list)))))
    (gtk-box-pack-start hbox bt1)
    (gtk-box-pack-start hbox lbl)
    (gtk-container-add ebox hbox)
    (gtk-container-add aln1 ebox)
    (gtk-container-add aln2 bt2)
    (gtk-table-attach tbl aln1 0 1 0 1)
    (gtk-table-attach tbl aln2 1 2 0 1)
    (gtk-container-add box tbl)
    (gtk-widget-show-all tbl)
    (ui-constraint-set-button! n tbl)
    tbl))


;; remove a button from the screen (when the constraint is deleted)

(define (remove-cn-button cn)
  (let ((but (ui-constraint-button cn)))
    (gtk-widget-hide but)
    (gtk-container-remove gtk-instance-box but)
    (ui-constraint-set-button! cn #f)))


;; sets up the toggle menu for use
;; creates

(define ui-box-spacing 4)
(define ui-box-border 5)

(define* (initialize-gtk-toggle-menu #&optional (close? #f))
  (let* ((toplevel gtk-toggle-window)
	 (vboxcn gtk-instance-box)
	 (vbox (gtk-vbox-new #f 3))
	 (cn-buttons (map (lambda (n) (make-cn-widget n)) 
			  global-constraint-instance-list))
	 (disable (gtk-button-new-with-label "Disable All"))
	 (enable (gtk-button-new-with-label "Enable All"))
	 (delete-all (gtk-button-new-with-label "Delete All"))
	 (hbuttonbox (gtk-hbutton-box-new))
	 (close (gtk-button-new-with-label "Close")))  ;; close button is not added below --12/08/99 gjb
    (gtk-window-set-title toplevel "Constraint investigator")
    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)
    (add-hook-once! constraint-add-hook
     (lambda (cn) (make-cn-widget cn)))
    (add-hook-once! constraint-delete-hook
     (lambda (cn) (remove-cn-button cn)))
    (set! gtk-toggle-close? close?)
    (gtk-container-add vbox vboxcn)
    (for-each (lambda (but) (gtk-container-add hbuttonbox but)) (list disable enable delete-all)) ;; close))
    (for-each 
     (lambda (but)
       (gtk-signal-connect but "enter"
			   (lambda () (draw-all-constraints)))
       (gtk-signal-connect but "leave"
			   (lambda () (undraw-all-constraints))))
     (list enable disable))
    (gtk-signal-connect disable "clicked" 
			(lambda () (disable-all-constraints) 
				(if close? (gtk-widget-hide toplevel))))
    (gtk-signal-connect enable "clicked" 
			(lambda () (enable-all-constraints) 
				(if close? (gtk-widget-hide toplevel))))
    (gtk-signal-connect delete-all "clicked" 
			(lambda () (delete-all-constraints) 
				(if close? (gtk-widget-hide toplevel))))
    (gtk-container-add vbox hbuttonbox)
    (gtk-signal-connect close "clicked"	(lambda () (gtk-widget-hide toplevel)))
    (gtk-container-add toplevel vbox)
    (gtk-widget-show-all vbox)
    (gtk-window-position toplevel 'mouse)
    (gtk-window-set-policy toplevel #t #t #t)
    (gtk-signal-connect toplevel "delete_event" (lambda (args) (gtk-widget-hide toplevel)))
    (set! is-toggle-initialized? #t)))


(define-public (start-constraint-investigator)
  "Start the GTk+-based constraint investigator window."
  (if (not is-toggle-initialized?)
      (initialize-gtk-toggle-menu))
  (gtk-widget-show gtk-toggle-window))
