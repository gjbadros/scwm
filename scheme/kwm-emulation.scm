;; $Id$
;;;; Copyright (C) 1999 Greg J. Badros
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



(define-module (app scwm kwm-emulation))



;; This is just filler -- use scheme/tests/test.scm for real testing
(defmacro-public test-case (TITLE FORM . RESULT)
  #f)

(define kwm-module-windows ())
;; (define kwm-module-windows (list kwm-kpanel-winid))
(define kwm-kpanel-winid #f)

(define WM_PROTOCOLS (string->X-atom "WM_PROTOCOLS"))
(define WM_DELETE_WINDOW (string->X-atom "WM_DELETE_WINDOW"))
(define WM_TAKE_FOCUS (string->X-atom "WM_TAKE_FOCUS"))
(define KWM_CURRENT_DESKTOP (string->X-atom "KWM_CURRENT_DESKTOP"))
(define KWM_ACTIVATE_WINDOW (string->X-atom "KWM_ACTIVATE_WINDOW"))
(define KWM_MODULE (string->X-atom "KWM_MODULE"))
(define KWM_MODULE_INIT (string->X-atom "KWM_MODULE_INIT"))
(define KWM_MODULE_DESKTOP_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_CHANGE"))
(define KWM_MODULE_DESKTOP_NAME_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_NAME_CHANGE"))
(define KWM_MODULE_DESKTOP_NUMBER_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_NUMBER_CHANGE"))
(define KWM_MODULE_WIN_ADD (string->X-atom "KWM_MODULE_WIN_ADD"))
(define KWM_MODULE_WIN_REMOVE (string->X-atom "KWM_MODULE_WIN_REMOVE"))
(define KWM_MODULE_WIN_CHANGE (string->X-atom "KWM_MODULE_WIN_CHANGE"))
(define KWM_WIN_ICONIFIED (string->X-atom "KWM_WIN_ICONIFIED"))
(define KWM_WIN_MAXIMIZED (string->X-atom "KWM_WIN_MAXIMIZED"))


(define desktop-names (list "Fun1" "Foo2" "Bar3" "Baz4"))
(define number-of-desktops (length desktop-names))

(define (kwm-numeric-property-set! prop value)
  (let ((property (string-append "KWM_" prop)))
    (X-property-set! 'root-window property (vector value) property 32)))

(define (kwm-numeric-property-get prop)
  (let ((property (string-append "KWM_" prop)))
    (vector-ref (car (X-property-get 'root-window property)) 0)))

(define (kwm-string-property-set! prop value)
  (let ((property (string-append "KWM_" prop)))
    (X-property-set! 'root-window property value "STRING" 8)))

(define (kwm-string-property-get prop)
  (let ((property (string-append "KWM_" prop)))
    (car (X-property-get 'root-window property))))

(define (kwm-double-property-set! prop value1 value2)
  (let ((property (string-append "KWM_" prop)))
    (X-property-set! 'root-window property (vector value1 value2) 
		     property 32)))

(define (kwm-double-property-get prop)
  (let ((property (string-append "KWM_" prop)))
    (car (X-property-get 'root-window property))))

(define (kwm-vector-property-set! prop vector-value)
  (let ((property (string-append "KWM_" prop)))
    (X-property-set! 'root-window property vector-value property 32)))

(define (kwm-vector-property-get prop)
  (let ((property (string-append "KWM_" prop)))
    (car (X-property-get 'root-window property))))


(test-case "kwm-double-property get/set"
	   (begin
	     (kwm-double-property-set! "TEST_DOUBLE" 5 8)
	     (kwm-double-property-get "TEST_DOUBLE"))
	   => #(5 8))

(test-case "kwm-vector-property get/set"
	   (begin
	     (kwm-vector-property-set! "QRECT" #(45 30 90 60))
	     (kwm-vector-property-get "QRECT"))
	   => #(45 30 90 60))

(define (send-kwm-modules-client-message atom data)
  (map (lambda (winid) (send-client-message winid atom data))
       kwm-module-windows))

(define (kwm-change-desk-handler new old)
  (let ((kwm-desk-number (+ 1 new)))
    (kwm-numeric-property-set! "CURRENT_DESKTOP" kwm-desk-number)
    (send-kwm-modules-client-message 
     KWM_MODULE_DESKTOP_CHANGE kwm-desk-number)))

(define (kwm-set-desktop-names)
  "Sets the names of desktops to those in `desktop-names'."
  (let ((i 1))
    (map (lambda (nm) 
	   (begin
	     (kwm-string-property-set! 
	      (string-append "DESKTOP_NAME_" (number->string i)) nm)  
	     (set! i (+ 1 i))))
	 desktop-names)))

(define (kwm-client-message-handler atom format data)
  (if (eq? atom KWM_MODULE)
      (let ((winid (vector-ref data 0)))
	(set! kwm-kpanel-winid winid)
	(set! kwm-module-windows (cons winid kwm-module-windows)))
      )
  (if (eq? atom KWM_ACTIVATE_WINDOW)
      (let ((winid (vector-ref data 0)))
	(focus (id->window winid)))))
      

(define (client-message-debug-handler a f d)
  (display "c-m-h\n")
  (display (X-atom->string a))
  (display ": ")
  (display f)
  (write d)
  (display "\n"))

(define (kwm-root-property-notify-handler atom deleted?)
  (if (eq? atom KWM_CURRENT_DESKTOP)
      (let ((desknum (kwm-numeric-property-get "CURRENT_DESKTOP")))
	(set-current-desk! (- desknum 1)))))

(define (property-notify-debug-handler prop-name win)
  (display "Property changed: ")
  (display prop-name) (display "\n") (write win) (display "\n"))

(define (root-property-notify-debug-handler atom deleted?)
  (display "Property changed: ")
  (display (X-atom->string atom)) (display " ") 
  (display deleted?) (display "\n"))

(define-public (kwm-send-window-list)
  (map (lambda (win) 
	 (send-kwm-modules-client-message KWM_MODULE_WIN_ADD (window-id win)))
       (list-all-windows)))

(define (kwm-after-new-window-handler win)
  (send-kwm-modules-client-message KWM_MODULE_WIN_ADD (window-id win)))

(define (kwm-change-window-handler win)
  (send-kwm-modules-client-message KWM_MODULE_WIN_CHANGE (window-id win)))

(define (kwm-remove-window-handler win)
  (send-kwm-modules-client-message KWM_MODULE_WIN_REMOVE (window-id win)))

(define (kwm-iconify-window-handler win)
  (X-property-set! win "KWM_WIN_ICONIFIED" 1 "KWM_WIN_ICONIFIED")
  (kwm-change-window-handler win))

(define (kwm-deiconify-window-handler win)
  (X-property-set! win "KWM_WIN_ICONIFIED" 1 "KWM_WIN_ICONIFIED")
  (kwm-change-window-handler win))

(define-public (kwm-emulation-initialize)
  (kwm-numeric-property-set! "NUMBER_OF_DESKTOPS" number-of-desktops)
  (kwm-numeric-property-set! "CURRENT_DESKTOP" (current-desk))
  (kwm-set-desktop-names)

  ;; (reset-hook! X-UnmapNotify-hook)
  (add-hook! X-UnmapNotify-hook kwm-remove-window-handler)

  ;; (reset-hook! X-DestroyNotify-hook)
  (add-hook! X-DestroyNotify-hook kwm-remove-window-handler)

  ;; (reset-hook! after-new-window-hook)
  (add-hook! after-new-window-hook kwm-after-new-window-handler)

  ;; (reset-hook! iconify-hook)
  (add-hook! iconify-hook kwm-iconify-window-handler)

  ;; (reset-hook! deiconify-hook)
  (add-hook! deiconify-hook kwm-deiconify-window-handler)

  ;; (reset-hook! change-desk-hook)
  (add-hook! change-desk-hook kwm-change-desk-handler)

  ;; (reset-hook! client-message-hook)
  ;; (add-hook! client-message-hook client-message-debug-handler)
  (add-hook! client-message-hook kwm-client-message-handler)

  ;; (reset-hook! X-root-PropertyNotify-hook)
  ;;(add-hook! X-root-PropertyNotify-hook root-property-notify-debug-handler)
  (add-hook! X-root-PropertyNotify-hook kwm-root-property-notify-handler)

  (kwm-numeric-property-set! "RUNNING" 1)

  (kwm-send-window-list))

(define-public (kwm-emulation-reset)
  (remove-hook! X-UnmapNotify-hook kwm-remove-window-handler)

  (remove-hook! X-DestroyNotify-hook kwm-remove-window-handler)

  (remove-hook! after-new-window-hook kwm-after-new-window-handler)

  (remove-hook! iconify-hook kwm-change-window-handler)

  (remove-hook! deiconify-hook kwm-change-window-handler)

  (remove-hook! change-desk-hook kwm-change-desk-handler)

  ;; (remove-hook! client-message-hook client-message-debug-handler)
  (remove-hook! client-message-hook kwm-client-message-handler)

  ;;(remove-hook! X-root-PropertyNotify-hook root-property-notify-debug-handler)
  (remove-hook! X-root-PropertyNotify-hook kwm-root-property-notify-handler))

(kwm-emulation-initialize)
