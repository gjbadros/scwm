;; $Id$
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



(define-module (app scwm kde-hints))



;; This is just filler -- use scheme/tests/test.scm for real testing
(defmacro-public test-case (TITLE FORM . RESULT)
  #f)

(define-public kwm-module-winids '())

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

(define (kwm-win-numeric-property-set! win prop value)
  (let ((property (string-append "KWM_" prop)))
    (X-property-set! win property (vector value) property 32)))

(define (kwm-numeric-property-set! prop value)
  (kwm-win-numeric-property-set! 'root-window prop value))

(define (kwm-win-numeric-property-get win prop)
  (let ((property (string-append "KWM_" prop)))
    (vector-ref (car (X-property-get win property)) 0)))

(define (kwm-numeric-property-get prop)
  (kwm-win-numeric-property-get 'root-window prop))

(define (kwm-win-string-property-set! win prop value)
  (let ((property (string-append "KWM_" prop)))
    (X-property-set! win property value "STRING" 8)))

(define (kwm-string-property-set! prop value)
  (kwm-win-string-property-set! 'root-window prop value))

(define (kwm-win-string-property-get win prop)
  (let ((property (string-append "KWM_" prop)))
    (car (X-property-get win property))))

(define (kwm-string-property-get prop)
  (kwm-win-string-property-get 'root-window prop))

(define (kwm-win-double-property-set! win prop value1 value2)
  (let ((property (string-append "KWM_" prop)))
    (X-property-set! win property (vector value1 value2) 
		     property 32)))

(define (kwm-double-property-set! prop value1 value2)
  (kwm-win-double-property-set! 'root-window prop value1 value2))

(define (kwm-win-double-property-get win prop)
  (let ((property (string-append "KWM_" prop)))
    (car (X-property-get win property))))

(define (kwm-double-property-get prop)
  (kwm-win-double-property-get 'root-window prop))

(define (kwm-win-vector-property-set! win prop vector-value)
  (let ((property (string-append "KWM_" prop)))
    (X-property-set! win property vector-value property 32)))

(define (kwm-vector-property-set! prop vector-value)
  (kwm-win-vector-property-set! 'root-window prop vector-value))

(define (kwm-win-vector-property-get win prop)
  (let ((property (string-append "KWM_" prop)))
    (car (X-property-get win property))))

(define (kwm-vector-property-get prop)
  (kwm-win-vector-property-get 'root-window prop))


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
       kwm-module-winids))

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

(define (kwm-client-message-handler win atom format data)
  (if (eq? atom KWM_MODULE)
      (let ((winid (vector-ref data 0)))
	(set! kwm-module-winids (cons winid kwm-module-winids))
	(kwm-send-window-list-to-winid winid))
      )
  (if (eq? atom KWM_ACTIVATE_WINDOW)
      (let* ((winid (vector-ref data 0))
	     (win (id->window winid)))
	(if (iconified-window? win) (deiconify-window win))
	(focus-window win))))
      

(define (client-message-debug-handler win a f d)
  (display "c-m-h\n")
  (display (X-atom->string a))
  (display ": ")
  (display f)
  (write d)
  (display "\n"))
;; (add-hook! client-message-hook client-message-debug-handler)

(define (kwm-root-property-notify-handler atom deleted?)
  (if (eq? atom KWM_CURRENT_DESKTOP)
      (let ((desknum (kwm-numeric-property-get "CURRENT_DESKTOP")))
	(set-current-desk! (- desknum 1)))))

(define (kwm-property-notify-handler name win)
  (cond 
   ((string=? name "KWM_WIN_ICONIFIED")
    (let ((f (kwm-win-numeric-property-get win "WIN_ICONIFIED")))
      ((if (= f 1) iconify-window deiconify-window) win)))))

(define (property-notify-debug-handler prop-name win)
  (display "Property changed: ")
  (display prop-name) (display "\n") (write win) (display "\n"))

(define (root-property-notify-debug-handler atom deleted?)
  (display "Property changed: ")
  (display (X-atom->string atom)) (display " ") 
  (display deleted?) (display "\n"))

(define (kwm-send-window-list-to-winid winid)
  (map (lambda (win) 
	 (send-client-message winid KWM_MODULE_WIN_ADD (window-id win)))
       (list-all-windows)))

;; public in case we need to resend
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

(define (kwm-iconify-window-handler win was-iconified?)
  (if (not was-iconified?)
      (begin
	(kwm-win-numeric-property-set! win "WIN_ICONIFIED" 1)
	(kwm-change-window-handler win))))

;; (X-property-set! (select-window-interactively) "KWM_WIN_ICONIFIED" 1 "KWM_WIN_ICONIFIED")

(define (kwm-deiconify-window-handler win was-iconified?)
  (if was-iconified?
      (begin
	(kwm-win-numeric-property-set! win "WIN_ICONIFIED" 0)
	(kwm-change-window-handler win))))

(define-public (kwm-emulation-initialize)
  (kwm-numeric-property-set! "NUMBER_OF_DESKTOPS" number-of-desktops)
  (kwm-numeric-property-set! "CURRENT_DESKTOP" (+ 1 (current-desk))) ;; KWM uses 1-based desks
  (kwm-set-desktop-names)

  ;; (reset-hook! X-UnmapNotify-hook)
  (add-hook! X-UnmapNotify-hook kwm-remove-window-handler)

  ;; (reset-hook! window-close-hook-hook)
  (add-hook! window-close-hook kwm-remove-window-handler)

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

  ;; (reset-hook! X-PropertyNotify-hook)
  (add-hook! X-PropertyNotify-hook kwm-property-notify-handler)

  (kwm-numeric-property-set! "RUNNING" 1)

  (kwm-send-window-list))

(define-public (kwm-emulation-reset)
  (remove-hook! X-UnmapNotify-hook kwm-remove-window-handler)

  (remove-hook! window-close-hook kwm-remove-window-handler)

  (remove-hook! after-new-window-hook kwm-after-new-window-handler)

  (remove-hook! iconify-hook kwm-iconify-window-handler)

  (remove-hook! deiconify-hook kwm-deiconify-window-handler)

  (remove-hook! change-desk-hook kwm-change-desk-handler)

  ;; (remove-hook! client-message-hook client-message-debug-handler)
  (remove-hook! client-message-hook kwm-client-message-handler)

  ;;(remove-hook! X-root-PropertyNotify-hook root-property-notify-debug-handler)
  (remove-hook! X-root-PropertyNotify-hook kwm-root-property-notify-handler)

  (remove-hook! X-PropertyNotify-hook kwm-property-notify-handler)

  )

(kwm-emulation-initialize)
;; (kwm-emulation-reset)

