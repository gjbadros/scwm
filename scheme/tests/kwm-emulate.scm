;; $Id$

(define kwm-modules ())
(define kwm-kpanel-winid #f)

(define WM_PROTOCOLS (string->X-atom "WM_PROTOCOLS"))
(define WM_DELETE_WINDOW (string->X-atom "WM_DELETE_WINDOW"))
(define WM_TAKE_FOCUS (string->X-atom "WM_TAKE_FOCUS"))
(define KWM_CURRENT_DESKTOP (string->X-atom "KWM_CURRENT_DESKTOP"))
(define KWM_MODULE (string->X-atom "KWM_MODULE"))
(define KWM_MODULE_INIT (string->X-atom "KWM_MODULE_INIT"))
(define KWM_MODULE_DESKTOP_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_CHANGE"))
(define KWM_MODULE_DESKTOP_NAME_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_NAME_CHANGE"))
(define KWM_MODULE_DESKTOP_NUMBER_CHANGE (string->X-atom "KWM_MODULE_DESKTOP_NUMBER_CHANGE"))
(define KWM_WIN_ADD (string->X-atom "KWM_WIN_ADD"))


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


(kwm-double-property-set! "TEST_DOUBLE" 5 8)
(kwm-double-property-get "TEST_DOUBLE")

(kwm-vector-property-set! "QRECT" #(45 30 90 60))
(kwm-vector-property-get "QRECT")

(kwm-numeric-property-set! "RUNNING" 1)
;; (kwm-numeric-property-get "RUNNING")

(kwm-numeric-property-set! "NUMBER_OF_DESKTOPS" number-of-desktops)

(kwm-numeric-property-set! "CURRENT_DESKTOP" (current-desk))

(kwm-numeric-property-set! "CURRENT_DESKTOP" 2) 


(define (kwm-change-desk new old)
  (let ((kwm-desk-number (+ 1 new)))
    (kwm-numeric-property-set! "CURRENT_DESKTOP" kwm-desk-number)
    (send-client-message kwm-kpanel-winid
			 KWM_MODULE_DESKTOP_CHANGE kwm-desk-number)))

;; (reset-hook! change-desk-hook)
(add-hook! change-desk-hook kwm-change-desk)

(let ((i 1))
  (map (lambda (nm) 
	 (begin
	   (kwm-string-property-set! 
	    (string-append "DESKTOP_NAME_" (number->string i)) nm)  
	   (set! i (+ 1 i))))
       desktop-names))

;; (kwm-string-property-get "DESKTOP_NAME_1")

(add-hook! X-PropertyNotify-hook (lambda (prop win)
				   (if (eq? win 'root-window)
				       (begin
					 (display prop)
					 (display " changed\n")))))

(define (kwm-client-message-handler atom format data)
  (if (eq? atom KWM_MODULE)
      (let ((win (vector-ref data 0)))
	(set! kwm-kpanel-winid win))
      ))

(define (client-message-debug-handler a f d)
  (display "c-m-h\n")
  (display (X-atom->string a))
  (display ": ")
  (display f)
  (write d)
  (display "\n"))

;; (reset-hook! client-message-hook)
(add-hook! client-message-hook client-message-debug-handler)
(add-hook! client-message-hook kwm-client-message-handler)

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

;; (reset-hook! X-root-PropertyNotify-hook)
(add-hook! X-root-PropertyNotify-hook kwm-root-property-notify-handler)
;;(add-hook! X-root-PropertyNotify-hook root-property-notify-debug-handler)
