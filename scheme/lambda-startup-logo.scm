;;; $Id$
;;; lambda-startup-logo.scm
;;; (C) 1999 Greg J. Badros
;;; 

(define-module (app scwm lambda-startup-logo)
  :use-module (app scwm optargs)
  :use-module (app scwm message-window))

(define scwm-logo (make-image "scwm-logo-0.xpm"))

(define scwm-logo-msgwin #f)

(define logo-image-sequence #f)

(define next-logo-image
  (let ((i 0))
    (lambda ()
      (set! i (modulo (+ i 1) (vector-length logo-image-sequence)))
      (message-window-set-image! scwm-logo-msgwin 
				 (vector-ref logo-image-sequence i) #f #f #t))))

(define (next-logo-image-n n) (next-logo-image))

(define*-public (logo-setup #&optional (should-rotate #f))
  (if (not scwm-logo)
      (error "Scwm logo image not found!"))
  (set! scwm-logo-msgwin 
	(make-message-window-with-image scwm-logo #t))
  (set-X-server-synchronize! #t)
  (message-window-show! scwm-logo-msgwin)
  (handle-pending-events)
  (set-X-server-synchronize! #f)
  
  (if should-rotate
      (begin
	(set! logo-image-sequence
	      (list->vector
	       (map (lambda (deg) (make-image
				   (string-append "scwm-logo-"
						  (number->string deg) ".xpm")))
		    (iota 24))))
	
	(add-hook! load-processing-hook next-logo-image-n)))
  (append-hook! startup-hook logo-remove)
  )

(define-public (logo-remove)
  (if scwm-logo
      (begin
	(message-window-hide! scwm-logo-msgwin)
	(set! next-logo-image (lambda () #f))
	(set! logo-image-sequence #f)
	
	(remove-hook! load-processing-hook next-logo-image-n))))