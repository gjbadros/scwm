;;;; $Id$ -*- scwm -*-

(define-module (app scwm desk-background)
  :use-module (app scwm background)
  :use-module (app scwm optargs))

;; JTL:FIXME:: completely arbitrary size
;; entries are lists, (reset? color image image-style)
(define desk-bg-styles (make-hash-table 13))
(define desk-bg-hooked #f)

(define (desk-background-hook new old)
  (let ((bgstyle (or (hashq-ref desk-bg-styles new) 
		     (hashq-ref desk-bg-styles #t))))
    (apply 
     (lambda (reset? color image image-style)
       (if reset?
	   (reset-background!))
       (if color
	   (set-background-color! color))
       (if image
	   (set-background-image! image image-style)))
     bgstyle)))

(define*-public (desk-background desk #&key
				 (reset #f) 
				 (color #f) 
				 (image #f)
				 (image-style 'centered))
  "Set the background to use on desk DESK.
RESET determines whether to reset the background to the standard X 
crosshatch before enacting any other settings.
COLOR is a background color to set, either a color name or a color object.
IMAGE is a background image, either an image name or an image object.
IMAGE-STYLE is either 'tiled or 'centered.
The default may be set by setting the background for DESK #t."
  (hashq-set! desk-bg-styles desk 
	      (list reset
		    (cond ((color? color)
			   color)
			  ((string? color)
			   (make-color color))
			  ((not color)
			   #f)
			  (else
			   (error "Invalid color specifier")))

		    (cond ((image? image)
			   image)
			  ((string? image)
			   (let ((made-image (make-image image)))
			     (if made-image
				 made-image
				 (error "Invalid image specifier"))))
			  ((not image)
			   #f)
			  (else
			   (error "Invalid image specifier")))

		    (cond ((or (eq? image-style 'tiled)
			       (eq? image-style 'centered))
			   image-style)
			  (else
			   (error "Invalid image-style specifier")))))
  (if (eq? desk (current-desk))
      (desk-background-hook desk desk))
  (cond ((not desk-bg-hooked)
	 (add-hook! change-desk-hook desk-background-hook)
	 (set! desk-bg-hooked #t))))

(hashq-set! desk-bg-styles #t '(#t #f #f 'centered))

;(reset-hook! change-desk-hook)
;(desk-background #t
;		 #:reset #t
;		 #:color "black"
;		 #:image "slate.xpm" 
;		 #:image-style 'tiled)
;(desk-background 0 #:color "yellow" #:image #f)
;(desk-background 1 #:image "blckrock.xpm" #:image-style 'tiled)
;(hashq-ref desk-bg-styles 0)
