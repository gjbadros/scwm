;;;; $Id$ -*- scwm -*-

(define-module (app scwm desk-background)
  :use-module (app scwm background)
  :use-module (app scwm optargs))

;; JTL:FIXME:: completely arbitrary size
;; entries are vectors, #(reset? color image image-style)
(define desk-bg-styles (make-hash-table 13))
(define desk-bg-hooked #f)

(define (bgstyle-elements bgstyle default)
  (map (lambda (n)
	 (if (eq? (vector-ref bgstyle n) 'default)
	     (vector-ref default n)
	     (vector-ref bgstyle n)))
       '(0 1 2 3)))

;; JTL:FIXME:: there HAS to be a prettier way to do this
(define (desk-background-hook new old)
  (let ((bgstyle (hashq-ref desk-bg-styles new))
	(default (hashq-ref desk-bg-styles #t)))
    (if (not bgstyle)
	(set! bgstyle default))
    (apply 
     (lambda (reset? color image image-style)
       (if reset?
	   (reset-background!))
       (if color
	   (set-background-color! color))
       (if image
	   (set-background-image! image image-style)))
     (bgstyle-elements bgstyle default))))

;; JTL:FIXME:: documentation
(define*-public (desk-background desk #&key reset color image image-style)
  "Set the background to use on desk DESK.
RESET determines whether to reset the background to the standard X 
crosshatch before enacting any other settings.
COLOR is a background color to set, either a color name or a color object.
IMAGE is a background image, either an image name or an image object.
IMAGE-STYLE is either 'tiled or 'centered.
Any of these may be left unset to use the default settings.
The default may be set by setting the background for DESK #t."
  (hashq-set! desk-bg-styles desk 
	      (vector
		(if (bound? reset)
		    reset
		    'default)

		(if (bound? color)
		    (cond ((string? color)
			   (make-color color))
			  ((color? color)
			   color))
		    'default)

		(if (bound? image)
		    (cond ((string? image)
			   (let ((made-image (make-image image)))
			     (if made-image
				 made-image
				 (error "Invalid image specifier"))))
			  ((image? image)
			   image))
		    'default)

		(if (bound? image-style)
		    (cond ((or (eq? image-style 'tiled)
			       (eq? image-style 'centered))
			   image-style)
			  (error "Invalid image-style specifier"))
		    'default)))
  (cond ((not desk-bg-hooked)
	 (add-hook! change-desk-hook desk-background-hook)
	 (set! desk-bg-hooked #t))))

(hashq-set! desk-bg-styles #t #(#t #f #f 'centered))

;(reset-hook! change-desk-hook)
;(desk-background #t
;		 #:reset #t
;		 #:color "black"
;		 #:image "slate.xpm" 
;		 #:image-style 'tiled)
;(desk-background 0 #:color "yellow" #:image #f)
;(desk-background 1 #:image "blckrock.xpm" #:image-style 'tiled)
;(hashq-ref desk-bg-styles 0)
