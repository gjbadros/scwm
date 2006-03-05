;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros


(define-module (app scwm cursor)
  :use-module (app scwm optargs)
  :use-module (app scwm nonants)
  :use-module (app scwm defoption)
  )


(define*-public (image-name->cursor name #:optional (x-hotspot 8) (y-hotspot 8))
  "Make a cursor given only the NAME.
X-HOTSPOT and Y-HOTSPOT can specify the hotspot coordinates for the cursor."
  (create-pixmap-cursor (make-image name) #f #f x-hotspot y-hotspot))

;;; these pixmaps are in pixmaps/
;;; double-headed arrow from top left to bottom right
(define resize-br (image-name->cursor "resize_br.xpm"))
;;; double-headed arrow from bottom left to top right
(define resize-tr (image-name->cursor "resize_tr.xpm"))
;;; double-headed arrow from left to right
(define resize-h (image-name->cursor "resize_h.xpm"))
;;; double-headed arrow from top to bottom
(define resize-v (image-name->cursor "resize_v.xpm"))

;;; SRL:FIXME:: Should allow different cursors for each nonant.
;; (apply-fancy-resize-cursors)
(define*-public (apply-fancy-resize-cursors #:optional (win (get-window)))
  "Use the fancy resize cursors for WIN."
  (map (lambda (pair) (set-window-cursor! (nonant-decoration win (car pair)) (cdr pair)))
       `((northwest . ,resize-br)
	 (southeast . ,resize-br)
	 (southwest . ,resize-tr)
	 (northeast . ,resize-tr)
	 (north . ,resize-v)
	 (south . ,resize-v)
	 (east . ,resize-h)
	 (west . ,resize-h))))

;;; GJB:FIXME:: It'd be nice if this were a window-style
(add-hook! startup-hook
	   (lambda () 
	     (add-hook! after-new-window-hook apply-fancy-resize-cursors)
	     (for-each apply-fancy-resize-cursors (list-all-windows))))
