;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros


(define-module (app scwm cursor)
  :use-module (app scwm optargs)
  :use-module (app scwm nonants)
  :use-module (app scwm defoption)
  )


(define*-public (image-name->cursor name #&optional (x-hotspot 8) (y-hotspot 8))
  "Make a cursor given only the NAME.
X-HOTSPOT and Y-HOTSPOT can specify the hotspot coordinates for the cursor."
  (create-pixmap-cursor (make-image name) #f #f x-hotspot y-hotspot))

;;; these pixmaps are in pixmaps/
(define resize-br (image-name->cursor "resize_br.xpm"))
(define resize-tr (image-name->cursor "resize_tr.xpm"))
(define resize-h (image-name->cursor "resize_h.xpm"))
(define resize-v (image-name->cursor "resize_v.xpm"))

;; (apply-fancy-resize-cursors)
(define*-public (apply-fancy-resize-cursors #&optional (win (get-window)))
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
