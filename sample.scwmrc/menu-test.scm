;;; menu-item test cases

(define sticky-menu-item
  (make-menu-item "Sticky - this is a long one" toggle-stick "C-S-F8" #f
		  (make-picture "mini-stick.xpm") #f "sticky"))

;; Use a pixmap separator is just temporary -- it has lots of problems
(define move-menu-item
  (make-menu-item "Move" interactive-move "C-S-F7" (make-image "separator.xbm")
		  (make-picture "mini-move.xpm") #f "move"))

(define resize-menu-item
  (make-menu-item "Resize" interactive-resize "C-S-F8" #f
		  (make-picture "mini-resize.xpm") #f "resize"))

(define a-menu
  (make-scwm-menu (list sticky-menu-item move-menu-item resize-menu-item)
		  (make-picture "linux-menu.xpm") (load-color "blue")
		  (load-color "gray80")))

(popup-menu a-menu)

;; Test binding and unbind mouse button 1
(bind-mouse 'root 1 (lambda () (display "foo\n")))

(unbind-mouse 'root 1)

;;(window-id)
;;(raise-window (window-from-window-id 16777234))

(define pic-unknown (make-image "unknown1.xpm"))
(image-properties pic-unknown)
