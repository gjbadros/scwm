;;; menu-item test cases

(define sticky-menu-item
  (make-menu-item "Sticky - this is a long one" toggle-stick "C-S-F8" #f
		  (make-image "mini-stick.xpm") #f "sticky"))

;; Use a pixmap separator is just temporary -- it has lots of problems
(define move-menu-item
  (make-menu-item "Move" interactive-move "C-S-F7" (make-image "separator.xbm")
		  (make-image "mini-move.xpm") #f "move"))

(define resize-menu-item
  (make-menu-item "Resize" interactive-resize "C-S-F8" #f
		  (make-image "mini-resize.xpm") #f "resize"))

(define a-menu
  (make-scwm-menu (list sticky-menu-item move-menu-item resize-menu-item)
		  (make-image "linux-menu.xpm") (load-color "blue")
		  (load-color "gray80")))


(popup-menu a-menu)
