;;; menu-item test cases

(define m (make-menu (list (make-menuitem "foo" #f)
			   (make-menuitem "bar" #f)
			   (make-menuitem "baz" #f)
			   (make-menuitem "bop" #f)
			   (make-menuitem "bin" #f)
			   (make-menuitem "bon" #f)
			   (make-menuitem "bong" #f)
			   (make-menuitem "bip" #f)
			   (make-menuitem "bom" #f)
			   (make-menuitem "bal" #f)
			   (make-menuitem "biz 123" #f #f #f #f #f #f "123"))
		     (make-image "linux-menu.xpm") 'top (make-color "blue")))
(popup-menu m)


;; Use a pixmap separator is just temporary -- it has some problems
(define bitmap-separator
  (make-menuitem "" #f #f (make-image "separator.xbm")))

;;(define bitmap-separator
;;  (make-menu-item "" #f #f (make-image "default.xbm")))
(define window-ops-title-item
  (make-menuitem "Window Operations" #f))

(define sticky-menu-item
  (make-menuitem "Sticky - this is a long one" stick "C-S-F8" #f
		  (make-image "mini-stick.xpm") #f #f "sticky"))

(define move-menu-item
  (make-menuitem "Move" interactive-move "C-S-F7" #f
		  (make-image "mini-move.xpm") #f #f "move"))

(define resize-menu-item
  (make-menuitem "Resize" interactive-resize "C-S-F8" #f
		  (make-image "mini-resize.xpm") #f #f
		  "resize"))

(define a-popup-menu 
  (make-menu 
   (list
    (make-menuitem "Iconify/Restore" toggle-iconify "C-S-Down" #f
		    (make-image "mini-iconify.xpm") #f #f
		    "iconify")
    (make-menuitem "Stick/Unstick" (lambda () 
					   (display "toggle-stick\n")) #f #f
		    (make-image "mini-stick.xpm") #f #f
		    "stick"))
   #f #f (make-color "gray80"))
   )

(define b-popup-menu 
  (make-menu 
   (list
    (make-menuitem "More more... " a-popup-menu "" #f #f
		   #f #f "moremore")
    (make-menuitem "Iconify/Restore" toggle-iconify "C-S-Down" #f
		    (make-image "mini-iconify.xpm") #f #f
		    "iconify")
    (make-menuitem "Stick/Unstick" (lambda () 
					   (display "toggle-stick\n")) #f #f
		    (make-image "mini-stick.xpm") #f #f
		    "stick"))
   #f #f #f (make-color "gray80"))
   )

(define more-menu-ops-item
  (make-menuitem "More options..." b-popup-menu "" #f
		  #f 
		  (lambda () (display "uhover\n"))
		  (lambda () (display "unhover\n"))
		  "moreoptions"))

(define a-color
  (make-color "gray40"))

(define a-menu
  (make-menu (list 
		   window-ops-title-item bitmap-separator
		   sticky-menu-item move-menu-item resize-menu-item
		   more-menu-ops-item)
		  (make-image "linux-menu.xpm") 'top (make-color "blue")
		  'a-color))


(define a-popup-menu 
  (make-menu 
   (list
    (make-menu-item "Iconify/Restore" toggle-iconify "C-S-Down" #f
		    (make-image "mini-iconify.xpm") #f #f
		    "iconify")
    (make-menu-item "Stick/Unstick" toggle-stick "" #f
		    (make-image "mini-stick.xpm") #f #f
		    "stick"))
   #f #f #f (make-color "gray80"))
   )

;;(popup-menu a-menu)
;;(popup-menu a-popup-menu)

;;(menuitem-properties (caar (menu-properties a-popup-menu)))
;;(menu-properties a-menu)

;; Test binding and unbind mouse button 1
(bind-mouse 'root 1 (lambda () (popup-menu a-menu)))

;;(unbind-mouse 'root 1)

;;(window-id)
;;(raise-window (id->window 16777234))

(define broadcast-hook (lambda (d1 d2 d3 d4 d5 d6 d7) (display "hook\n")))
(define pic-unknown (make-image "unknown1.xpm"))
(image-properties pic-unknown)
