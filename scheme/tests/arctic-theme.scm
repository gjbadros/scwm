;; $Id$


(define theme-dir "/scratch/gjb/themes/")  ;; needs trailing slash

(define* (list-of-theme-images prefix #&optional 
			       (list-of-image-names '("topleft"
						      "top"
						      "topright"
						      "left"
						      "right"
						      "bottomleft"
						      "bottom"
						      "bottomright")))
  (map (lambda (s) (make-image (string-append prefix s ".xpm"))) list-of-image-names))

(define menu-decor-arctic
  (list-of-theme-images (string-append theme-dir "arctic/arctic-")))

(define menu-decor-blue-metal 
  (list-of-theme-images "BlueMetal/"))

(set-menu-look! xpm-shaped-menu-look)

(define menu-root
  (menu
   (list
    (menuitem "Root Menu" #f) menu-title
    menu-separator
    (menuitem "&Switch to..." #:action
	      (lambda () 
		(show-window-list-menu #:show-geometry #t)))
    (menuitem "Re&fresh Screen"
;	      #:image-left "mini-ray.xpm" 
	      #:action refresh)
    menu-separator
    (menuitem "&Restart scwm" #:action (lambda () (restart "scwm")))
    (menuitem "&Exit scwm" #:action 'menu-quit-verify))
   #:image-side "linux-menu.xpm"
   #:image-align 'bottom
   #:color-bg-image-side "blue"
   #:image-bg (make-image "wh_marble_3.xpm")
   #:color-stipple "navy"
;  #:color-bg (make-color "#242b4a")
;  #:color-text "yellow"
   #:extra menu-decor-arctic))

(popup-menu menu-root)
