;; $Id$ -*-scwm-*-

(use-modules (app scwm xpm-menus))

(define theme-dir "/home/jtl/.pixmaps/kde_themes/")  ;; needs trailing slash
;;(define theme-dir "/scratch/gjb/themes/")  ;; needs trailing slash
;;(define theme-dir "/home/gjb/scwm-themes/")

(define* (list-of-theme-images prefix #:optional 
			       (list-of-image-names '("topleft"
						      "top"
						      "topright"
						      "left"
						      "right"
						      "bottomleft"
						      "bottom"
						      "bottomright")))
  (map (lambda (s) (make-image (string-append prefix s ".xpm"))) list-of-image-names))

(define (menu-decor-arctic)
  (list-of-theme-images (string-append theme-dir "Arctic/arctic-")))

(define (menu-decor-blue-metal )
  (list-of-theme-images "BlueMetal/"))

(define (xpm-menu-decor-tekno)
  (list-of-theme-images "Tekno/kwm/"))

(define (xpm-menu-decor-openlook)
  (list-of-theme-images "OpenLook/"
			'("kv" "v" "dv"
			  "k"      "d"
			  "ka" "a" "da")))
;; (use-modules (app scwm xpm-menus))
(set-default-menu-look! xpm-shaped-menu-look)

(define menu-test
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
   #:extra (menu-decor-arctic)))

(menu-hl-fg-color)
(menu-hl-g-color)
(set-menu-highlight-colors! menu-test (make-color "green") "grey50")
(set-menu-highlight-colors! menu-test #f #f)
(menu-highlight-colors menu-test)

(popup-menu menu-test)

(set-menu-look! scwm-menu-look)
(popup-menu menu-root)

(define tekno-menu-look
  (copy-menu-look xpm-shaped-menu-look "tekno" (xpm-menu-decor-tekno)))

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
   #:menu-look tekno-menu-look))

(popup-menu menu-root)

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
   #:color-stipple "navy"))


(popup-menu menu-root)

(define openlook-menu-look
  (copy-menu-look xpm-shaped-menu-look "tekno" (xpm-menu-decor-tekno)))

(set-menu-look! openlook-menu-look)

(popup-menu menu-root)
