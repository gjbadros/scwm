;; $Id$
;; for testing new menuitem fg, bg, and font properties

(set-X-server-synchronize! #t)

(begin
  (define m1 (menuitem "foo" #:action raise-window #:fg "green" ))
  (define m2 (menuitem "bar" #:action lower-window #:bg "blue" ))
  (define m3 (menuitem "baz" #:action iconify #:fg "red" #:bg "yellow"))
  (define m4 (menuitem "bong" #:action deiconify-to-current-viewport
		       #:fg "red" #:bg "yellow" #:font "*charter*18*")))

(set! menu-hl-text-color (make-color "yellow"))
(set! menu-hl-bg-color (make-color "black"))
(set-menuitem-font! m3 (make-font "fixed"))
(set-menuitem-font! m3 (make-font "*charter*"))
(set-menuitem-font! m3 (make-font "*charter*24*"))
(set-menuitem-font! m3 #f)
(set-menuitem-colors! m3 #f #f)

(set-menuitem-colors! m4 "yellow" "red")

(begin
  (define m (menu (list m1 m2 m3 m4)))
  (set-menu-highlight-colors! m "green" "black")
  (set-menu-highlight-relief! m #f)
  (popup-menu m))

(menuitem-colors m1)
(set-menuitem-colors! m1 (make-color "red") (make-color "white"))
(set-menuitem-colors! m2 (make-color "purple") (make-color "green"))

(set-menu-highlight-colors! menu-root-start "yellow" "black")
(set-menu-highlight-colors! menu-root-start "yellow" "pink")
(set-menu-highlight-relief! menu-root-start #f)

(menu-highlight-colors menu-root-start)
;; (set-window-property! (get-window) 'squashed-titlebar #t)

