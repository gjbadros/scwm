;; $Id$
;; for testing new menuitem fg, bg, and font properties

(begin
  (define m1 (menuitem "foo" #:action raise-window #:fg "green" ))
  (define m2 (menuitem "bar" #:action lower-window #:bg "blue" ))
  (define m3 (menuitem "baz" #:action iconify #:fg "red" #:bg "yellow"))
  (define m4 (menuitem "bong" #:action deiconify-to-current-viewport
		       #:fg "red" #:bg "yellow" #:font "charter")))

(set-menuitem-font! m3 (make-font "fixed"))
(set-menuitem-font! m3 (make-font "*charter*"))
(set-menuitem-font! m3 #f)
(set-menuitem-colors! m3 #f #f)
(popup-menu (menu (list m1 m2 m3 m4)))

(menuitem-colors m1)
(set-menuitem-colors! m1 (make-color "red") (make-color "white"))
(set-menuitem-colors! m2 (make-color "purple") (make-color "green"))
