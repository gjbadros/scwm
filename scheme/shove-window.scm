;;; $Id$
;;; shove-window.scm
;;; (C) 1999 Greg J. Badros

(define-module (app scwm shove-window)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm window-configuration)
  :use-module (app scwm animated-edge-moves)
  :use-module (app scwm message-window))

(define nonant-keys-mwin (make-message-window-with-image (make-image "nonant-keys.xpm")))

(define*-public (shove-window-prefix)
  "Display prompt to shove the current window to an edge or center."
  (interactive)
  (with-message-window-shown 
   nonant-keys-mwin
   (let* ((next-key-event (get-key-event))
	  (key (string->symbol (car next-key-event)))
	  (proc
	   (case key
	     ((j) animated-move-to-w)
	     ((l) animated-move-to-e)
	     ((i) animated-move-to-n)
	     ((comma) animated-move-to-s)
	     ((period) animated-move-to-se)
	     ((o) animated-move-to-ne)
	     ((u) animated-move-to-nw)
	     ((m) animated-move-to-sw)
	     ((k) animated-move-to-center)
	     (else #f)
	     )))
     (if proc 
	 (begin
	   (push-window-configuration)
	   (proc))))))
     

(define-public menu-window-shove
  (menu
   (list
    (menu-title "Move window") menu-separator
    (menuitem "Center" #:image-left "win-pos-center.xpm" 
	      #:action animated-move-to-center)
    (menuitem "North" #:image-left "win-pos-n.xpm"
	      #:action animated-move-to-n)
    (menuitem "East" #:image-left "win-pos-e.xpm"
	      #:action animated-move-to-e)
    (menuitem "South" #:image-left "win-pos-s.xpm"
	      #:action animated-move-to-s)
    (menuitem "West" #:image-left "win-pos-w.xpm" 
	      #:action animated-move-to-w)
    (menuitem "Northeast" #:image-left "win-pos-ne.xpm" 
	      #:action animated-move-to-ne)
    (menuitem "Southeast" #:image-left "win-pos-se.xpm" 
	      #:action animated-move-to-se)
    (menuitem "Southwest" #:image-left "win-pos-sw.xpm"
	      #:action animated-move-to-sw)
    (menuitem "Northwest" #:image-left "win-pos-nw.xpm"
	      #:action animated-move-to-nw)
    )))
