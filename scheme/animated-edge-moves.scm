;;; $Id$
;;; Copyright (C) 1999, 2000 Greg J. Badros <gjb@cs.washington.edu>

(define-module (app scwm animated-edge-moves)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm animation)
  :use-module (app scwm defoption))


;; Northwest
(define*-public (animated-move-to-nw #&optional (win (get-window)))
  "Move WIN to the northwest edge and update its gravity."
  (interactive)  
  (with-window win
	       (animated-move-to 0 0) 
	       (set-window-gravity! 'northwest)))

;; Northeast
(define*-public (animated-move-to-ne #&optional (win (get-window)))
  "Move WIN to the northeast edge and update its gravity."
  (interactive)  
  (with-window win
	       (animated-move-to (x- (w%x 100)) 0)
	       (set-window-gravity! 'northeast)))

;; Southwest
(define*-public (animated-move-to-sw #&optional (win (get-window)))
  "Move WIN to the southwest edge and update its gravity."
  (interactive)  
  (with-window win
	       (animated-move-to 0 (y- (w%y 100)))
	       (set-window-gravity! 'southwest)))

;; SouthEast
(define*-public (animated-move-to-se #&optional (win (get-window)))
  "Move WIN to the southeast edge and update its gravity."
  (interactive)  
  (with-window win
	       (animated-move-to (x- (w%x 100)) 
				 (y- (w%y 100)))
	       (set-window-gravity! 'southeast)))

;; North
(define*-public (animated-move-to-n #&optional (win (get-window)))
  "Move WIN to the north edge and update its gravity."
  (interactive)  
  (with-window win
	       (animated-move-to #f 0)
	       (set-window-gravity! 'north)))

;; East
(define*-public (animated-move-to-e #&optional (win (get-window)))
  "Move WIN to the east edge and update its gravity."
  (interactive)  
  (with-window win
	       (animated-move-to (x- (w%x 100)) #f)
	       (set-window-gravity! 'east)))

;; South
(define*-public (animated-move-to-s #&optional (win (get-window)))
  "Move WIN to the south edge and update its gravity."
  (interactive)
  (with-window win
	       (animated-move-to #f (y- (w%y 100)))
	       (set-window-gravity! 'south)))

;; West
(define*-public (animated-move-to-w #&optional (win (get-window)))
  "Move WIN to the west edge and update its gravity."
  (interactive)
  (with-window win
	       (animated-move-to 0 #f)
	       (set-window-gravity! 'west)))

;; Center
(define*-public (animated-move-to-center #&optional (win (get-window)))
  "Move WIN to the center of the viewport and update its gravity."
  (interactive)
  (with-window win
	       (animated-move-to (- (/ display-width 2) (w%x 50))
				 (- (/ display-height 2) (w%y 50)))
	       (set-window-gravity! 'center)))
