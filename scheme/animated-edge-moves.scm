;;; $Id$
;;; (C) 1999 Greg J. Badros <gjb@cs.washington.edu>

(define-module (app scwm animated-edge-moves)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm animation)
  :use-module (app scwm defoption))


;; Northwest
(define*-public (animated-move-to-nw #&optional (win (get-window)))
  (with-window win
	       (animated-move-to 0 0) 
	       (set-window-gravity! 'northwest)))

;; Northeast
(define*-public (animated-move-to-ne #&optional (win (get-window)))
  (with-window win
	       (animated-move-to (x- (w%x 100)) 0)
	       (set-window-gravity! 'northeast)))

;; Southwest
(define*-public (animated-move-to-sw #&optional (win (get-window)))
  (with-window win
	       (animated-move-to 0 (y- (w%y 100)))
	       (set-window-gravity! 'southwest)))

;; SouthEast
(define*-public (animated-move-to-se #&optional (win (get-window)))
  (with-window win
	       (animated-move-to (x- (w%x 100)) 
				 (y- (w%y 100)))
	       (set-window-gravity! 'southeast)))

;; North
(define*-public (animated-move-to-n #&optional (win (get-window)))
  (with-window win
	       (animated-move-to #f 0)
	       (set-window-gravity! 'north)))

;; East
(define*-public (animated-move-to-e #&optional (win (get-window)))
  (with-window win
	       (animated-move-to (x- (w%x 100)) #f)
	       (set-window-gravity! 'east)))

;; South
(define*-public (animated-move-to-s #&optional (win (get-window)))
  (with-window win
	       (animated-move-to #f (y- (w%y 100)))
	       (set-window-gravity! 'south)))

;; West
(define*-public (animated-move-to-w #&optional (win (get-window)))
  (with-window (get-window)
	       (animated-move-to 0 #f)
	       (set-window-gravity! 'west)))

;; Center -- just set gravity
(define*-public (animated-move-to-center #&optional (win (get-window)))
  (with-window win
	       (set-window-gravity! 'center)))