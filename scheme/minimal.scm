;; $Id$
;; This file gets compiled directly into scwm
;; Scwm will eval these commands before reading
;; any other .scm file
;; In a sense, these are compiled-in primitives implemented in scheme
;; (these can get overridden later, of course)

(define quit scwm-quit)
(undefine scwm-quit)

;;; Make some colors
(define FIXED-FONT (load-font "fixed"))
(define BLACK (load-color "black"))
(define GRAY (load-color "gray"))
(define SLATEGRAY (load-color "slategray"))
(define LIGHTGRAY (load-color "lightgray"))
(define DIMGRAY (load-color "dimgray"))

;;; Set some global options
(set-rubber-band-mask! 1)
(set-menu-colors! BLACK GRAY SLATEGRAY)
(set-menu-font! FIXED-FONT)
(set-menu-mwm-style! #f)
(set-hilight-colors! BLACK GRAY)
(set-icon-font! FIXED-FONT)
(set-window-font! FIXED-FONT)
(set-title-justify! 'center)

;;; A Menu
(define default-menu (make-menu 
		      (list
		       (make-menuitem "Default Menu" #f)
		       (make-menuitem "Exit SCWM" quit))))

;;; Some functions for decoration bindings
(define (resize-or-raise)
  (raise-window)
  (case (mouse-event-type)
    ((motion) (interactive-resize))
    ((double-click) (lower-window))))
(define (move-or-raise)
  (raise-window)
  (case (mouse-event-type)
    ((motion) (interactive-move))
    ((double-click) (lower-window))))

;;; bindings
(bind-mouse 'frame 1 resize-or-raise)

(bind-mouse '(title sidebar) 1 move-or-raise)

(bind-event 'new-window (lambda () (set-window-colors!
                                    LIGHTGRAY DIMGRAY)
				(show-titlebar)))
(bind-event 'new-window-hint (lambda () (set-random-placement!
					 #t)
				     (set-smart-placement! #t)))
(bind-mouse 'root 1 (lambda () (popup-menu default-menu)))

