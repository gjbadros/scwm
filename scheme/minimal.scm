;; $Id$
;; This file gets compiled directly into scwm
;; Scwm will eval these commands before reading
;; any other .scm file
;; In a sense, these are compiled-in primitives implemented in scheme
;; (these can get overridden later, of course)

(define quit scwm-quit)
(undefine scwm-quit)

;;; Make some colors
(define FIXED-FONT (make-font "fixed"))
(define BLACK (make-color "black"))
(define GRAY (make-color "gray"))
(define SLATEGRAY (make-color "slategray"))

;;; Set some global options
(set-rubber-band-mask! 30)
(set-menu-foreground! BLACK)
(set-menu-background! GRAY)
(set-menu-stipple! SLATEGRAY)

(set-menu-font! FIXED-FONT)
(set-menu-mwm-style! #f)

(set-hilight-foreground! BLACK)
(set-hilight-background! GRAY)
(set-icon-font! FIXED-FONT)
(set-title-font! FIXED-FONT)
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

(bind-mouse 'root 1 (lambda () (popup-menu default-menu)))




