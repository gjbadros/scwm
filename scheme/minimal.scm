;; $Id$
;; (C) 1997-1998 Maciej Stachowiak and Greg J. Badros
;; This file gets compiled directly into scwm
;; Scwm will eval these commands before reading
;; any other .scm file
;; In a sense, these are compiled-in primitives implemented in scheme
;; (these can get overridden later, of course)

;; Make quit an alias for scwm-quit
(define quit scwm-quit)
(undefine scwm-quit)

(define FIXED-FONT (make-font "fixed"))

;;; Make some colors
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

;;; The default menu
(define default-menu (make-menu 
		      (list
		       (make-menuitem "Default Menu" #f)
		       (make-menuitem "Exit SCWM" quit))))

;;; Some functions for decoration bindings
(define (resize-or-raise)
  "Perform a resize, raise, or lower based on the mouse-event-type.
To be bound to a window decoration: click does `raise-window',
motion does `interactive-resize', and double-click does
`lower-window'."
  (case (mouse-event-type)
    ((click) (raise-window))
    ((motion) (interactive-resize))
    ((double-click) (lower-window))))

(define (move-or-raise)
  "Perform a move, raise, or lower based on the mouse-event-type.
To be bound to a window decoration: click does `raise-window',
motion does `interactive-move', and double-click does
`lower-window'."
  (case (mouse-event-type)
    ((click) (raise-window))
    ((motion) (interactive-move))
    ((double-click) (lower-window))))

;;; Initialize the decoration bindings to
;;; permit at least some useful behaviour
(bind-mouse 'frame 1 resize-or-raise)

(bind-mouse '(title sidebar) 1 move-or-raise)

(bind-mouse 'root 1 (lambda () (popup-menu default-menu)))
