;; $Id$
;; -*- scwm -*-

;;;; SCWM-MODE BASICS
;; first show scwmexec

(+ 1 2)
(select-window-interactively)

(move-window-relative 25 25 (select-window-interactively))
(move-window-relative -25 -25 (select-window-interactively))

(animated-window-shade)
(bind-mouse 'title 3 #f popup-window-ops)

(list-windows)

(map iconified-window? (list-windows))

(list-windows #:only iconified-window?)
(list-windows #:only (lambda (w) (string=? (window-class w) "XTerm")))
(list-windows #:only (wildcard-matcher "XTerm"))
;; de-iconify via window list


;; arbitrary predicate!
(list-windows #:only 
	      (lambda (w) (equal? (window-viewport-position w) '(0 0))))


;; Show scwm-options-dialog

;; here's all the code it takes to
;; make a new GUI-configurable option
(define-scwm-option *menu-font* (make-font "*helvetica*medium-r*12*")
  "The default menu font."
  #:type 'font
  #:group 'menu
  #:setter (lambda (font) (set! menu-font font))
  #:getter (lambda () menu-font))

(clever-place-window w)

(define (fully-obscured-handler win from-vport-move?)
  (if (not from-vport-move?)
      (clever-place-window win)))

(add-hook! window-fully-obscured-hook fully-obscured-handler)
(remove-hook! window-fully-obscured-hook fully-obscured-handler)

;;;; WINDOW GROUP SELECTIONS

;; H-S-mouse1 to select, H-S-mouse2 to move

;; Uses window-enter-hook, window-leave-hook



;;;; CONSTRAINT SOLVER

;; turn on via root menu, create inequality & equality cns

;; C-M-A-c to list constraints, and turn off one by one



;;;; NETSCAPE INTERACTION
;; (be sure netscape is running)

;; C-M-S-t
;; http://scwm.mit.edu/scwm

;; All via X properties-- no C code specifically written to do the above!


;;;; SYNTHETIC EVENT SUPPORT
;; keybindings for pointer movement
;; keybindings for button presses  (click on Home of netscape, then iconify)

;; Enable send-events in an xterm (or demo in GNU Emacs)
;; C-M-S-4

;; Only need below if did not define w earlier
;; (define w (select-window-interactively))

(add-timer-hook! (sec->msec 1.5) 
		 (lambda () (X-synthetic-send-string "ls -ld g*" w)))


;;;; WINDOW REARRANGEMENTS AND UNDO
;; H-s is insert-undo-global

;; (reset-undo!)

;; Select a bunch of windows, then do group->tile
;; then H-/ to undo, H-M-/ to redo

;;;; REGISTERS


;;;; LEARNING SCWM
;; file:///home/gjb/scwm/doc/html/book1.htm

;; tab-completion

;; list-windows C-h C-s 

;; window-enter-hook

;; *theme-path*



