;; $Id$
;; -*- scwm -*-

;;;; SCWM-MODE BASICS
;; first show scwmexec

(+ 1 2)
(select-window-interactively)

(move-window-relative 25 25 (select-window-interactively))

;; like with-current-buffer
(with-window (select-window-interactively)
	     (animated-move-window 500 500)
	     (raise-window))

;; interactive move it back
;; interactive resize

;;;; EXECUTION AND MENUS

;; Start second xterm
(execute "nxterm -e zsh")
;; start a third one via the root menu


;;;; DECORATIONS
(window-style "XTerm" #:use-theme (load-theme "woven"))
(window-style (title-match?? "demo*") #:use-theme (load-theme "mwm"))
;; change one xterm using the menu
;; change the global theme using preferences menu

(animated-window-shade)
;; also by double-clicking on titlebar


(define w
  (select-window-interactively "Pick an XTerm" default-message-window))
(set-window-property! w 'squashed-titlebar #t)
(set-window-property! w 'squashed-titlebar #f)

;;;; ICONIFY, ICONS
(window-style "xcalc" #:icon "xcalc.xpm"
	              #:mini-icon "mini-calc.xpm"
		      #:icon-box (list 0 (y- 100) (x- 200) (y- 0))
		      #:show-icon #t)
(execute "xcalc")

(list-windows)

(map iconified? (list-windows))

(list-windows #:only iconified?)
;; de-iconify via window list

(list-windows #:only 
	      (lambda (w) (equal? (window-viewport-position w) '(0 0))))
;; window-list deiconify


;;;; VIRTUAL DESKTOP, STICKINESS, PAGER

;; N.B. Interacting with fvwm2 pager -- all in Scheme!

(set-viewport-position! 200 100)

(move-viewport 200 100)

;; make a window sticky
(stick (select-window-interactively))

;; unstick it using titlebar decoration 2nd from left

;;;; GTK INTERFACE
;; rename-window-interactively (H-r)

(use-scwm-modules defoption primopts preferences themes
		  menus-extras prompt-proc auto-raise base
		  face fvwm-compat fvwm-module 
		  (gtk gtk)
		  (gtk gdk))

(scwm-options-dialog)

;; Change menu colors, fonts
;; autoraise option

;; here's all the code it takes to
;; make a new GUI-configurable option
(define-scwm-option *menu-font* (make-font "*helvetica*medium-r*12*")
  "The default menu font."
  #:type 'font
  #:group 'menu
  #:setter (lambda (font) (set! menu-font font))
  #:getter (lambda () menu-font))


;;;; MESSAGE WINDOWS

;; Show via interactive resize/move

(display-message-briefly "Hello world!")
;; above uses timer hooks


;;;; CHANGING DESKS

;; change-desk-hook

(use-change-desk-commands
 #(
   "xsetroot -solid cyan4" 
   "xsetroot -solid navyblue" 
   ))

(define (show-desk-name new old)
  (display-message-briefly (string-append "Desk: " (number->string new))))

(add-hook! change-desk-hook show-desk-name)



;;;; WINDOW GROUP SELECTIONS

;; H-S-mouse1 to select, H-S-mouse2 to move

;; Uses window-enter-hook, window-leave-hook



;;;; CONSTRAINT SOLVER

;; turn on via root menu, create inequality & equality cns

;; C-M-A-c to list constraints, and turn off one by one



;;;; BINDINGS

(bind-mouse 'root "H-1"
	    (lambda () (display-message-briefly "Hello world")))

(bind-key 'all "H-M-r" 
	  (lambda () (display-message-briefly "Bound to keystroke")))


;;;; NETSCAPE INTERACTION
;; (be sure netscape is running)

;; C-M-S-t
;; http://vicarious-existence.mit.edu/scwm

;; All via X properties-- no C code specifically written to do the above!


;;;; SYNTHETIC EVENT SUPPORT
;; keybindings for pointer movement
;; keybindings for button presses  (click on Home of netscape, then iconify)

;; Enable send-events in an xterm (or demo in GNU Emacs)
;; C-M-S-4

;; Only need below if did not define w earlier
;; (define w (select-window-interactively))

(add-timer-hook! (sec->usec 1.5) 
		 (lambda () (X-synthetic-send-string "ls g*" w)))


;;;; WINDOW REARRANGEMENTS AND UNDO

(use-scwm-modules cascade tile undo)

(bind-key 'all "H-M-C-u" undo)
(bind-key 'all "H-M-C-r" redo)

;; (reset-undo!)

(save-window-configuration)
(save-window-configuration)

(begin
  (save-window-configuration)
  (cascade-windows (list-windows #:only (class-match?? "XTerm"))))

(undo)

(begin
  (save-window-configuration)
  (tile-windows (list-windows)))

(undo)

;;;; SCWM-BUTTONS (GTK)

(use-scwm-modules ScwmButtons)

(window-style "ScwmButtons2"
	      #:use-style desk-widget-on-top-no-titlebar)

(run-ScwmButtons
 (list
  (button-item "mini-term.xpm" #:action "xterm" #:tooltip "XTerm")
  (button-item "mini-calc.xpm" #:action "xcalc" #:tooltip "XCalc")
  (button-item "mini-xmcd.xpm" #:action "xmcd" #:tooltip "Xmcd")
  (button-item "mini-xv.xpm" #:action "xv" #:tooltip "Xv")
  (button-item "mini-gv.xpm" #:action "gv" #:tooltip "gv")
  (button-item "mini-nscape.xpm" #:action "netscape" #:tooltip "Netscape"))
;;  #:orientation 'vertical
  )


;;;; ALTERNATIVE MULTIPLE WINDOWS INTERFACES

(delete-group (selected-windows-list))

(move-group-relative 20 20 (select-multiple-windows-interactively))


;;; Local Variables:
;;; eval: (progn (set-default-font "10x20") (scwm-eval-sexp (concat "(with-window (id->window (- " (cdr (assq 'window-id (cadadr (current-frame-configuration)))) " 2)) (resize-frame-to 795 935) (move-to (x- (w%x 100)) 0))") t))
