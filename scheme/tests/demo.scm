;; $Id$
;; -*- scwm -*-

;;;; SCWM-MODE BASICS
;; first show scwmexec

(+ 1 2)
(select-window-interactively)

(move-window-relative 25 25 (select-window-interactively))
(move-window-relative -25 -25 (select-window-interactively))

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
(window-style "XTerm" #:use-theme (load-theme "gjb"))
(window-style (title-match?? "demo*") #:use-theme (load-theme "mwm"))
;; change one xterm using the menu
;; change the global theme using preferences menu

(animated-window-shade)
(get-window #t)
(select-viewport-position)
(window-context)

;; also by double-clicking on titlebar

(define w
  (select-window-interactively "Pick an XTerm" default-message-window))
(set-window-property! w 'squashed-titlebar #t)
(set-window-property! w 'squashed-titlebar #f)

;;;; ICONIFY, ICONS

;; iconify 3rd xterm

(window-style "xcalc" #:icon "xcalc.xpm"
	              #:mini-icon "mini-calc.xpm"
		      #:icon-box (list 0 (y- 100) (x- 200) (y- 0))
		      #:show-icon #t)
(execute "xcalc")

;; iconify it via decoration button

(list-windows)

(map iconified-window? (list-windows))

(list-windows #:only iconified-window?)
(list-windows #:only (lambda (w) (string=? (window-class w) "XTerm")))
(list-windows #:only (wildcard-matcher "XTerm"))
;; de-iconify via window list


;; arbitrary predicate!
(list-windows #:only 
	      (lambda (w) (equal? (window-viewport-position w) '(0 0))))
;; window-list deiconify

(define w (select-window-interactively))

(define wid (car (window-size w)))
(define height (cadr (window-size w)))
wid
height
(resize-window wid height w)
(resize-window (* wid 2) (* height 2) w)

(move-window-relative 20 -50 w )

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
;; http://scwm.mit.edu/scwm
;; file:///home/gjb/uist99/screen-shot-eg/table-example.html

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

(use-scwm-modules cascade tile undo)

;; H-s is insert-undo-global

;; (reset-undo!)

;; (insert-undo-global)

(begin
  (insert-undo-global)
  (cascade-windows (list-windows #:only (class-match?? "XTerm"))))

;; H-/ is undo
;; H-M-/ is redo
;; (undo)

(begin
  (insert-undo-global)
  (tile-windows (list-windows #:only (win-not?? sticky?))))

;; (undo)

;;;; SCWM-BUTTONS (GTK)

(use-scwm-modules ScwmButtons)

(window-style "ScwmButtons"
	      #:use-style desk-widget-on-top-no-titlebar)

;; little icons
(run-ScwmButtons
 (list
  (button-item "mini-term.xpm" #:action "xterm" #:tooltip "XTerm")
  (button-item "mini-calc.xpm" #:action "xcalc" #:tooltip "XCalc")
  (button-item "mini-xmcd.xpm" #:action "xmcd" #:tooltip "Xmcd")
  (button-item "mini-xv.xpm" #:action "xv" #:tooltip "Xv")
  (button-item "mini-gv.xpm" #:action "gv" #:tooltip "gv")
  (button-item "mini-nscape.xpm" #:action "netscape" #:tooltip "Netscape"))
 #:auto-orient #t
 #:orientation 'vertical)

;; big icons
(run-ScwmButtons
 (list
  (button-item "term.xpm" #:action "xterm" #:tooltip "XTerm")
  (button-item "xcalc.xpm" #:action "xcalc" #:tooltip "XCalc")
  (button-item "xmcd.xpm" #:action "xmcd" #:tooltip "Xmcd")
  (button-item "xv.xpm" #:action "xv" #:tooltip "Xv")
  (button-item "gv.xpm" #:action "gv" #:tooltip "gv")
  (button-item "nscape.xpm" #:action "netscape" #:tooltip "Netscape"))
 #:auto-orient #t
 #:orientation 'horizontal)


;;;; ALTERNATIVE MULTIPLE WINDOWS INTERFACES

(delete-group (selected-windows-list))

(move-group-relative 20 20 (select-multiple-windows-interactively))


;;;; LEARNING SCWM
;; file:///home/gjb/scwm/doc/html/book1.htm

;; Would love the docbook->info converter!

;; tab-completion

;; list-windows C-h C-s 

;; window-enter-hook

;; *theme-path*


;;; Local Variables:
;;; eval: (progn (set-default-font "10x20") (scwm-eval-sexp (concat "(with-window (id->window (- " (cdr (assq 'window-id (cadadr (current-frame-configuration)))) " 2)) (resize-frame-to 795 935) (move-to (x- (w%x 100)) 0))") t))
