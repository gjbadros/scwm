;;;; $Id$
;;;; Copyright (C) 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 



(define-module (app scwm fvwm-eval)
  :use-module (ice-9 string-fun)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm fvwm-compat)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm winops))



;;   fvwm-eval
;;
;; This module allows you to evaluate fvwm commands, either from a module
;; or interactively. Interactive usage is probably not terribly useful,
;; however, it would be good to support more of the fvwm commands if other
;; modules need them.

(define (get-two-numeric-args args cont)
  (let* ((s-args (split-after-char 
		  #\space 
		  (sans-leading-whitespace args)
		  (lambda a a))))
    (cont (string->number 
	   (sans-surrounding-whitespace (car s-args)))
	  (string->number 
	   (sans-surrounding-whitespace 
	    (cadr s-args))))))

(define (get-one-numeric-arg args)
  (let* ((s-args (split-after-char 
		  #\space 
		  (sans-leading-whitespace args)
		  (lambda args args))))
    (string->number 
     (sans-surrounding-whitespace (car s-args)))))

(define (get-one-string-arg args)
  (let* ((s-args (split-after-char
                  #\space
                  (sans-leading-whitespace args)
    		  (lambda args args))))
    (sans-surrounding-whitespace (car s-args))))
				    
(define fvwm-command-hash-table
  (make-hash-table 10))

(define (add-fvwm-command str proc)
  (hash-set! fvwm-command-hash-table (string-downcase!
				      (string-copy str)) proc))

(defmacro define-fvwm-command (str . body)
  `(add-fvwm-command 
    ,str (lambda (args fmod window)
	   ,@body)))

(define (calc-desk-args args)
;; arg1 [ arg2 ] [ min max ]
;; arg1==0; desk=arg2
;; arg1!=0; desk=+arg1
;; need to deal with arg3 and arg4
  (get-two-numeric-args args
    (lambda (arg1 arg2)
      (if (= arg1 0) arg2 (+ arg1 (current-desk))))))

(define-fvwm-command "Desk"
  (set-current-desk! (calc-desk-args args)))

(define-fvwm-command "Focus"
  (if window
      (focus-window window)))

(define-fvwm-command "GotoPage"
  (get-two-numeric-args 
   args (lambda (x y)
	  (set-viewport-position! 
	   (* x display-width)
	   (* y display-height)))))

(define-fvwm-command "Iconify"
  (let* ((arg (get-one-numeric-arg args))
         (fn (cond 
	      ((or (not arg) (= arg 0)) toggle-iconify)
	      ((< arg 0) deiconify-window)
	      (else iconify-window))))
    (if window
	(fn window)
	(fn))))

(define-fvwm-command "Move"
  (get-two-numeric-args
   args
   (lambda (x y)
     (if (and x y)
	 (if window
	     (move-window-viewport-position x y window)
	     (move-window-viewport-position x y))
	 (if window
	     (begin
	       (if fmod (move-window-viewport-position 
			 (car (pointer-position))
			 (cadr (pointer-position)) window))
	       (interactive-move window))
             (interactive-move))))))

(define-fvwm-command "Raise"
  (if window
      (raise-window window)
      (raise-window)))

(define-fvwm-command "Scroll"
  (get-two-numeric-args 
   args ;; the args are percentage of display size
   (lambda (x y) (move-viewport (%x x) (%y y)))))

(define-fvwm-command "Send_ConfigInfo"
  ((caddr fmod)))

;;; GJB:FIXME:MS:  How can we make it easier to
;;; debug fvwm2-module-send-window-list (that this calls?)
(define-fvwm-command "Send_WindowList"
  ((cadddr fmod)))   ;; invokes the modules's fvwm2-module-send-window-list

(define-fvwm-command "Set_mask"
  (set-car! (cdr fmod) (get-one-numeric-arg args)))

(define-fvwm-command "WindowsDesk"
  (move-window-to-desk (get-one-numeric-arg args) window))

;;; WindowsDesk is now obsoleted in favour of MoveToDesk
;;; MoveToDesk arg1 [ arg2 ] [ min max ]
(define-fvwm-command "MoveToDesk"
  (move-window-to-desk (calc-desk-args args) window))

(define-fvwm-command "KillMe"
  (display "Got KillMe! \n")
  ((list-ref 5 fmod)))

(define-fvwm-command "Eval"
  (eval-string args))

(define-fvwm-command "Exec"
  (fvwm-exec args))

(define-fvwm-command "Restart"
  (restart))

(define-fvwm-command "Quit"
  (quit))

(define-fvwm-command "Resize"
  (get-two-numeric-args
   args
   (lambda(x y)
     (if (and x y)
	 (if window
	     (resize-window x y window)
	     (resize-window x y))
	 (if window
	     (interactive-resize window)
	     (interactive-resize))))))

(define-fvwm-command "Lower"
  (if window
      (lower-window window)
      (lower-window)))

(define-fvwm-command "WarpToWindow"
  (if window
      (warp-to-window window)
      (warp-to-window)))

(define-fvwm-command "Delete"
  (if window
      (delete-window window)
      (delete-window)))

(define-fvwm-command "Destroy"
  (if window
      (destroy-window window)
      (destroy-window)))

(define-fvwm-command "Close"
  (if window
      (close-window window)
      (close-window)))

(define-fvwm-command "Maximize"
  (get-two-numeric-args
   args
   (lambda(x y)
     (if (and x y)
         (if window
	     (toggle-maximize (%x x) (%y y) window)
	     (toggle-maximize (%x x) (%y y)))
         (if window
	     (toggle-maximize (%x 100) (%y 100) window)
	     (toggle-maximize (%x 100) (%y 100)))))))

(define-fvwm-command "WindowShade"
  (let ((x (get-one-numeric-arg args)))
    (if x
	(cond
         ((= x 1) (if window (shade-window window) (shade-window)))
         ((= x 2) (if window (unshade-window window) (unshade-window))))
	(if window (toggle-window-shade window) (toggle-window-shade)))))

(define-fvwm-command "RaiseLower"
  (if window
      (toggle-raise window)
      (toggle-raise)))

(define-fvwm-command "CursorMove"
  (get-two-numeric-args
   args
   (lambda(x y)
     (if (and x y)
         (move-pointer (%x x) (%y y))))))

(define-fvwm-command "Refresh"
  (refresh))     

(define-fvwm-command "RefreshWindow"
  (if window
      (refresh-window window)
      (refresh-window)))

(define-fvwm-command "Stick"
  (if window
      (toggle-stick window)
      (toggle-stick)))

(define-fvwm-command "Beep"
  (beep))

(define-fvwm-command "Echo"
  (display args))

(define-fvwm-command "EdgeScroll"
  (get-two-numeric-args
   args
   (lambda(x y)
     (if (and x y)
         (set-edge-scroll! x y)))))

(define-fvwm-command "ExecUseShell"
  (if args
      (fvwm-exec-use-shell args)
      (fvwm-exec-use-shell)))

(define-fvwm-command "DeskTopSize"
 ;;; abit of a pain, since the arg to DeskTopSize is XxY and not X Y
  (if args
      (let* (
	     (s-args (split-after-char
		      #\x
		      (sans-leading-whitespace args)
		      (lambda a a)))
	     (x (if (< (string-length (car s-args)) 1)
		    #f
		    (string->number (sans-leading-whitespace 
				     (substring (car s-args) 0 (- (string-length (car s-args)) 1))))))
	     (y (string->number (sans-surrounding-whitespace (cadr s-args))))
	     )
	(if (and x y)
	    (set-desk-size! x y)))))

(define-fvwm-command "EdgeResistance"
  (get-two-numeric-args
   args
   (lambda(x y)
     (if (and x y)
         (set-edge-resistance! x y)))))

(define-fvwm-command "XORvalue"
  (let ((x (get-one-numeric-arg args)))
    (if x
	(set-rubber-band-mask! x))))

(define (clean l)
  (if (null? l) '()
      (if (car l)
	  (cons (car l)(clean (cdr l)))
	  (clean (cdr l)))))

(define (words s)
  (clean (separate-fields-discarding-char #\space s (lambda a a))))

(define-fvwm-command "SetAnimation"
  (set-animation!
   (list->vector 
    (clean
     (words args)))))

;; FIXMS: set-opaque-move-size is broken! for now.

;;(define-fvwm-command "OpaqueMoveSize"
;;  (let ((x (get-one-numeric-arg args)))
;;    (if x
;;	(set-opaque-move-size! x))))


(define-fvwm-command "ClickTime"
  (let ((x (get-one-numeric-arg args)))
    (if x
	(set-click-delay! x))))

(define-fvwm-command "HiLightColor"
 ;;; HilightColor textcolour bgcolour
 ;;; lets assume colournames are spaceless
  (let (
	(s-args (separate-fields-discarding-char
		 #\space
		 (sans-leading-whitespace args)
		 (lambda a a)))
	)
    (set-highlight-foreground! (car s-args))
    (set-highlight-background! (cadr s-args))))

(define-fvwm-command "Recapture"
 ;;; running this seems to screw my display up
  (recapture))

(define fvwm-flags-table
  (make-hash-table 7))

(define (add-flag s fn)
  (hash-set! fvwm-flags-table (string-downcase! (string-copy s)) fn))

(add-flag "Iconic" iconified-window?)
(add-flag "Visible" visible?)
(add-flag "Sticky" sticky-window?)
(add-flag "Maximized" maximized?)
(add-flag "Transient" transient?)
(add-flag "Raised" raised?)
(add-flag "CurrentDesk" on-current-desk?)
(add-flag "CurrentPage" visible?)
(add-flag "CurrentPageAnyDesk" in-viewport-any-desk?)

(define (extract-conditions s)
  (let (
	(x (string-index s #\[))
	(y (string-index s #\]))
	)
    (if (and x y)
	(substring s (+ x 1) y)
	#f)))

(define (extract-command s)
  (sans-surrounding-whitespace
      (let ((end-of-conditions (string-index s #\])))
           (if end-of-conditions
	       (substring s (+ 1 end-of-conditions) (string-length s))
	       s))))

(define (parse-flag s)
  (let* (
	 (invert (equal? #\! (string-ref s 0)))
	 (reals (if invert (substring s 1 (string-length s)) s))
	 (lc-reals (string-downcase! (string-copy reals)))
	 (tr (hash-ref fvwm-flags-table lc-reals))
	 )
    (cons
     (not invert)
     (if tr
	 tr
	 (wildcard-matcher reals)))))

(define (parse-conditions s)
  (define (just l a)
    (if (null? l) '()
	(if (equal? a (caar l))
	    (cons (cdr (car l))
		  (just (cdr l) a))
	    (just (cdr l) a))))
  (let* (
	 (c (extract-conditions s))
	 (flags (if c (words c) '()))
	 (fns (map parse-flag flags))
	 (o (just fns #t))
	 (e (just fns #f))
	 )
    (cons
     (if (not (null? o)) o '())
     (if (not (null? e)) e '()))
    )
  )

(define-fvwm-command "Next"
  (let ((c (parse-conditions args)))
    (next-window #:only (car c) #:except (cdr c)
		 #:proc (lambda(x)(eval-fvwm-command
				   (extract-command args) fmod x)))))

(define-fvwm-command "Prev"
  (let ((c (parse-conditions args)))
    (prev-window #:only (car c) #:except (cdr c)
		 #:proc (lambda(x)(eval-fvwm-command
				   (extract-command args) fmod x)))))

(define* (fvwm-none thunk #&key (only '()) (except '()))
  (if (null? (list-windows #:only only #:except except))
      (thunk)))

(define-fvwm-command "None"
  (let ((c (parse-conditions args)))
    (fvwm-none (lambda () 
		 (eval-fvwm-command (extract-command args) fmod))
	       #:only (car c) #:except (cdr c))))

(define-fvwm-command "Current"
  (let ((c (parse-conditions args))
        (w (window-with-focus)))
    (if (filter-only-except (list w) (car c) (cdr c))
	(eval-fvwm-command (extract-command args) fmod w))))

(define-fvwm-command "IconFont"
  (set-icon-font! (sans-surrounding-whitespace args)))

(define-fvwm-command "WindowFont"
  (set-title-font! (sans-surrounding-whitespace args)))

(define (parse-context s)
  (define (parse-a-context c)
    (cond
     ((equal? c #\R) 'root)
     ((equal? c #\W) 'window)
     ((equal? c #\T) 'title)
     ((equal? c #\S) 'sidebar)
     ((equal? c #\F) 'frame)
     ((equal? c #\I) 'icon)
 ;;;sod it..lets just do the button numbers the long way..
     ((equal? c #\0) 'button-0)
     ((equal? c #\1) 'button-1)
     ((equal? c #\2) 'button-2)
     ((equal? c #\3) 'button-3)
     ((equal? c #\4) 'button-4)
     ((equal? c #\5) 'button-5)
     ((equal? c #\6) 'button-6)
     ((equal? c #\7) 'button-7)
     ((equal? c #\8) 'button-8)
     ((equal? c #\9) 'button-9)
     ((equal? c #\0) 'button-10)
     ((equal? c #\A) 'all) ; not sure of thats the correct mapping
     ))
  (map parse-a-context (string->list s))
  )

(define (flatten l)
  (if (null? l)
      '()
      (append (car l)(flatten (cdr l)))))

(define (parse-modifier s)
;;; C Control S Shift M Meta A Any N None
;;; C-        S-      A-     ?-    ?-
  (list->string
   (flatten
    (map
     (lambda(x)
       (cond
	((equal? x #\C) (list #\C #\-))
	((equal? x #\S) (list #\S #\-))
	((equal? x #\M) (list #\A #\-))
	((equal? x #\A) '())
	((equal? x #\N) '())))
     (string->list s)))))

(define-fvwm-command "Mouse"
 ;;; Mouse Mouse-button region modifier command
 ;;; (bind-mouse CONTEXTS MOUSE-SPECIFIER PROC)
  (let* (
	 (largs (map sans-surrounding-whitespace
		     (separate-fields-discarding-char
		      #\space
		      (sans-surrounding-whitespace args)
		      (lambda a a))))
	 (button-string (car largs))
	 (context-string (cadr largs))
	 (contexts (parse-context context-string))
	 (modifier-string (caddr largs))
	 (modifier (parse-modifier modifier-string))
	 (command-string (cadddr largs))
	 )
    (display 'mouse) (write contexts)(newline) (write modifier)(newline)
    (write button-string)(newline) (write command-string)(newline)
    (bind-mouse contexts (string-append modifier button-string)
 ;;; do we get a window??
		(lambda()
		  (eval-fvwm-command command-string fmod))))
  )

(define-fvwm-command "Key"
 ;;; Key keyname Context Modifiers Function
 ;;; (bind-key CONTEXTS KEY PROC)
  (let* (
	 (largs (map sans-surrounding-whitespace
		     (separate-fields-discarding-char
		      #\space
		      (sans-surrounding-whitespace args)
		      (lambda a a))))
	 (key-string (car largs))
	 (context-string (cadr largs))
	 (contexts (map (lambda(x)(if (equal? x 'any) 'all x))
			(parse-context context-string)))
	 (modifier-string (caddr largs))
	 (modifier (parse-modifier modifier-string))
	 (command-string (cadddr largs))
	 )
    (display 'key) (write contexts)(newline) (write modifier)(newline)
    (write key-string)(newline) (write command-string)(newline)
    (bind-key contexts (string-append modifier key-string)
 ;;; do we get a window??
	      (lambda()
		(eval-fvwm-command command-string fmod))))
  )
		    
(define-fvwm-command "ChangeDecor"
      (if window
            (let* ((arg (get-one-string-arg args)))
	        (set-window-decor! window (eval (string->symbol arg))))))



(define*-public (eval-fvwm-command command #&optional (fmod #f) 
				   (window #f))
  "Evaluate an fvwm2 command.
Implemented for compatibility with fvwm2 modules, which can send
commands back to the window manager for evaluation.  Not all fvwm2
commands are implemented; see the end of fvwm-eval.scm for a list of
working commands."
  (let* ((split-result (split-before-char #\space command 
					  (lambda args args)))
	 (main-cmd (car split-result))
	 (lc-cmd (string-downcase! (string-copy main-cmd)))
	 (args (cadr split-result)))
    ((hash-ref fvwm-command-hash-table lc-cmd) args fmod window)))

;;; Implemented and Tested:
;;; Beep        EdgeResistance  Maximize        Resize          WarpToWindow
;;; ClickTime   EdgeScroll      Move            Restart         WindowsDesk
;;; Close       Exec            Quit            Scroll          WindowShade
;;; CursorMove  GotoPage        OpaqueMoveSize  Send_ConfigInfo XORValue
;;; Delete      HilightColor    Raise           Send_WindowList
;;; Desk        Focus           RaiseLower      set_mask
;;; DesktopSize Iconify         Refresh         SetAnimation
;;; Destroy     Lower           RefreshWindow   Stick
;;; MoveToDesk (7th July 1999)
;;; Current (17th October 1999)
;;; ChangeDecor (17th October 1999) 

;;; Implemented and Untested or broken:
;;;                    IconFont        Next            Recapture
;;; Echo               Key             None            WindowFont
;;; ExecUseSHELL       Mouse           Prev

;;; Unimplemented:
;;; AddButtonStyle     DestroyMenu             QuitScreen
;;; AddModuleConfig    DestroyModuleConfig     Read
;;; AddTitleStyle      FlipFocus               SendToModule
;;; AddToDecor         Function                SetEnv
;;; AddToFunc          GlobalOpts              SetMenuDelay
;;; AddToMenu          IconPath                Style
;;; AnimatedMove       KillModule              Title
;;; BorderStyle        Menu                    TitleStyle
;;; ButtonStyle        Menustyle               UpdateDecor
;;;                    Module                  Wait
;;; ColorLimit         ModulePath              WindowId
;;; ColormapFocus      Nop                     WindowList
;;; CursorStyle        PipeRead                +
;;; DestroyDecor       PixmapPath
;;; DestroyFunc        PopUp
