;;;; $Id$
;;;; Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
;;;; This file gets compiled directly into scwm
;;;; Scwm will eval these commands before reading
;;;; any other .scm file
;;;; In a sense, these are compiled-in primitives implemented in scheme
;;;; (these can get overridden later, of course)

(use-modules (app scwm optargs))

(define-public guile-version (+ (string->number (major-version)) 
				(/ (string->number (minor-version)) 10)))

;; Turn off buffering so that we can see messages as
;; they are displayed (an issue in >= guile-1.3.2)
(if (> guile-version 1.3)
    (setvbuf (current-output-port) _IONBF))

;; Make quit an alias for scwm-quit
(define quit scwm-quit)
(undefine scwm-quit)

(define FIXED-FONT (make-font "fixed"))

;;; Make some colors

;;; Set some global options
(set-not-menu-foreground! "black")
(set-not-menu-background! "gray")


(set-highlight-foreground! "white")
(set-highlight-background! "navyblue")
(set-icon-font! FIXED-FONT)
(set-title-font! FIXED-FONT)
(set-title-justify! 'center)

;; temporary definitions for bootstrapping, use winops.scm to
;; redefine properly.
(define hack-interactive-move rubber-band-move)
(define hack-interactive-resize rubber-band-resize)


;;; Some functions for decoration bindings
(define* (resize-or-raise)
  "Perform a resize, raise, or lower based on the mouse-event-type.
To be bound to a window decoration: click does `raise-window',
motion does `interactive-resize', and double-click does
`lower-window'."
  (interactive)
  (case (mouse-event-type)
    ((click) (raise-window))
    ((motion) (hack-interactive-resize))
    ((double-click) (lower-window))))

(define* (move-or-raise)
  "Perform a move, raise, or lower based on the mouse-event-type.
To be bound to a window decoration: click does `raise-window',
motion does `interactive-move', and double-click does
`lower-window'."
  (interactive)
  (case (mouse-event-type)
    ((click) (raise-window))
    ((motion) (hack-interactive-move))
    ((double-click) (lower-window))))

;;; Initialize the decoration bindings to
;;; permit at least some useful behaviour
(bind-mouse 'frame-corners 1 resize-or-raise)

(bind-mouse '(title frame-sides) 1 move-or-raise)

(bind-mouse 'icon 1 deiconify-window)

(let ((default-menu (make-menu 
		     (list
		      (make-menuitem "Default Menu" #f)
		      (make-menuitem "Exit SCWM" quit))
		     (make-color "gray") (make-color "black")
		     (make-color "slate gray") FIXED-FONT)))
  (bind-mouse 'root 1 (lambda () (popup-menu default-menu))))

;; GJB:FIXME:: Here I trade flexibility
;; for safety.  If this ever becomes an issue
;; we should consider exposing the X-grab-server, X-ungrab-server
;; primitives
(define (with-grabbed-server thunk)
  "Execute THUNK with the X server grabbed."
  #f)

(let ((xgs X-grab-server)
      (xugs X-ungrab-server))
  (set! with-grabbed-server (lambda (thunk)
			      (dynamic-wind xgs thunk xugs))))

;; now undefine the dangerous primitives
(undefine X-grab-server)
(undefine X-ungrab-server)

;; END gross hack

(if (not (defined? 'run-hook))
    ;; GJB:FIXME:MS: I'd like a backtrace when a hook fails
    (define-public (run-hook hook-list . args)
      "Runs the procedures in HOOK-LIST, each getting ARGS as their arguments.
If any error, the others still run.  The procedures are executed in the
order in which they appear in HOOK-LIST"
      (for-each (lambda (p) 
		  (catch #t
			 (lambda () (apply p args))
			 (lambda args
			   (display "Error running hook: ")
			   (write p)
			   (newline))))
		hook-list)))


(if (not (defined? 'reset-hook!))
    (defmacro-public reset-hook! (hook)
      `(set! ,hook ())))

(if (not (defined? 'make-hook))
    (begin
      ;; guile-1.3
      (define-public (make-hook . n) ())
      (define-public hook? list?))
    ;; guile-1.3.2 and later
    (define-public (hook? h) 
      (and (pair? h) (eq? (car h) 'hook))))

(define-public (append-hook! hook proc)
  "Add PROC to HOOK at the end of the list."
  (add-hook! hook proc #t))

;; GJB:FIXME:: this should not be public,
;; but I leave it public for now for easier debugging --07/03/99 gjb
(define-public *scwm-modules* '())

(define-public (scwm-module-loaded? module)
  "Return #t iff MODULE has been loaded."
  (let ((entry (assoc module *scwm-modules*))) 
    (and entry (null? (cdr entry)))))

;;; GJB:FIXME:G1.3.2:  This might work in guile-1.3.2
;;  (environment-bound? module-environment name))


(define (use-scwm-module-note-success module)
  (let ((entry (assoc module *scwm-modules*)))
    (if (not entry)
	(set! *scwm-modules* (cons (cons module '()) *scwm-modules*))
	(let ((eval-after-load-proc (cdr entry)))
	  (if (not (null? eval-after-load-proc))
	      (let ((answer (eval-after-load-proc)))
		(set-cdr! entry '())
		answer))))))

(define-public (eval-after-load module proc)
  "Run PROC after MODULE is loaded.
Run PROC immediately if MODULE has already been loaded."
  (if (scwm-module-loaded? module)
      (proc)
      (set! *scwm-modules* (cons (cons module proc) *scwm-modules*))))

(define (process-use-scwm-module module)
  (if (symbol? module)
      (set! module (append '(app scwm) (list module))))
  (catch #t
	 (lambda ()
	   (process-use-modules (list module))
	   (use-scwm-module-note-success module)
	   (run-hook load-processing-hook -1)
	   module)
	 (lambda (key . args)
	   (display "Error loading module: ")
	   (display module) (newline)
	   (catch #t
		  (lambda () 
		    (apply handle-system-error (cons key args)) 
		    (backtrace))
		  (lambda (key . args) #t))
	   #f)))

(define-public (process-use-scwm-modules module-list)
  "Returns a list of all the modules loaded in successfully.
Modules that failed to load have #f in their place in the
list instead of the module."
  (map process-use-scwm-module (reverse module-list)))

(defmacro use-scwm-modules modules
  `(process-use-scwm-modules ',modules))

(X-property-set! 'root-window "_WIN_WM_NAME" "scwm")
(X-property-set! 'root-window "_WIN_WM_VERSION" (scwm-version))
