;;; $Id$
;;; flux.scm
;;; (C) 1998 Sam Steingold, Greg J. Badros, and Maciej Stachowiak
;;;
;;; This are functions used by various sample .scwmrc, but not necessarily
;;; stabilized even as well as the other files in scheme/*.scm
;;; Expect the semantics of these functions to change, and don't
;;; be surprised if some even completely disappear (as we figure out a better
;;; way to do things)



(define-module (app scwm flux)
  :use-module (app scwm base)
  :use-module (app scwm optargs))



(define-public (write-all port . lst)
  (if (eq? port ()) (set! port (current-output-port)))
  (do ((zz lst (cdr zz))) ((null? zz))
    (if (string? (car zz)) (display (car zz) port) (write (car zz) port))))

(define-public (quotify-single-quotes str)
  (regexp-substitute/global #f "'" str 'pre "'\"'\"'" 'post))

;;; FIXGJB: how set width of an xmessage?
(define-public (message . str)		; return lambda
  (execute (string-append "echo -e \'" 
			  (quotify-single-quotes (apply string-append str))
			   "\'| xmessage -file - -default okay -nearmouse")))

(define-public (show-mesg . str) (lambda () (apply message str)))
(define-public (show-file fl)		; return lambda
  (exe (string-append
	"xmessage -buttons ok:0 -default ok -nearmouse -file " fl)))

(define-public (bool->str arg) (if arg "true" "false"))

(define*-public (size->str sz #&optional (sep "x"))
  (let ((xx (car sz)) (yy (cadr sz)))
    (string-append (number->string xx) sep (number->string yy))))

(define*-public (window-info #&optional (ww (selected-window)))
  (message
   "Window ID:\t\t" (number->string (window-id ww))
   "\nTitle:\t\t\t\"" (window-title ww) "\"\nPosition:\t\t"
   (size->str (window-position ww)) "\nSize:\t\t\t"
   (size->str (window-size ww))
   "\nDesk:\t\t\t" (number->string (window-desk ww)) "\nClass:\t\t\t\""
   (window-class ww) "\"\nResource:\t\t\"" (window-resource ww)
   "\"\nBorder normal:\t\t" (bool->str (border-normal? ww))
   "\nDeletable:\t\t" (bool->str (window-deletable? ww))
   "\nIconified:\t\t" (bool->str (iconified? ww))
   "\nKept on top:\t\t" (bool->str (kept-on-top? ww))
   "\nRaised:\t\t\t" (bool->str (raised? ww))
   "\nShaded:\t\t\t" (bool->str (window-shaded? ww))
   "\nSticky Icon:\t\t" (bool->str (icon-sticky? ww))
   "\nSticky:\t\t\t" (bool->str (sticky? ww))
   "\nTitle bar shown:\t" (bool->str (titlebar-shown? ww))))


(define-public (make-menuitems-from-menu-information-list menu-info-list)
  (cons menu-title
	(map (lambda (elem) 
	       (let ((title (car elem))
		     (mini-icon (cadr elem))
		     (icon (caddr elem))
		     (exename (cadddr elem)))
		 (if (program-exists? exename)
		     (menuitem title
			       #:image-left (if mini-icon (string-append "mini-" mini-icon ".xpm") #f)
			       #:icon (if icon (string-append icon ".xpm") #f)
			       #:action (lambda () (execute exename)))
		     #f)))
	     menu-info-list)))


(define-public (animated-move-to x y)
  (let* ((w (get-window))
	 (size (window-size w))
	 (width (car size))
	 (height (cadr size))
	 (position (window-position w))
	 (oldx (car position))
	 (oldy (cadr position)))
    ;;; FIXGJB: is there a better way to correct for width, height, etc
    ;;; MS: perhaps we should make move-to take #f for either coordinate,
    ;;; which means don't move along that coord?
    (if (equal? x 'x) (set! x oldx))
    (if (equal? y 'y) (set! y oldy))
    ;; MS: see base.scm for x- and y-, which I think are a more genric
    ;; way to handle negative positions in the traditional X11 way.
    (if (< x 0) (set! x (- root-size-x (abs x) width)))
    (if (< y 0) (set! y (- root-size-y (abs y) height)))
    (raise-window w)
    (move-to x y w 'animated 'move-pointer-too)))

(define-public (key-mouse-moves modifiers pct-of-screen left down up right)
  (bind-key 'all (string-append modifiers "-" left) 
	    (lambda () (move-pointer (%x (- pct-of-screen)) 0)))
  (bind-key 'all (string-append modifiers "-" down) 
	    (lambda () (move-pointer 0 (%y pct-of-screen))))
  (bind-key 'all (string-append modifiers "-" up) 
	    (lambda () (move-pointer 0 (%y (- pct-of-screen)))))
  (bind-key 'all (string-append modifiers "-" right) 
	    (lambda () (move-pointer (%x pct-of-screen) 0))))

(define-public (key-viewport-moves modifiers pct-of-screen left down up right)
  (bind-key 'all (string-append modifiers "-" left) 
	    (lambda () (move-viewport (%x (- pct-of-screen)) 0)))
  (bind-key 'all (string-append modifiers "-" down) 
	    (lambda () (move-viewport 0 (%y pct-of-screen))))
  (bind-key 'all (string-append modifiers "-" up) 
	    (lambda () (move-viewport 0 (%y (- pct-of-screen)))))
  (bind-key 'all (string-append modifiers "-" right) 
	    (lambda () (move-viewport (%x pct-of-screen) 0))))

