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
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm optargs))



(define-public (interactive-move-window-with-focus)
  (let ((w (current-window-with-focus))) (and w (interactive-move w))))

(define-public (interactive-resize-window-with-focus)
  (let ((w (current-window-with-focus))) (and w (interactive-resize w))))

(define-public (interactive-move-window-with-pointer)
  (let ((w (current-window-with-pointer))) (and w (interactive-move w))))

(define-public (interactive-resize-window-with-pointer)
  (let ((w (current-window-with-pointer))) (and w (interactive-resize w))))

(define-public (wiggle-window)
  (let ((w (get-window))) (window-shade w #t) (un-window-shade w #t)))

(define-public (write-all port . lst)
  (if (eq? port ()) (set! port (current-output-port)))
  (do ((zz lst (cdr zz))) ((null? zz))
    (if (string? (car zz)) (display (car zz) port) (write (car zz) port))))

(define-public (to-string . rest)
  (with-output-to-string (lambda () (apply write-all () rest))))

(define-public (make-file-menu file . rest)
  (menu (append! (list (menuitem "View" #:action (show-file file))
		       (menuitem "Edit" #:action
				 (string-append (or (getenv "EDITOR") "gvim")
						file)))
		 rest)))

(define-public (quotify-single-quotes str)
  (regexp-substitute/global #f "'" str 'pre "'\"'\"'" 'post))

;;; FIXGJB: how set width of an xmessage?
(define-public (message . str)
  (execute (string-append "echo -e \'"
			  (quotify-single-quotes (apply string-append str))
			   "\'| xmessage -file - -default Okay -nearmouse")))

(define-public (show-mesg . str) (lambda () (apply message str)))
(define-public (show-file fl)	; return lambda
  (exe (string-append
	"xmessage -buttons ok:0 -default Okay -nearmouse -file " fl)))
(define-public (show-com com) ; return lambda
  (exe (string-append com "| xmessage -file - -default Okay -nearmouse")))

(define-public (bool->str arg) (if arg "true" "false"))

(define*-public (size->str sz #&optional (sep "x"))
  (let ((xx (car sz)) (yy (cadr sz)))
    (string-append (number->string xx) sep (number->string yy))))

(define*-public (window-info #&optional (ww (selected-window)))
  (message
   "Window ID:\t\t" (number->string (window-id ww))
   "\nWindow Frame ID:\t" (number->string (window-frame-id ww))
   "\nTitle:\t\t\t\"" (window-title ww) "\"\nPosition:\t\t"
   (size->str (window-position ww)) "\nSize:\t\t\t"
   (size->str (window-size ww))
   "\nDesk:\t\t\t" (number->string (window-desk ww)) "\nClass:\t\t\t\""
   (window-class ww) "\"\nResource:\t\t\"" (window-resource ww)
   "\"\nBorder Normal:\t\t" (bool->str (border-normal? ww))
   "\nDeletable:\t\t" (bool->str (window-deletable? ww))
   "\nIconified:\t\t" (bool->str (iconified? ww))
   "\nKept On Top:\t\t" (bool->str (kept-on-top? ww))
   "\nRaised:\t\t\t" (bool->str (raised? ww))
   "\nShaded:\t\t\t" (bool->str (window-shaded? ww))
   "\nSticky Icon:\t\t" (bool->str (icon-sticky? ww))
   "\nSticky:\t\t\t" (bool->str (sticky? ww))
   "\nTitle Bar Shown:\t" (bool->str (titlebar-shown? ww))))

(define-public (show-system-info)
  (let ((vv (X-version-information)) (dd (X-display-information)))
    (apply
     message "Guile verion:\t" (version)
     "\nSCWM version:\t" (scwm-version)
     "\nRestarted:\t" (bool->str (restarted?))
     "\nDisplay Size:\t" (size->str (display-size))
     "\nDesk Size:\t" (size->str (desk-size))
     "\nViewport:\t" (size->str (viewport-position))
     "\nPointer:\t" (size->str (pointer-position))
     "\nCurrent Desk:\t" (number->string (current-desk))
     "\nX vendor:\t" (caddr vv) "; version: " (number->string (car vv)) "."
     (number->string (cadr vv)) "; release: " (number->string (cadddr vv))
     "\nX Display:\n\tResolution:\t" (size->str dd) "\n\tColor:\t\t"
     (list-ref dd 4) " (depth: " (number->string (caddr dd))
     "; bits per RGB: " (number->string (cadddr dd)) ")\nimage-load-path:"
     (map (lambda (st) (string-append "\n\t" st)) image-load-path))))

(define-public (make-menuitems-from-menu-information-list menu-info-list)
  (cons menu-title
	(map (lambda (elem)
	       (let ((title (car elem))
		     (mini-icon (cadr elem))
		     (icon (caddr elem))
		     (exename (cadddr elem)))
		 (if (program-exists? exename)
		     (menuitem title
			       #:image-left (if mini-icon
						(string-append
						 "mini-" mini-icon ".xpm") #f)
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

(define-public (sleep-ms ms)
  (select '() '() '() 0 (* 1000 ms)))

;; Does not work for arbitrary strings since, e.g,. ' ' needs
;; to be sent as "space"  FIXGJB: Do we have a char->keysym fn?
(define-public (X-synthetic-send-string str)
  (let ((w (get-window))
	(i 0))
    (while (< i (length str))
	   (send-key-press (substring str i (+ 1 i)) w)
	   (set! i (+ 1 i)))))

