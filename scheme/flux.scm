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



(define-public HOME (getenv "HOME"))
(define-public USER (getenv "USER"))
(define-public user-init-file (string-append HOME "/.scwmrc"))

;; The #t arguments should perhaps instead be a closure 
;; returning whether an opaque move/resize is desired

(define-public (interactive-move-window-with-focus)
  (let ((w (current-window-with-focus))) (and w (interactive-move w #t))))

(define-public (interactive-resize-window-with-focus)
  (let ((w (current-window-with-focus))) (and w (interactive-resize w #t))))

(define-public (interactive-move-window-with-pointer)
  (let ((w (current-window-with-pointer))) (and w (interactive-move w #t))))

(define-public (interactive-resize-window-with-pointer)
  (let ((w (current-window-with-pointer))) (and w (interactive-resize w #t))))

(define-public (toggle-max-vert) (toggle-maximize 0 (%y 100)))
(define-public (toggle-max-horz) (toggle-maximize (%x 100) 0))
(define-public (toggle-max-both) (toggle-maximize (%x 100) (%y 100)))
(define-public (toggle-max-vert-part) (toggle-maximize 0 (%y 95)))

(define-public (wiggle-window)
  (let ((w (get-window))) (window-shade w #t) (un-window-shade w #t)))

(define-public (write-all port . lst)
  "Write all arguments into the port. #t means `current-output-port'."
  (if (eq? port #t) (set! port (current-output-port)))
  (do ((zz lst (cdr zz))) ((null? zz))
    (if (string? (car zz)) (display (car zz) port) (write (car zz) port))))

(define-public (to-string . rest)
  "Dump all arguments into a string."
  (with-output-to-string (lambda () (apply write-all #t rest))))

(define*-public (move-window-to-viewport xx yy #&optional ww)
  "Move the window to the viewport; the first one being (0 0)."
  (let ((pos (window-position ww)) (sz (display-size))
        (vp (viewport-position)))
    (move-to (+ (* xx (car sz)) (- (car vp)) (modulo (car pos) (car sz)))
             (+ (* yy (cadr sz)) (- (cadr vp))
                (modulo (cadr pos) (cadr sz))) ww)))

(define-public (in-viewport xx yy)
  "Return a function of one argument, a window, moving it to the viewport."
  (lambda (ww) (move-window-to-viewport xx yy ww)))

(define-public (system-info-string)
  "Return a string with various system information.
Use `show-system-info' to display it in a window."
  (let ((vv (X-version-information)) (dd (X-display-information)))
    (apply
     to-string "Guile verion:\t\t" (version)
     "\nLibguile timestamp:\t" (libguile-config-stamp)
     "\nSCWM version:\t\t" (scwm-version)
     "\nRestarted:\t\t" (bool->str (restarted?))
     "\nDisplay Size:\t\t" (size->str (display-size))
     "\nDesk Size:\t\t" (size->str (desk-size))
     "\nViewport:\t\t" (size->str (viewport-position))
     "\nPointer:\t\t" (size->str (pointer-position))
     "\nCurrent Desk:\t\t" (number->string (current-desk))
     "\nX vendor:\t\t" (caddr vv) "; version: " (number->string (car vv)) "."
     (number->string (cadr vv)) "; release: " (number->string (cadddr vv))
     "\nX Display:\n\tResolution:\t" (size->str dd) "\n\tColor:\t\t"
     (list-ref dd 4) " (depth: " (number->string (caddr dd))
     "; bits per RGB: " (number->string (cadddr dd)) ")\nimage-load-path:\n"
     (map (lambda (st) (string-append "\t" st "\n")) image-load-path))))

(define-public (make-file-menu file . rest)
  (menu (append! (list (menuitem "View" #:action (show-file file))
		       (menuitem "Edit" #:action
				 (string-append (or (getenv "EDITOR") "gvim")
						" " file)))
		 rest)))

(define-public (quotify-single-quotes str)
  (regexp-substitute/global #f "'" str 'pre "'\"'\"'" 'post))

;;; FIXGJB: how set width of an xmessage?
(define-public (message . str)
  "Display the string arguments in a message window."
  (execute (string-append "echo -e \'"
			  (quotify-single-quotes (apply string-append str))
			   "\'| xmessage -file - -default okay -nearmouse")))

(define-public (show-mesg . str) (lambda () (apply message str)))
(define-public (show-file fl)	; return lambda
  (exe (string-append "xmessage -default okay -nearmouse -file " fl)))
(define-public (show-com com)   ; return lambda
  (exe (string-append com "| xmessage -file - -default okay -nearmouse")))

(define-public (bool->str arg) (if arg "true" "false"))

(define*-public (size->str sz #&optional (sep "x"))
  "Convert a two-element list to a string.
Use the optional second argument as the separator."
  (string-append (number->string (car sz)) sep (number->string (cadr sz))))

(define*-public (window-info #&optional (ww (get-window)))
  "Display information about a window in a message window."
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
   "\nTransient:\t\t" (bool->str (transient? ww))
   "\nRaised:\t\t\t" (bool->str (raised? ww))
   "\nShaded:\t\t\t" (bool->str (window-shaded? ww))
   "\nSticky Icon:\t\t" (bool->str (icon-sticky? ww))
   "\nSticky:\t\t\t" (bool->str (sticky? ww))
   "\nTitle Bar Shown:\t" (bool->str (titlebar-shown? ww))))

(define-public (show-system-info) (message (system-info-string)))

(define-public (make-menuitems-from-menu-information-list menu-info-list)
  (cons menu-title
	(cons menu-separator
	      (map (lambda (elem)
		     (let ((title (car elem))
			   (mini-icon (cadr elem))
			   (icon (caddr elem))
			   (exename (cadddr elem)))
		       (if (program-exists? exename)
			   (menuitem
                            title #:action exename #:image-left
                            (if mini-icon
                                (string-append "mini-" mini-icon ".xpm") #f)
                            #:icon (if icon (string-append icon ".xpm") #f))
			   #f)))
		   menu-info-list))))

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

(define-public doc-files        ; '("/usr/src/scwm/doc/scwm-procedures.txt")
  (map (lambda (s) (string-append (scwm-path-prefix) "/share/scwm/" s))
       '("scwm-procedures.txt" "cassowary_scm-procedures.txt")))

(define*-public (documentation func #&optional (port (current-output-port)))
  "Print the documentation for the string or symbol.
Return #t if found anything, #f if no documentation."
  (let* ((head (string-append
                "(" (if (string? func) func (symbol->string func))))
         (len (string-length head))
         (delim (lambda (st) (and (= 1 (string-length st))
                                  (char=? (string-ref st 0)  #\np)))))
    (do ((fl doc-files (cdr fl)) (done #f) (fd #f))
        ((or (null? fl) done)
         (if (not done) (write-all #t "No documentation for `" func "'\n"))
         done)
;;      (display "trying ") (display (car fl)) (display "...\n")
      (set! fd (open-input-file (car fl)))
      (do ((ln (read-line fd) (read-line fd)) (delim-p #f))
          ((or (eof-object? ln) done) (close-input-port fd))
        (cond ((delim ln) (set! delim-p #t))
              ((and delim-p (< len (string-length ln))
                    (string=? head (substring ln 0 len))
                    (string-index " )" (string-ref ln len)))
               (set! done #t)
               (display ln port) (newline port)
               (do ((ln (read-line fd) (read-line fd)))
                   ((delim ln))
                 (display ln port) (newline port))))))))
;; For testing...
;; (documentation "window-position")
;; (documentation "make-cl-constraint")
