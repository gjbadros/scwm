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
  :use-module (app scwm winlist)
  :use-module (app scwm winops)
  :use-module (app scwm optargs))



(define-public HOME (getenv "HOME"))
(define-public USER (getenv "USER"))
(define-public user-init-file (string-append HOME "/.scwmrc"))

;; The #t arguments should perhaps instead be a closure
;; returning whether an opaque move/resize is desired

(define-public (interactive-move-window-with-focus)
  "Interactively move the window which currently has the focus.
`interactive-move-maybe-opaque' is used to control whether a rubberband
outline or the window itself is moved."
  (let ((w (current-window-with-focus))) (and w (interactive-move-maybe-opaque w))))

(define-public (interactive-resize-window-with-focus)
  "Interactively resize the window which currently has the focus.
`interactive-resize-maybe-opaque' is used to control whether a rubberband
outline or the window itself is resized."
  (let ((w (current-window-with-focus))) (and w (interactive-resize-maybe-opaque w))))

(define-public (interactive-move-window-with-pointer)
  "Interactively move the window which currently contains the pointer.
`interactive-move-maybe-opaque' is used to control whether a rubberband
outline or the window itself is moved."
  (let ((w (current-window-with-pointer))) (and w (interactive-move-maybe-opaque w))))

(define-public (interactive-resize-window-with-pointer)
  "Interactively resize the window which currently contains the pointer.
`interactive-resize-maybe-opaque' is used to control whether a rubberband
outline or the window itself is resized."
  (let ((w (current-window-with-pointer))) (and w (interactive-resize-maybe-opaque w))))

(define-public (toggle-max-vert)
  "Toggle the current window's maximized-vertically state."
  (toggle-maximize 0 (%y 100)))

(define-public (toggle-max-horz)
  "Toggle the current window's maximized-horizontally state."
  (toggle-maximize (%x 100) 0))

(define-public (toggle-max-both)
  "Toggle the current window's maximization (both vertically and horizontally)."
  (toggle-maximize (%x 100) (%y 100)))

(define-public (toggle-max-vert-part)
  "Toggle the current window's maximization-vertically to 95% of the screen height."
  (toggle-maximize 0 (%y 95)))

(define-public (wiggle-window)
  "Animatedly window shade and then unshade the current window.
Just a toy--- perhaps could be useful to call attention to a window."
  (let ((w (get-window))) (window-shade w #t) (un-window-shade w #t)))

(define-public (write-all port . lst)
  "Write all arguments into the port. #t means `current-output-port'."
  (if (eq? port #t) (set! port (current-output-port)))
  (do ((zz lst (cdr zz))) ((null? zz))
    (if (string? (car zz)) (display (car zz) port) (write (car zz) port))))

(define-public (to-string . rest)
  "Dump all arguments into a string."
  (with-output-to-string (lambda () (apply write-all #t rest))))

(define*-public (move-window-to-viewport xx yy #&optional win)
  "Move WIN to the viewport at (XX,YY).
The (0,0) viewport is the starting viewport."
  (let ((pos (window-position win)) (sz (display-size))
        (vp (viewport-position)))
    (move-to (+ (* xx (car sz)) (- (car vp)) (modulo (car pos) (car sz)))
             (+ (* yy (cadr sz)) (- (cadr vp))
                (modulo (cadr pos) (cadr sz))) win)))

(define-public (in-viewport xx yy)
  "Return a function of one argument, a window, moving it to the viewport."
  (lambda (win) (move-window-to-viewport xx yy win)))

(define-public (system-info-string)
  "Return a string with various system information.
Use `show-system-info' to display it in a window."
  (let ((vv (X-version-information)) (dd (X-display-information)))
    (apply
     to-string "Guile verion:\t\t" (version)
     "\nLibguile timestamp:\t" (libguile-config-stamp)
     "\nSCWM version:\t\t" (scwm-version)
     "\nFrom repository date:\t\t" (scwm-version-date)
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
  "Return a menu-object for viewing or editing FILE.
REST is a list of other menu-items to include in the returned menu."
  (menu (append! (list (menuitem "View" #:action (show-file file))
		       (menuitem "Edit" #:action
				 (string-append (or (getenv "EDITOR") "vi")
						" " file)))
		 rest)))

(define-public (quotify-single-quotes str)
  "Return a string that has single quote characters backslashified."
  (regexp-substitute/global #f "'" str 'pre "'\"'\"'" 'post))

;;; FIXGJB: how set width of an xmessage?
(define-public (message . str)
  "Display the string arguments STR in a message window."
  (execute (string-append "echo -e \'"
			  (quotify-single-quotes (apply string-append str))
			   "\'| xmessage -file - -default okay -nearmouse")))

(define-public (show-mesg . str)
  "Return a lambda to display the string arguments STR in a message window.
See also `message'."
  (lambda () (apply message str)))

(define-public (show-file filename)
  "Return a lambda to display the contents of filename in a window."
  (exe (string-append "xmessage -default okay -nearmouse -file " filename)))

(define-public (show-com com)   ; return lambda
  (exe (string-append com "| xmessage -file - -default okay -nearmouse")))

(define-public (bool->str arg) 
  "Return the string \"true\" if ARG is #t, \"false\" otherwise."
  (if arg "true" "false"))

(define*-public (size->str sz #&optional (sep "x"))
  "Convert a two-element list to a string.
Use the optional second argument as the separator."
  (string-append (number->string (car sz)) sep (number->string (cadr sz))))

(define*-public (window-info #&optional (win (get-window)))
  "Display information about WIN in a message window."
  (message
   "Window ID:\t\t" (number->string (window-id win))
   "\nWindow Frame ID:\t" (number->string (window-frame-id win))
   "\nTitle:\t\t\t\"" (window-title win) "\"\nPosition:\t\t"
   (size->str (window-position win)) "\nSize:\t\t\t"
   (size->str (window-frame-size win))
   "\nDesk:\t\t\t" (number->string (window-desk win)) "\nClass:\t\t\t\""
   (window-class win) "\"\nResource:\t\t\"" (window-resource win)
   "\"\nBorder Normal:\t\t" (bool->str (border-normal? win))
   "\nDeletable:\t\t" (bool->str (window-deletable? win))
   "\nIconified:\t\t" (bool->str (iconified? win))
   "\nKept On Top:\t\t" (bool->str (kept-on-top? win))
   "\nTransient:\t\t" (bool->str (transient? win))
   "\nRaised:\t\t\t" (bool->str (raised? win))
   "\nShaded:\t\t\t" (bool->str (window-shaded? win))
   "\nShaped:\t\t\t" (bool->str (window-shaped? win))
   "\nIcon Shaped:\t\t" (bool->str (window-icon-shaped? win))
   "\nSticky Icon:\t\t" (bool->str (icon-sticky? win))
   "\nSticky:\t\t\t" (bool->str (sticky? win))
   "\nTitle Bar Shown:\t" (bool->str (titlebar-shown? win))))

(define-public (show-system-info)
  "Display the `system-info-string' system details in a window."
  (message (system-info-string)))

(define-public (make-menuitems-from-menu-information-list menu-info-list)
  "Return a list of menu-items from a list of detailed programs list.
The format is subject to change.  See sample.scwmrc/gjb.scwmrc for
example usage."
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

(define-public (key-mouse-moves modifiers pct-of-screen left down up right)
  "Bind four keys to move the mouse in compass directions by PCT-OF-SCREEN.
MODIFIERS specifies which modifiers must be depressed for the bindings
to be active.
LEFT, DOWN, UP, and RIGHT are the four keysym names to use for each
of the directions."
  (bind-key 'all (string-append modifiers "-" left)
	    (lambda () (move-pointer (%x (- pct-of-screen)) 0)))
  (bind-key 'all (string-append modifiers "-" down)
	    (lambda () (move-pointer 0 (%y pct-of-screen))))
  (bind-key 'all (string-append modifiers "-" up)
	    (lambda () (move-pointer 0 (%y (- pct-of-screen)))))
  (bind-key 'all (string-append modifiers "-" right)
	    (lambda () (move-pointer (%x pct-of-screen) 0))))

(define-public (key-viewport-moves modifiers pct-of-screen left down up right)
  "Bind four keys to move the viewport in compass directions by PCT-OF-SCREEN.
MODIFIERS specifies which modifiers must be depressed for the bindings
to be active.
LEFT, DOWN, UP, and RIGHT are the four keysym names to use for each
of the directions."
  (bind-key 'all (string-append modifiers "-" left)
	    (lambda () (move-viewport (%x (- pct-of-screen)) 0)))
  (bind-key 'all (string-append modifiers "-" down)
	    (lambda () (move-viewport 0 (%y pct-of-screen))))
  (bind-key 'all (string-append modifiers "-" up)
	    (lambda () (move-viewport 0 (%y (- pct-of-screen)))))
  (bind-key 'all (string-append modifiers "-" right)
	    (lambda () (move-viewport (%x pct-of-screen) 0))))

(define-public (sleep-ms ms)
  "Delay for MS milliseconds. 
Note that timer-hooks are much more useful in nearly all
cases.  See `add-timer-hook!'."
  (select '() '() '() 0 (* 1000 ms)))

(define-public (printable-char->keysym-string char)
  "Return the keysym string corresponding to a printable character.
CHAR is a scheme character.  The return value is appropriate for
use by `send-key-press'.  See also `X-synthetic-send-string'."
  (let ((charval (char->integer char)))
    (cond ((char=? char #\space) "space")
	  ((char=? char #\newline) "Return")
	  ((char=? char #\cr) "Return")
	  ((= charval 27) "Escape")
	  ((< charval 32) (string-append "C-" (make-string 1 (integer->char
							      (+ 64 charval)))))
	  (#t (make-string 1 char)))))

;; (printable-char->keysym-string "")

(define*-public (X-synthetic-send-string str #&optional (win (get-window)))
  "Send string STR to WIN via synthetic X events.
Note that some programs (e.g., xterm) by default do not
honour synthetic key events as they are a security hole."
  (let ((i 0))
    (while (< i (string-length str))
	   (send-key-press
	    (printable-char->keysym-string (string-ref str i)) w)
	   (set! i (+ 1 i)))))

;; from Harvey Stein
(define-public (find-window-by-name window-name)
  "Return a window with name WINDOW-NAME.
If there are multiple such windows, an unspecified one of them
will be returned."
  (let ((wlist (list-windows
		#:only (lambda (w)
			 (string=? (window-title w) window-name)))))
    (if (not (null? wlist))
	(car wlist)
	#f)))

;; Returns them in reverse the order they were selected
;; should probably turn off the invalid interaction hook
;; or provide a way of telling select-window-interactively that
;; the root window is not an erro
(define*-public (select-multiple-windows-interactively #&optional (max 32000))
  "Return a list of user-selected windows, up to MAX.
The list is in the reverse order from the way by which they were selected."
  (do ((w '())
       (wlist '() (cons w wlist))
       (i 0 (+ 1 i)))
      ((or (not w) (>= i max))
       (if w wlist
	   (cdr wlist)))
    (set! w (select-window-interactively (string-append "select #" (number->string i))))))

;; e.g.
;;(select-multiple-windows-interactively 10)
;;(restack-windows (select-multiple-windows-interactively 3))

(define*-public (select-window-from-window-list #&key (only '()) (except '()))
  "Permit selecting a window from a window list.
Return the selected window object, or #f if none was selected"
  (show-window-list-menu #:only only #:except except #:proc (lambda (w) w)))

;; e.g.
;; (let ((w (select-window-from-window-list #:only iconified?)))
;;  (deiconify w) (move-to 0 0 w))

(define-public (color->string color)
  "Convert scwm color object COLOR into an X11 name of that color.
The resulting string can, e.g., be used in command lines for executing
other applications."
  (color-property color 'name))

(define-public (set-window-title! win title)
  "Change the window title X-Property of WIN to TITLE.
WIN is a Scwm window object, TITLE is a string.  This procedure alters the
window title by changing the WM_NAME X-Property."
  (X-property-set! win "WM_NAME" title))

(define-public (get-wm-command win)
  "Get the "WM_COMMAND" X-Property of WIN and return that string.
WIN is a Scwm window object. The "WM_COMMAND" X-Property is the application's
notion of what the command line was used to run the application."
  (let ((prop (X-property-get win "WM_COMMAND")))
    (and (list? prop) (car prop))))

(define-public (sec->usec sec)
  "Convert SEC seconds into an equivalent number of microseconds.
Especially useful for add-hook! and other timing related procedures
that take microseconds."
  (* 1000000 sec))

(define-public (ms->usec ms)
  "Convert MS milliseconds into an equivalent number of microseconds.
Especially useful for add-hook! and other timing related procedures
that take microseconds."
  (* 1000 sec))

;;; FIXGJB: this is an ugly hack, and doesn't work
;;; anyway -- I just want to know the height of the title
;;; bar for WIN
(define-public (window-title-height win)
  (let ((old-decor (current-decor))
	(answer 1))
    (set-current-decor! (window-decor win))
    (set! answer (title-height))
    (set-current-decor! old-decor)
    answer))

;; FIXGJB: a hack for now
(define-public (window-border-width win) 2)

(define-public (popup-menu-from-decoration menu win button-number)
  "Popup MENU from WIN's decoration numbered BUTTON-NUMBER.
This positions the popup menu appropriately."
  (let* ((pos (window-viewport-position win))
	 (x-ne (car pos))
	 (y (+ (cadr pos) (+ 1 (* 2 (window-border-width win)) (window-title-height win))))
	 (x (if (odd? button-number) x-ne (+ 1 x-ne (car (window-frame-size))))))
    (popup-menu menu #f x y (odd? button-number))))


;; We need accessors for window background information,
;; and window-hilight background information
(define*-public (flash-window win #&optional
			      (color (make-color "red"))
			      (unflash-delay .5))
  (set-object-property! win 'old-bg (cadr (get-window-colors win)))
  (set-object-property! win 'old-hi-bg (cadr (get-window-highlight-colors win)))
  (set-window-background! color win)
  (set-window-highlight-background! color win)
  (if (number? unflash-delay)
      (add-timer-hook! (sec->usec unflash-delay) 
		       (lambda () 
			 (unflash-window win)))))


(define*-public (unflash-window #&optional (win (get-window)))
  (let ((old-bg (object-property win 'old-bg))
	(old-hi-bg (object-property win 'old-hi-bg)))
    (set-window-background! old-bg win)
    (set-window-highlight-background! old-hi-bg win)))


(define-public (make-string-usable-for-resource-key string)
  "Return a converted string from STRING that can be used as an X resource key.
The returned string will have all non-alphanumeric characters replaced with
underscores, so that the resulting string can be used as a key for
`X-resource-get' and `X-resource-put'."
  (regexp-substitute/global
   #f "[^a-zA-Z_0-9]" string
   'pre (lambda (match) "_")
   'post))

;;; (make-string-usable-for-resource-key "foo bar baz")
;;; (make-string-usable-for-resource-key "foo*bar.baz")


;; From S.Senda -- Aug 3, 1998
;;;;;;;; rlogin menu making from .rhosts file ;;;;;;;;;

(define (make-rhosts-menu)
  (false-if-exception
   (let* ((rhostfn (string-append HOME "/.rhosts"))
	  (termprog "xterm")
	  (p (open-input-file rhostfn))
	  (ret '())
	  (ap (lambda (a)
		(set! ret (append ret (list a)))))
	  (mm (lambda (h u)
		(menuitem h #:action
			  (lambda () (execute
				      (string-append termprog " -e rlogin "
						     h " -l " u))))))
      )
    (ap (menuitem ".rhosts" #f))
    (ap menu-title)
    (do ((l (read-line p 'trim) (read-line p 'trim)))
	((eof-object? l) ret)
      (cond ((string-match "([^ \t]+)[ \t]+([^ \t]+)" l)
	     => (lambda (m)
		  (ap (mm (match:substring m 1)   ; machine name
			  (match:substring m 2))) ; user name
		  ))))
    (ap menu-title)
    (ap (menuitem "reread .rhosts file" #:action
	    (lambda () (set! rhosts-menu (make-rhosts-menu)))))
    (menu ret)
)))

(define rhosts-menu (make-rhosts-menu))

(define-public (X-cut-buffer-string)
  "Return the text of the CUT_BUFFER0 property of the root window.
This is the cut text selected by X clients."
  (X-property-get 'root-window "CUT_BUFFER0"))


(define*-public (display-message-briefly msg #&optional (sec-timeout 3))
  "Display MSG in the message window for SEC-TIMEOUT seconds.
See `display-message' for details about MSG."
  (display-message msg)
  (add-timer-hook! (sec->usec sec-timeout)
		   (lambda () (hide-message))))

(define-public (close-all-xlogo-windows)
  "Close each window with class == XLogo.
Greg uses XLogo windows as a sample window, so this
is useful for clearing the xlogos away when there get to
be more than desired."
  (for-each (lambda (w) (close-window w)) 
	    (list-windows #:only (lambda (w) (string=? (window-class w) "XLogo")))))

;; useful for debugging/testing
;;(set-X-server-synchronize! #t)
