;;; $Id$
;;; flux.scm
;;; Copyright (C) 1998-1999 Sam Steingold, Greg J. Badros, and Maciej Stachowiak
;;;
;;; This are functions used by various sample .scwmrc, but not necessarily
;;; stabilized even as well as the other files in scheme/*.scm
;;; Expect the semantics of these functions to change, and don't
;;; be surprised if some even completely disappear (as we figure out a better
;;; way to do things)



(define-module (app scwm flux)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm time-convert)
  :use-module (app scwm defoption)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm message-window)
  :use-module (app scwm winops)
  :use-module (app scwm flash-window)
  :use-module (app scwm listops)
  :use-module (app scwm file)
  :use-module (app scwm stringops)
  :use-module (app scwm group)
  :use-module (app scwm path-cache)
  :use-module (app scwm window-selection)
  :use-module (app scwm optargs))



(define-public user-init-file (string-append (user-home) "/.scwmrc"))

;; The #t arguments should perhaps instead be a closure
;; returning whether an opaque move/resize is desired

(define-public (interactive-move-window-with-focus)
  "Interactively move the window which currently has the focus.
`move-opaquely?' is used to control whether a rubberband
outline or the window itself is moved."
  (let ((w (current-window-with-focus))) (and w (interactive-move w))))

(define-public (interactive-resize-window-with-focus)
  "Interactively resize the window which currently has the focus.
`resize-opaquely?' is used to control whether a rubberband
outline or the window itself is resized."
  (let ((w (current-window-with-focus))) (and w (interactive-resize w))))

(define-public (interactive-move-window-with-pointer)
  "Interactively move the window which currently contains the pointer.
`move-opaquely?' is used to control whether a rubberband
outline or the window itself is moved."
  (let ((w (current-window-with-pointer))) (and w (interactive-move w))))

(define-public (interactive-resize-window-with-pointer)
  "Interactively resize the window which currently contains the pointer.
`resize-opaquely?' is used to control whether a rubberband
outline or the window itself is resized."
  (let ((w (current-window-with-pointer))) (and w (interactive-resize w))))

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

(define-public (system-info-string)
  "Return a string with various system information.
Use `show-system-info' to display it in a window."
  (let ((vv (X-version-information)) (dd (X-display-information)))
    (apply
     to-string "Guile verion:\t\t" (version)
     "\nLibguile timestamp:\t" (libguile-config-stamp)
     "\nSCWM version:\t\t" (scwm-version)
     "\nFrom repository date:\t" (scwm-version-date)
     "\nRestarted:\t\t" (bool->string (restarted?))
     "\nDisplay Size:\t\t" (size->string (display-size))
     "\nDesk Size:\t\t" (size->string (desk-size))
     "\nViewport Position:\t" (size->string (viewport-position))
     "\nPointer:\t\t" (size->string (pointer-position))
     "\nCurrent Desk:\t\t" (number->string (current-desk))
     "\nX vendor:\t\t" (caddr vv) "; version: " (number->string (car vv)) "."
     (number->string (cadr vv)) "; release: " (number->string (cadddr vv))
     "\nX Display:\n\tResolution:\t" (size->string dd) "\n\tColor:\t\t"
     (list-ref dd 4) " (depth: " (number->string (caddr dd))
     "; bits per RGB: " (number->string (cadddr dd)) ")\nimage-load-path:\n"
     (map (lambda (st) (string-append "\t" st "\n")) image-load-path))))

;; CRW:FIXME:: This should be merged with make-context-menu in
;; std-menus.scm

;; I (CRW) changed "vi" to "emacs" below.  If anybody feels strongly
;; that the default should be "vi", at least make it "xterm -e vi"
;; instead of just "vi".
(define-public (make-file-menu file . rest)
  "Return a menu-object for viewing or editing FILE.
REST is a list of other menu-items to include in the returned menu."
  (menu (append! (list (menuitem "View" #:action (show-file file))
		       (menuitem "Edit" #:action
				 (string-append (or (getenv "EDITOR") "emacs")
						" " file)))
		 rest)))

(define-public (quotify-single-quotes str)
  "Return a string that has single quote characters backslashified."
  (regexp-substitute/global #f "'" str 'pre "'\"'\"'" 'post))

;;; GJB:FIXME:: do not use xmessage-- use guile-gtk
(define-public (message . str)
  "Display the string arguments STR in a message window.
Requires the program `xmessage'."
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

(define-public (show-com com)
  "Return a lambda to show the stdout generated by the COM shell pipeline."
  (exe (string-append com "| xmessage -file - -default okay -nearmouse")))

(define*-public (window-info #&optional (win (get-window)))
  "Display information about WIN in a message window."
  (message
   "Window ID:\t\t" (number->string (window-id win))
   "\nWindow Frame ID:\t" (number->string (window-frame-id win))
   "\nTitle:\t\t\t\"" (window-title win) "\""
   "\nVirtual Position:\t\t" (size->string (window-position win))
   "\nViewport Position:\t\t" (size->string (window-viewport-position win))
   "\nSize:\t\t\t" (size->string (window-frame-size win))
   "\nDesk:\t\t\t" (number->string (window-desk win)) "\nClass:\t\t\t\""
   (window-class win) "\"\nResource:\t\t\"" (window-resource win)
   "\"\nBorder Normal:\t\t" (bool->string (border-normal? win))
   "\nFocus:\t\t\t" (get-window-focus win)
   "\nDeletable:\t\t" (bool->string (window-deletable? win))
   "\nIconified:\t\t" (bool->string (iconified? win))
   "\nKept On Top:\t\t" (bool->string (kept-on-top? win))
   "\nTransient:\t\t" (bool->string (transient? win))
   "\nRaised:\t\t\t" (bool->string (raised? win))
   "\nShaded:\t\t\t" (bool->string (window-shaded? win))
   "\nShaped:\t\t\t" (bool->string (window-shaped? win))
   "\nIcon Shaped:\t\t" (bool->string (window-icon-shaped? win))
   "\nSticky Icon:\t\t" (bool->string (icon-sticky? win))
   "\nSticky:\t\t\t" (bool->string (sticky? win))
   "\nTitle Bar Shown:\t" (bool->string (titlebar-shown? win))))

(define-public (show-system-info)
  "Display the `system-info-string' system details in a window."
  (message (system-info-string)))

(define (first-word s)
  "Return the first word of S (up to but not including first space char)."
  (let ((i (string-index s #\space))
	(j (string-index s #\tab))
	(l (string-length s)))
    (let ((k (if (< (or i j l) (or j i l)) i j)))
      (if k (substring s 0 k) s))))
;;(first-word "foo bar") => "foo"
;;(first-word "foo	bar") => "foo"
;;(first-word "foobar") => "foo"

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
		       (if (cached-program-exists? (first-word exename))
			   (menuitem
                            title #:action exename #:image-left
                            (if mini-icon
                                (string-append "mini-" mini-icon ".xpm") #f)
                            ;; #:icon (if icon (string-append icon ".xpm") #f)
			    )
			   #f)))
		   menu-info-list))))


(define-public (sleep-ms ms)
  "Delay for MS milliseconds.
Note that timer-hooks are much more useful in nearly all
cases.  See `add-timer-hook!'."
  (select '() '() '() 0 (* 1000 ms)))


(define-public (select-window-group)
  "Prompt for multiple windows and return the list of selected windows.
Windows are highlighted (see `flash-window') as they are selected.  The
returned list can be used to un-highlight the windows:
 (let ((winlist (select-window-group)))
   (for-each (lambda (w) (unflash-window w)) winlist))"
  (do ((w #f)
       (wlist '())
       (cwin-selected 0)
       (w #f)
       (done #f))
      (done
       wlist)
    (set! w (select-window-interactively
	     (string-append "select #" (number->string cwin-selected))))
    (if w
	(if (memq w wlist)
	    (begin
	      ;; remove w from wlist
	      (set! wlist (list-without-elem wlist w))
	      (unflash-window w)
	      (set! cwin-selected (- cwin-selected 1)))
	    (begin
	      (set! wlist (cons w wlist))
	      (flash-window w #:unflash-delay #f)
	      (set! cwin-selected (+ cwin-selected 1))))
	(set! done #t))))
;; (define wg (select-window-group))
;; (object-properties (select-window-interactively))
;; (for-each (lambda (w) (unflash-window w)) (list-all-windows))
;; (unflash-window)
;; (flash-window-on)



;; From S.Senda -- Aug 3, 1998
;;;;;;;; rlogin menu making from .rhosts file ;;;;;;;;;

(define-public (make-rhosts-menu)
  "Returns a menu which lets you rlogin to each host mentioned in your .rhosts"
  (false-if-exception
   (let* ((rhostfn (string-append (user-home) "/.rhosts"))
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

;; sds: users should call this function themselves
;;(define-public rhosts-menu (make-rhosts-menu))

(define-public (close-all-xlogo-windows)
  "Close each window with class == XLogo.
Greg uses XLogo windows as a sample window, so this
is useful for clearing the xlogos away when there get to
be more than desired."
  (for-each (lambda (w) (close-window w))
	    (list-windows #:only
			  (lambda (w)
			    (string=? (window-class w) "XLogo")))))


;; useful for debugging/testing
;;(set-X-server-synchronize! #t)




;; (run-in-netscape "openUrl(http://vicarious-existence.mit.edu/scwm)" display-message-briefly)
;; (run-in-netscape "openUrl(http://www.cs.washington.edu/homes/gjb)" display-message-briefly)
;; (netscape-goto-url "http://vicarious-existence.mit.edu/scwm")

;; Inspired by Julian Satchell's version of this --10/09/98 gjb
(define-public (use-change-desk-commands vector-of-commands)
  "Execute one of the VECTOR-OF-COMMANDS shell commands when the desk changes.
The 0th element of the vector is used for changes to desk 0,
the first element for changes to desk 1, etc.  Changes to desks which are
\"off the end\" of the vector do nothing."
  (add-hook! change-desk-hook
	     (lambda (new old)
	       ;; (display n) (newline) ;; for debugging
	       (if (< new (vector-length vector-of-commands))
		   (system (vector-ref vector-of-commands new)))
	       )))

(define (extreme1 pred lst)
  (if (null? (cdr lst))
      (car lst)
      (let ((ex (extreme pred (cdr lst))))
	(if (pred (car lst) ex)
	    (car lst)
	    ex))))

(define-public (extreme pred lst)
  "Find extreme value e of PRED in LST.
If PRED defines a semi-ordering, `(PRED e x)' will hold for all members x
of LST not equal to e. E.g. `(extreme < ...)' returns the lowest number."
  (if (null? lst)
      ()
      (extreme1 pred lst)))

(define*-public (take-screenshot
		 #&optional (template (string-append
				       (user-home)
				       "/screenshot%y%m%d%H%M%S.xwd")))
  "Take a snapshot of the whole screen.
The screenshot will be saved in xwd format in the filename constructed from
TEMPLATE. %-escapes in TEMPLATE will be replaced by time-elements, according
to strftime rules. TEMPLATE defaults to the file \"screenshot%y%m%d%H%M%S.xwd\"
in the user's home directory."
  (execute (string-append "xwd -root >"
			  (strftime template (localtime (current-time))))))


;;; palm pilot stuff
;;; requires pilot-link's pilot-clip program

;; (system "ssh-add </dev/null &")
;;(define pilot-clip-binary "pilot-clip")
(define pilot-clip-binary "remote-pilot-clip")  ;; this does ssh HOST-WITH-CRADLE pilot-clip "$@"

(define-public (put-string-in-palm-clipboard str)
  (let ((port (open-output-pipe (string-append pilot-clip-binary " -s &"))))
    (display str port)
    (close-port port)))

(define-public (X-cut-buffer->palm-clipboard)
  (put-string-in-palm-clipboard (X-cut-buffer-string)))

;;(put-string-in-palm-clipboard "testing\nto\nsee\nif this\nworks")
;; (X-cut-buffer->palm-clipboard)

;; This is not such a hot idea-- scwm can hang!
;; (define-public (get-string-from-palm-clipboard)
;;   (let* ((port (open-input-pipe (string-append pilot-clip-binary " -g")))
;; 	    (str (read-line port)))
;;     (close-port port)
;;     str))
;;
;; (get-string-from-palm-clipboard)

(define-public (delete-multiple-windows-interactively)
  (select-multiple-windows-interactively #f delete-window))

;; ((help-mesg "move-to"))

(define-public (move-all-windows-relative x y)
  "Move all windows right X, down Y pixels.
See `move-window-relative.'"
  (for-each (lambda (w) (move-window-relative x y w)) (list-all-windows)))

;; Recapture sometimes requires me to use this -- WHY? GJB:FIXME::
;; (use-modules (app scwm flux))
;; (move-all-windows-relative 0 display-height)
;; (move-all-windows-relative display-width 0)

(defmacro-public @ args
  `(lambda (sym)
     (variable-ref (module-variable (resolve-module ',args) sym))))

; define the WM_HINTS flags WH-*
(do ((n 1 (* 2 n))
     (sym '(WH-Input WH-State WH-IconPixmap WH-IconWindow
	    WH-IconPosition WH-IconMask WH-WindowGroup WH-reserved WH-Urgency)
	  (cdr sym)))
    ((null? sym))
  (eval `(define ,(car sym) ,n)))

; define the WM_SIZE_HINTS flags WSH-*
(do ((n 1 (* 2 n))
     (sym '(WSH-USPosition WSH-USSize WSH-PPosition WSH-PSize
	    WSH-PMinSize WSH-PMaxSize WSH-PResizeInc WSH-PAspect
	    WSH-PBaseSize WSH-PWinGravity)
	  (cdr sym)))
    ((null? sym))
  (eval `(define ,(car sym) ,n)))

(define*-public (X-properties->string #&optional (win (get-window))
				      (recurse #t))
  "Prints the X properties of WIN into a string.
WIN is a window object, an X window id, or 'root-window.
If RECURSE is non-#f, also list properties of referenced windows."
  (let ((win (if (number? win) win (window-id win)))
	(todo ()))
    (apply
     string-append
     (append!
      (map (lambda (prop)
		(let* ((res (X-property-get win prop))
		       (val (car res))
		       (type (cadr res)))
		  (string-append
		   prop "(" type ") = "
		   (cond
		    ((equal? prop "WM_CLIENT_LEADER")
		     (let ((leader (array-ref val 0)))
		       (if (not (or (equal? leader win) (assoc leader todo)))
			   (set! todo (append! todo
					       (cons (cons leader
							   "client leader")
						     ()))))
		       (string-append "# 0x" (number->hex-string leader))))
		    ((equal? type "STRING")
		     (string-join
		      ", "
		      (map
		       (lambda (s)
			 (if (not (equal? s ""))
			     (string-append "\"" s "\"")
			     ""))
		       (separate-fields-discarding-char #\null val list))))
		    ((equal? type "ATOM")
		     (string-join
		      ", "
		      (map (lambda (atom) (or (X-atom->string atom) "<undef>"))
			   (array->list val))))
		    ((member type
			     '("WINDOW" "VISUALID" "WM_COLORMAP_WINDOWS"
					"FONT" "BITMAP" "PIXMAP" "DRAWABLE"
					"CURSOR" "COLORMAP"))
		     (string-join ", "
				  (map
				   (lambda (id)
				     (string-append "# 0x"
						    (number->string id 16)))
				   (array->list val))))
		    ((equal? type "WM_ICON_SIZE")
		     (apply string-append
			    (map (lambda (name value)
				   (string-append name
						  (number->string value)))
				 '("\n\t\tminimum icon size: " " by "
				   "\n\t\tmaximum icon size: " " by "
				   "\n\t\tincremental size change: " " by ")
				 (array->list val))))
		    ((equal? type "WM_STATE")
		     (string-append "\n\t\tWindow state: "
				    (window-state->string (array-ref val 0))
				    "\n\t\tIcon window: # 0x"
				    (number->string (array-ref val 1) 16)))
		    ((equal? type "WM_HINTS")
		     (let ((leader (if (positive? (logand WH-WindowGroup
							   (array-ref val 0)))
				       (array-ref val 8)
				       #f)))
		       (if (and leader (not (equal? leader win))
				(not (assoc leader todo)))
			   (set! todo (append! todo (cons (cons leader
								"group leader")
							  ())))))
		     (apply
		      string-append
		      (map
		       (lambda (name flag converter value)
			 (if (not (zero? (logand flag (array-ref val 0))))
			     (string-append name (converter value))
			     ""))
		       '("\n\t\tClient accepts input or input focus: "
			 "\n\t\tInitial state: "
			 "\n\t\tIcon pixmap: 0x"
			 "\n\t\tIcon window: # 0x"
			 "\n\t\tIcon location: " ", "
			 "\n\t\tIcon mask: 0x"
			 "\n\t\tGroup leader window: # 0x")
		       (list WH-Input
			     WH-State
			     WH-IconPixmap
			     WH-IconWindow
			     WH-IconPosition WH-IconPosition
			     WH-IconMask
			     WH-WindowGroup)
		       (list (lambda (v) (if (zero? v) "False" "True"))
			     window-state->string
			     number->hex-string
			     number->hex-string
			     number->string number->string
			     number->hex-string
			     number->hex-string)
		       ; throw away flags
		       (cdr (array->list val)))))
		    ((equal? type "WM_SIZE_HINTS")
		     (apply
		      string-append
		      (map
		       (lambda (name flag converter value)
			 (if (not (zero? (logand flag (array-ref val 0))))
			     (string-append name (converter value))
			     ""))
		       '("\n\t\tuser specified location: " ", "
			 "\n\t\tuser specified size: " " by "
			 "\n\t\tprogram specified location: " ", "
			 "\n\t\tprogram specified size: " " by "
			 "\n\t\tprogram specified minimum size: " " by "
			 "\n\t\tprogram specified maximum size: " " by "
			 "\n\t\tprogram specified resize increment: " " by "
			 "\n\t\tprogram specified minimum aspect ratio: " "/"
			 "\n\t\tprogram specified maximum aspect ratio: " "/"
			 "\n\t\tmax_aspect: " " by "
			 "\n\t\tprogram specified base size: " " by "
			 "\n\t\twindow gravity: ")
		       (list WSH-USPosition WSH-USPosition
			     WSH-USSize WSH-USSize
			     WSH-PPosition WSH-PPosition
			     WSH-PSize WSH-PSize
			     WSH-PMinSize WSH-PMinSize
			     WSH-PMaxSize WSH-PMaxSize
			     WSH-PResizeInc WSH-PResizeInc
			     WSH-PAspect WSH-PAspect
			     WSH-PAspect WSH-PAspect
			     WSH-PBaseSize WSH-PBaseSize
			     WSH-PWinGravity)
		       (list number->string number->string
			     number->string number->string
			     number->string number->string
			     number->string number->string
			     number->string number->string
			     number->string number->string
			     number->string number->string
			     number->string number->string
			     number->string number->string
			     number->string number->string
			     gravity->string)
		       ; throw away flags and duplicate x, y, w, h
		       (let ((vals (cdr (array->list val))))
			 (append! (list-head vals 4) vals)))))
		    ((equal? type "RGB_COLOR_MAP")
		     (apply
		      string-append
		      (map (lambda (name converter value)
			     (string-append name (converter value)))
			   '("\n\t\tcolormap id: # 0x"
			     "\n\t\tred-max: "
			     "\n\t\tred-mult: "
			     "\n\t\tgreen-max: "
			     "\n\t\tgreen-mult: "
			     "\n\t\tblue-max: "
			     "\n\t\tblue-mult: "
			     "\n\t\tbase-pixel: "
			     "\n\t\tvisual id: # 0x"
			     "\n\t\tkill id: # 0x")
			   (repeat 1 number->hex-string
				   7 number->string
				   2 number->hex-string)
			   (array->list val))))
		    ((equal? type "RECTANGLE")
		     (apply
		      string-append
		      (map (lambda (name converter value)
			     (string-append name (converter value)))
			   '("\n\t\tupper left corner: " ", "
			     "\n\t\tsize: " " by ")
			   (repeat 4 number->string)
			   (array->list val))))
		    ((equal? type "POINT")
		     (string-append (number->string (array-ref val 0))
				    ", "
				    (number->string (array-ref val 1))))
		    ((equal? type "ARC")
		     (apply
		      string-append
		      (map (lambda (name converter value)
			     (string-append name (converter value)))
			   '("\n\t\tarc at " ", "
			     "\n\t\tsize: " " by "
			     "\n\t\tfrom angle " " to angle ")
			   (repeat 6 number->string)
			   (array->list val))))
		    (else ; INTEGER, CARDINAL also handeled here
		     (string-join ", "
				  (map number->string (array->list val)))))
		   "\n")))
	      (X-properties win))
      (if recurse
	  (map (lambda (w)
		 (string-append "\n--- " (cdr w) " ---\n"
				(X-properties->string (car w) #f)))
	       todo)
	  ())))))

(define*-public (show-X-properties #&optional (win (get-window)))
  "Displays the X properties of WIN in a message window.
WIN is a window object, an X window id, or 'root-window."
  (message (X-properties->string win)))
