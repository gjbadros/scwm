;;; $Id$
;;; xprop-extras.scm
;;; Copyright (C) 1999 Greg J. Badros


;; (use-modules (app scwm xprop-extras))
(define-module (app scwm xprop-extras)
  :use-module (app scwm optargs)
  :use-module (app scwm stringops)
  :use-module (app scwm listops)
  :use-module (ice-9 string-fun))

(define-public (set-window-title! win title)
  "Change the window title X-Property of WIN to TITLE.
WIN is a Scwm window object, TITLE is a string.  This procedure alters the
window title by changing the WM_NAME X-Property."
  (X-property-set! win "WM_NAME" title))

(define-public (X-cut-buffer-string)
  "Return the text of the primary cut buffer.
This is the cut text selected by X clients.  Returns #f if the
cut buffer is empty or is not a string."
  (X-fetch-bytes))

(define-public (X-set-cut-buffer-string! string)
  "Set the text of the primary cut buffer.
This is the cut text selected by X clients."
  (X-store-bytes string))

(define*-public (copy-window-title-to-cut-buffer #&optional (window (get-window)))
  "Set X cut buffer to be a string that is the title of WINDOW."
  (X-set-cut-buffer-string! (window-title window)))

(define*-public (paste-window-title-from-cut-buffer #&optional (window (get-window)))
  "Set the window title of WINDOW to be the string in the X cut buffer.
Do nothing if the cut buffer does not contain a string."
  (let ((t (X-cut-buffer-string)))
    (if t (set-window-title! window t))))

(define*-public (X-handle-selection-string selection value-handler)
  "Run VALUE-HANDLER on the selection SELECTION after retrieving it.
The VALUE-HANDLER should take a single argument, the string value
of the selection retrieved.  The X11 protocol for selections is
asynchronous, so so must this procedure be."
  (let ((atom-name (string-append "RW_SELECT_" 
				  (if (string? selection) selection (X-atom->string selection)))))
    (define (X-handle-root-property-selection prop-atom deleted?)
      (if (eqv? prop-atom (string->X-atom atom-name))
	  (let* ((val (X-property-get 'root-window prop-atom))
		 (str (and (pair? val) (car val))))
	    (remove-hook! X-root-PropertyNotify-hook X-handle-root-property-selection)
	    (remove-hook! X-SelectionNotify-hook X-handle-no-selection)
	    ;; (display "got value = ") (write val) (newline)
	    (X-property-delete! 'root-window prop-atom)
	    (value-handler str))))
    (define (X-handle-no-selection)
      (remove-hook! X-root-PropertyNotify-hook X-handle-root-property-selection)
      (remove-hook! X-SelectionNotify-hook X-handle-no-selection)
      (value-handler #f))
    (add-hook! X-root-PropertyNotify-hook X-handle-root-property-selection)
    (add-hook! X-SelectionNotify-hook X-handle-no-selection)
    (X-convert-selection selection "STRING" atom-name 'root-window)))

;; (X-handle-selection-string "PRIMARY" (lambda (value) (display value) (newline)))
;; (define-public (debug-property-notify prop win) (display prop) (display " for ") (write win) (newline))

(define-public (get-wm-command win)
  "Get the \"WM_COMMAND\" X-Property of WIN and return that string.
WIN is a Scwm window object. The \"WM_COMMAND\" X-Property is the application's
notion of what the command line was used to run the application."
  (let ((prop (X-property-get win "WM_COMMAND")))
    (and (list? prop) (car prop))))

(define*-public (X-atomic-property-set-if-unset! window name value #&optional
						(type "STRING") (format 8))
  "Set property NAME on WINDOW to VALUE, if it's currently unset.
Returns #f if the property is already set, #t otherwise.
TYPE and FORMAT are as in X-property-set!"
  (with-grabbed-server
   (lambda ()
     (cond ((X-property-get window name) #f)
	   (#t (X-property-set! window name value type format 'replace) #t)))))


;;SCWM_OTHER_ID=foo LD_PRELOAD=/usr/contrib/bin/scwm_set_pid_property.so  xlogo
;(use-scwm-modules xprop-extras)
;(window-pid (get-window))
;(window-other-id (get-window))

(define-public (window-pid win)
  "Returns the process id of the process that created WIN.
Requires using the LD_PRELOAD environment variable for the
started process:

LD_PRELOAD=/path/to/scwm_set_pid_property.so

Returns #f if the property does not exist on WIN (most
likely because you did not use the LD_PRELOAD variable).
See also `window-client-machine-name' to get the machine
name on which the returned process id is valid, and
`window-other-id' to get the string in environment variable
SCWM_OTHER_ID.
"
  (let ((prop (X-property-get win "SCWM_RUNNING_PID")))
    (and (list? prop) (vector-ref (car prop) 0))))


(define-public (window-other-id win)
  "Returns the other id string given to the process that created WIN.
Requires using the LD_PRELOAD environment variable for the
started process:

SCWM_OTHER_ID=\"answer\" LD_PRELOAD=/path/to/scwm_set_pid_property.so

Returns #f if the property does not exist on WIN (most
likely because you did not use the LD_PRELOAD variable and
the SCWM_OTHER_ID environment variable).
See also `window-client-machine-name' to get the machine
name, and `window-pid' to get the process id.
"
  (let ((prop (X-property-get win "SCWM_OTHER_ID")))
    (and (list? prop) (car prop))))

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

; define the _MOTIF_WM_HINTS flags MWH-*
(define MWH-FUNCTIONS 1)
(define MWH-DECORATIONS 2)

; define the Motif WM function flags MWF-*
(do ((n 1 (* 2 n))
     (sym '(MWF-ALL MWF-RESIZE MWF-MOVE MWF-MINIMIZE MWF-MAXIMIZE MWF-CLOSE)
	  (cdr sym)))
    ((null? sym))
  (eval `(define ,(car sym) ,n)))

(define-public (mwm-functions->string func)
  "Converts the MWM function flags into a readable form."
  (if (zero? func)
      "NONE"
      (string-join ", "
		   (map (lambda (flag)
			  (if (not (zero? (logand func
						  (eval flag))))
			      (substring (symbol->string flag) 4)
			      ""))
			'(MWF-ALL MWF-RESIZE MWF-MOVE
				  MWF-MINIMIZE MWF-MAXIMIZE MWF-CLOSE)))))

; define the Motif WM decoration flags MWD-*
(do ((n 1 (* 2 n))
     (sym '(MWD-ALL MWD-BORDER MWD-RESIZEH MWD-TITLE MWD-MENU
		    MWD-MINIMIZE MWD-MAXIMIZE)
	  (cdr sym)))
    ((null? sym))
  (eval `(define ,(car sym) ,n)))

(define-public (mwm-decorations->string decor)
  "Converts the MWM decoration flags into a readable form."
  (if (zero? decor)
      "NONE"
      (string-join ", "
		   (map (lambda (flag)
			  (if (not (zero? (logand decor
						  (eval flag))))
			      (substring (symbol->string flag) 4)
			      ""))
			'(MWD-ALL MWD-BORDER MWD-RESIZEH MWD-TITLE MWD-MENU
				  MWD-MINIMIZE MWD-MAXIMIZE)))))

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
			 (if (not (string-null? s))
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
		    ((member type '("INTEGER" "CARDINAL"))
		     (string-join ", "
				      (map number->string
					   (array->list val))))
		    ((equal? type "_MOTIF_WM_HINTS")
		     (apply
		      string-append
		      (map
		       (lambda (name flag converter value)
			 (if (not (zero? (logand flag (array-ref val 0))))
			     (string-append name (converter value))
			     ""))
		       '("\n\t\tmwm functions: "
			 "\n\t\tmwm decorations: ")
		       (list MWH-FUNCTIONS MWH-DECORATIONS)
		       (list mwm-functions->string mwm-decorations->string)
		       (array->list val))))
		    (else
		     (string-append "0x"
				    (string-join ", 0x"
						 (map
						  (if (= (caddr res) 8)
						      (lambda (v)
							(number->hex-string
							 (char->integer v)))
						      number->hex-string)
						  (array->list val))))))
		   "\n")))
	      (X-properties win))
      (if recurse
	  (map (lambda (w)
		 (string-append "\n--- " (cdr w) " ---\n"
				(X-properties->string (car w) #f)))
	       todo)
	  ())))))
