;;; $Id$
;;; xprop-extras.scm
;;; Copyright (C) 1999 Greg J. Badros


(define-module (app scwm xprop-extras)
  :use-module (app scwm optargs))

(define-public (set-window-title! win title)
  "Change the window title X-Property of WIN to TITLE.
WIN is a Scwm window object, TITLE is a string.  This procedure alters the
window title by changing the WM_NAME X-Property."
  (X-property-set! win "WM_NAME" title))

(define-public (X-cut-buffer-string)
  "Return the text of the CUT_BUFFER0 property of the root window.
This is the cut text selected by X clients.  Returns #f if the
CUT_BUFFER0 property is not a string."
  (let ((l (X-property-get 'root-window "CUT_BUFFER0")))
    (if (string=? (cadr l) "STRING")
	(car l)
	#f)))

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
