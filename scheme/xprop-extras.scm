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
