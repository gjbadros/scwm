;;; $Id$
;;; Copyright (C) 1998-1999 Greg J. Badros, Todd Larason
;;;
;;; Various procedures for managing netscape windows
;;;


(define-module (app scwm netscape)
  :use-module (ice-9 regex)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm xprop-extras)
  :use-module (app scwm message-window)
  :use-module (app scwm winops)
  :use-module (app scwm optargs))


(define-scwm-option *netscape-new-window* #f
  "If #t, `netscape-goto-cut-buffer-url' will open the URL in a new window."
  #:type 'boolean
  #:group 'system)

;; Get a visible netscape window, or any netscape window
;; if none are visible
(define-public (netscape-win)
  "Return a netscape window, prefer a visible netscape window.
May error if no netscape windows are present."
  (let* ((ns-wins
	 (list-windows #:only (win-and?? (class-match?? "Netscape") 
					 (resource-match?? "Navigator"))))
	 (win (if (pair? ns-wins) 
		  (car ns-wins) 
		  #f)))
    (for-each (lambda (w) (if (visible? w) (set! win w))) ns-wins)
    win))


;; from Todd Larason
(define*-public (run-in-netscape command completion #&optional (netwin (netscape-win)))
  "Runs COMMAND in a Netscape window, calling COMPLETION when done, if set.
Uses Netscape window NETWIN if specifies, or one found by (netscape-win)
otherwise; it is an error if NETWIN refers to a non-Netscape window."
  (if netwin
      (letrec
	  ((get-mozilla-hook
	    (lambda ()
	      (if (X-atomic-property-set-if-unset! netwin "_MOZILLA_LOCK" "lock!")
		  (begin
		    (add-hook! X-PropertyNotify-hook mozilla-property-notify)
		    (put-mozilla-command))
		  (add-timer-hook! 50 get-mozilla-hook))))

	   (put-mozilla-command
	    (lambda ()
	      (X-property-set! netwin "_MOZILLA_COMMAND" command)))
	   
	   (mozilla-property-notify
	    (lambda (propname window)
	      (cond ((and (eq? window netwin)
			  (string=? propname "_MOZILLA_RESPONSE"))
		     (remove-hook! X-PropertyNotify-hook mozilla-property-notify)
		     (X-property-get netwin "_MOZILLA_LOCK" #t)
		     (if completion
			 (completion (car (X-property-get
					   netwin "_MOZILLA_RESPONSE" #t)))))))))
	(get-mozilla-hook)
	#t)
      (error "No netscape window.")))

(define-public (uri-escapify-comma uri)
  "Replace commas in URI with the %2C escape code.
This is useful since netscape's remote command invocation does a stupid
syntactic scan of the passed url and treats commas as argument command
separators, so the url gets chopped off at the first literal comma."
  (regexp-substitute/global #f "," uri 'pre (lambda (match) "%2C") 'post))
;; (use-modules (ice-9 regex))
;; (uri-escapify-comma "foo,bar")

(define*-public (netscape-goto-url url 
                #&optional
		(completion #f)
		(new *netscape-new-window*))
  "Make netscape go to the location URL.
Calls COMPLETION when done.
The optional argument specifies whether a new window should be opened.
It defaults to `*netscape-new-window*'."
  (run-in-netscape
   (string-append "openURL(" (uri-escapify-comma url) (if new ",new-window)" ")"))
   completion))

(define*-public (netscape-goto-cut-buffer-url 
		 #&optional (new *netscape-new-window*))
  "Goto the url that is held in the X11 cut buffer.
See `X-cut-buffer' and `netscape-goto-url'.  NEW can be #f to
not open a new netscape frame."
  (netscape-goto-url (X-cut-buffer-string) display-message-briefly new))
