;;;; $Id$
;;;; Copyright (C) 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 



(define-module (app scwm fvwm-module)
  :use-module (app scwm winlist)
  :use-module (app scwm wininfo)
  :use-module (app scwm base)
  :use-module (app scwm file)
  :use-module (app scwm defoption)
  :use-module (app scwm module-types)
  :use-module (app scwm bincomm)
  :use-module (app scwm fvwm-eval)
  :use-module (app scwm optargs)
  :use-module (app scwm decor)
  :use-module (app scwm file)
  :use-module (app scwm listops))



(define-scwm-group fvwm2-module "Fvwm2 Modules")

;;   fvwm-module
;; Supports running fvwm2 modules from scwm - this is mostly useful for
;; the pager.
;;
;; Public interface:
;;
;;   variable: fvwm2-module-path 
;; This list of strings contains is used as the list of directories to
;; search when running fvwm2 modules.
;;
;;   register-fvwm2-module-config MODULE-TYPE . ARGS
;; Set the configuration for fvwm modules named MODULE-TYPE to be the
;; list of strings ARGS by default.
;;
;;   append-fvwm2-module-config MODULE-TYPE . ARGS
;; Append the strings ARGS to the current default configuration for
;; modules of type MODULE-TYPE
;;
;;   clear-fvwm2-module-config MODULE-TYPE
;; Clear the default configuration for modules of type MODULE-TYPE 
;;
;;   get-fvwm2-module-config MODULE-TYPE
;; Retrieve the current default configuration for modules of type 
;; MODULE-TYPE 
;;
;;   run-fvwm2-module MODULE-NAME #:optional OTHER-ARGS CONFIG-INFO
;;   CONFIG-FILE
;; Run the module MODULE-NAME, searching the fvwm2-module-path to find
;; it, unless MODULE-NAME starts with "/", "./" or "../", in which
;; case it is taken as an absolute filename. OTHER-ARGS specifies the
;; extra arguments to pass to the module. For example, the FvwmPager
;; module requires two arguments specifying the first and last desk
;; the pager should manage. All module arguments should be given as
;; strings. The CONFIG-FILE and CONFIG-INFO arguments should not be
;; needed in most cases. CONFIG-FILE defaults to "~/.fvwm2rc". This is
;; right in most cases, and further, most modules totally ignore this
;; argument. CONFIG-INFO is a list of configuration parameter strings.
;; It defaults to the registered configuration for this module type,
;; so in most cases it will be more convenient not to use it. However,
;; in some cases it may be desirable to launch multiple instances of
;; an fvwm module with different configurations; this argument makes
;; it easy to do so. An fvwm2-module object is returned.
;;
;;   kill-fvwm2-module FMOD
;; Kill the module represented by the fvwm2-module object FMOD.
;;
;;   kill-all-fvwm2-modules
;; Kill all running fvwm2 modules.
;;
;;   kill-fvwm2-modules-by-name MODULE-NAME
;; Kill all fvwm2 modules of type MODULE-NAME
;;
;;
;; Here is an example of using the FvwmPager in a scwmrc:
;;
;; ;; Make sure the right place is in the module path. Note: the
;; ;; directory listed here is for example purposes only, it is actually
;; ;; in the default module path.
;; (set! *fvwm2-module-path* (append *fvwm2-module-path* '("/usr/lib/X11/fvwm2")))
;; ;; Register the module configuration for the pager.
;; (register-fvwm2-module-config "FvwmPager"
;;  			         "*FvwmPagerBack grey76"
;;			         "*FvwmPagerFore black"
;;			         "*FvwmPagerHilight navyblue"
;;			         "*FvwmPagerFont none"
;;			         "*FvwmPagerDeskTopScale 40"
;;			         "*FvwmPagerLabel 0 Top"
;;			         "*FvwmPagerLabel 1 Bottom"
;;			         "*FvwmPagerSmallFont 5x8")
;;
;; ;; Actually run the pager. Make it manage desktops 0-2. Save the
;; ;; module object in a variable so we can kill this specific pager
;; ;; later.
;; (define fvwm2-pager (run-fvwm2-module "/path/to/FvwmPager" '("0" "2")))
;;
;; ;; To later shut down the module, use kill-fvwm2-module:
;;   (kill-fvwm2-module fvwm2-pager)
;;
;; ;; To kill all "FvwmPager" modules:
;;   (kill-fvwm2-modules-by-name "FvwmPager")
;;
;; This support is in progress. It may or may not work with particular
;; modules, depending on whether all the commands they use are
;; supported by the fvwm-eval module (q.v.).
;;
;;


(define-scwm-option *debug-fvwm-module* #f
  "Set this if you want debugging output from fvwm2 modules."
  #:type 'boolean
  #:group 'fvwm2-module)


(define-scwm-option *fvwm2-module-path* '("/usr/lib/X11/fvwm2"
					"/usr/local/lib/X11/fvwm2")
  "Set this to the path where your fvwm2 module binaries live."
  #:type 'path
  #:group 'fvwm2-module)


;; FIXME: Maybe scwm should always ignore sigpipes?
(sigaction SIGPIPE SIG_IGN)

(define app-window "0")
(define context "0") ; C_NO_CONTEXT

(define (fvwm2-module-read-packet port)
  (define (wait-for-input)
    (if (char-ready? port)
	#t
	(let ((result (select (list (fileno port))
			      '() '() 100 100)))
	  (if (null? (car result))
	      (wait-for-input)
	      result))))
  (wait-for-input)
  (let* ((window
	  (catch #t (lambda () (binary-read-long port))
		 (lambda args (error 
                               (string-append "Could not get window id:\n"
                                              (call-with-output-string
                                               (lambda (port)
                                                 (write args port)
                                                 (newline port))))))))
	 (msglen
	  (catch #t (lambda () (binary-read-long port))
		 (lambda args (error 
                               (string-append "Could not get msglen:\n"
                                              (call-with-output-string
                                               (lambda (port)
                                                 (write args port)
                                                 (newline port))))))))
	 (command
	  (catch #t (lambda () (binary-read msglen port))
		 (lambda args (error 
                               (string-append "Could not get command:\n"
                                              (call-with-output-string
                                               (lambda (port)
                                                 (write args port)
                                                 (newline port))))))))
	 (keepgoing
	  (catch #t (lambda () (binary-read-long port))
		 (lambda args (error 
                               (string-append "Could not keepgoing:\n"
                                              (call-with-output-string
                                               (lambda (port)
                                                 (write args port)
                                                 (newline port)))))))))
    (list window msglen command keepgoing)))

(define (fvwm2-module-send-config-info config-info port)
  (map (lambda (x) (send-config-info x port))
       config-info)
  (send-end-config-info port))

(define (longs->string . longs)
  (apply string-append 
	 (map long->string longs)))

(define (make-packet-header id body-length)
  (longs->string #xffffffff id (+ body-length 4) (current-time)))

(define (fvwm2-module-send-packet id body port)
  (if (not (= (modulo (string-length body)  4) 0))
      (error "Bad packet length"))
  (binary-write 
   (string-append (make-packet-header 
		   id 
		   (inexact->exact (round (/ (string-length body) 4))))
		  body) port)
  (force-output port))

(define (cdr-assq key alist)
  (cdr (assq key alist)))

;; Send mini-icon info 
(define (send-mini-icon-packet win port)
  (let ((mini-icon (window-mini-icon win)))
    (if mini-icon
	(let* ((props (image-properties mini-icon))
	       (body (string-append
		      (longs->string 
		       (window-id win) (window-frame-id win)
		       0 (cdr-assq 'width props)
		       (cdr-assq 'height props) (cdr-assq 'depth props)
		       (cdr-assq 'pixmap props) (cdr-assq 'mask props))
		      (pad-string-to-long (cdr-assq 'filename props)))))
	  (fvwm2-module-send-packet M_MINI_ICON body port)))))


(define (send-end-config-info port)
  (fvwm2-module-send-packet M_END_CONFIG_INFO "" port))

(define (send-string-packet type data1 data2 data3 str port)
  (let* ((data (string-append
		(longs->string data1 data2 data3)
		(pad-string-to-long str))))
    (fvwm2-module-send-packet type data port)))

(define (send-config-info str port)
  (send-string-packet M_CONFIG_INFO 0 0 0 (string-append str "\n") port))

(define (fvwm2-module-send-window-list port)
  ;; XXX - sadly, it is necessary to do a gratuitous send of
  ;; the desk and page info for the pager to work right. Icky!
  ;; broadcasting an M_NEW_DESK first can fortunately be avoided.
  (fvwm2-module-send-packet 
   M_NEW_PAGE
   (apply longs->string
	  (append (viewport-position)
		  (list (current-desk))
		  (list (* (- (car (desk-size)) 1) display-width)
			(* (- (cadr (desk-size)) 1) display-height))))
   port)

  (let ((focus-win (window-with-focus)))
    (fvwm2-module-send-packet
     M_FOCUS_CHANGE
     (longs->string
      (if focus-win (window-id focus-win) 0)
      (if focus-win (window-frame-id focus-win) 0)
      0 ;; was (unsigned long)Scr.Hilite, which is EVIL
      ;; Fvwm always uses the default decor to send this; perhaps we should
      ;; use the window's decor instead?
      (with-decor (default-decor)
		  (color-property (highlight-foreground) 
				  'pixel))
      (with-decor (default-decor)		   
		  (color-property (highlight-background) 
				  'pixel)))
     port))
  
  (map (lambda (w) (add-window w port)) (list-all-windows))
  (end-window-list port))


(define (add-window win port)
  (let* ((id (window-id win))
	 (frame-id (window-frame-id win))
	 (send-win-string 
	  (lambda (type str)
	    (send-string-packet type id frame-id 0 str port))))
    
    (fvwm2-module-send-packet 
     M_CONFIGURE_WINDOW 
     (marshal-fvwm2-config-info win)
     port)
    
    (send-win-string M_WINDOW_NAME (window-title win))
    
    (send-win-string M_ICON_NAME (window-icon-title win))
    
    (let* ((wi (window-icon win))
	   (wif (if wi (image-property wi 'filename) #f)))
      (if (and wif
	       (not (string=? wif "FromApp"))
	       (not (string=? wif "FromAppBitmap")))
	  (send-win-string M_ICON_FILE wif)))

    (send-win-string M_RES_CLASS (window-class win))
    (send-win-string M_RES_NAME (window-resource win))
  
    (if (iconified-window? win)
    	(fvwm2-module-send-packet 
    	 M_ICONIFY 
    	 (marshal-fvwm2-iconify-info win)
    	 port))

    (send-mini-icon-packet win port)
    ))

(define (end-window-list port)
  (fvwm2-module-send-packet M_END_WINDOWLIST "" port))

(define active-modules '())

(define (add-active-module! fmod) 
  (set! active-modules (cons fmod active-modules)))

(define (remove-active-module! fmod)
  (set! active-modules (delq! fmod active-modules)))

;; CRW:FIXME:MS: The (logior type mask) below doesn't make any sense.
;; 1) Shouldn't it be some sort of "and" instead of "or"?
;; 2) logior will never return #f, so the "true" branch will always be taken...
;; (There are several other functions in this file with the same problem.)

;; Alexander Vorobiev: It seems (not (zero? (logand type mask))) should
;; be used. I corrected all relevant functions.

(define (module-broadcast type num-data arg1 arg2 arg3 arg4 arg5 arg6 arg7)
  (if (optget *debug-fvwm-module*)
      (begin
	   (display "broadcasting message of type: ")
	   (write type)
	   (newline)))
  
  (map (lambda (fmod)
	 (let ((to-module-write (car fmod))
	       (mask (cadr fmod))
	       (args (list arg1 arg2 arg3 arg4 arg5 arg6 arg7)))
	   (set-cdr! (list-tail args (- num-data 1)) '())
	   (if (optget *debug-fvwm-module*)
   	       (begin
	          (display "mask: ")
	          (write mask)
	          (newline)
		  (display "(logand type mask): ")
		  (write (logand type mask))
		  (newline)))

	   (if (not (zero? (logand type mask)))
	       (fvwm2-module-send-packet 
		type
		(apply string-append
		       (map long->string args))
		to-module-write))))
       active-modules))

(add-hook! broadcast-hook module-broadcast) 

(define (module-broadcast-config type window)
  (map (lambda (fmod)
	 (let ((to-module-write (car fmod))
	       (mask (cadr fmod)))
	   (if (not (zero? (logand type mask)))
	       (fvwm2-module-send-packet 
		type
		(marshal-fvwm2-config-info window)
		to-module-write))))
       active-modules))

(add-hook! broadcast-config-hook module-broadcast-config)

(define (module-broadcast-name type data1 data2 data3 name)
  (map (lambda (fmod)
	 (let ((to-module-write (car fmod))
	       (mask (cadr fmod)))
	   (if (not (zero? (logand type mask)))
	       (fvwm2-module-send-packet 
		type
		(pad-string-to-long
		 (string-append
		  (long->string data1) 
		  (long->string data2) 
		  (long->string data3)
		  name))
		to-module-write))))
       active-modules))

(add-hook! broadcast-name-hook module-broadcast-name)

(define (module-broadcast-mini-icon type window)
  (map (lambda (fmod)
	 (let ((to-module-write (car fmod))
	       (mask (cadr fmod)))
	   (if (not (zero? (logand type mask)))
	       (send-mini-icon-packet window to-module-write))))
       active-modules))

(add-hook! broadcast-mini-icon-hook module-broadcast-mini-icon)


(define fvwm2-module-config-hash (make-hash-table 5))

;;;; Public interface

(define-public (get-fvwm2-module-config module-type)
  (cond 
   ((hash-ref fvwm2-module-config-hash module-type) => id)
   (else '())))

(define-public (register-fvwm2-module-config module-type . args)
  (hash-set! fvwm2-module-config-hash module-type args))

(define-public (append-fvwm2-module-config module-type . args)
  (hash-set! fvwm2-module-config-hash module-type 
	     (append (get-fvwm2-module-config module-type) args)))

(define-public (clear-fvwm2-module-config module-type)
  (hash-remove! fvwm2-module-config-hash module-type))

(define (aux-config-info)
  (let ((path (path-list->string-with-colons image-load-path)))
    (list
     (string-append "IconPath " path)
     (string-append "PixmapPath " path)
     "ColorLimit 0"
     "ClickTime 150")))

(define*-public (run-fvwm2-module module-name #:optional
				  (other-args '())	       
				  (config-file "~/.fvwm2rc")
				  (config-info
				   (append (aux-config-info)
                                           (get-fvwm2-module-config
					    (basename module-name)))))
  (let ((module-file (find-file-in-path module-name
					*fvwm2-module-path*)))
    (if module-file
	(let* ((from-module-pipe (pipe))
	       (from-module-read (car from-module-pipe))
	       (from-module-write (cdr from-module-pipe))
	       (to-module-pipe (pipe))
	       (to-module-read (car to-module-pipe))
	       (to-module-write (cdr to-module-pipe))
	       (input-hook-handle #f)
	       ;; MS:FIXME:: should use a struct, not a list.
	       (fmod (list to-module-write 0
			   (lambda ()
			     (fvwm2-module-send-config-info 
			      config-info to-module-write))
			   (lambda ()
			     (fvwm2-module-send-window-list
			      to-module-write))
			   #f))
	       (pid (primitive-fork)))
	  
	  (append! fmod
		   (list (lambda ()
			   (cond 
			    ((list-ref fmod 4)
			     (remove-active-module! fmod)
			     (catch #t
				    (lambda ()
				      (close-port to-module-write)
				      (close-port from-module-read))
				    (lambda args 
				      args))
			     (remove-input-hook! input-hook-handle)
			     (list-set! fmod 4 #f))))))
	  (append! fmod (basename module-name))

	  ;; actually, scwm waits on its children so it should be no problem.
	  ;; $SIG{'CHLD'} = sub { wait };
	  ;; Need to look up how to provide sigchld handler
	  (cond
	   ((= pid 0)
	    ;; child process
            (if *debug-fvwm-module*
                (map (lambda (x) (write x)) (list to-module-write from-module-read from-module-write to-module-read))
                (newline))
	    (close-port to-module-write)
	    (close-port from-module-read)
	    (let ((write-fd (number->string (fileno from-module-write)))
		  (read-fd (number->string (fileno to-module-read))))
              ;(display (string-append 
              ;          "child: " module-file " " write-fd " " read-fd " " 
              ;          config-file " " app-window " " context " ")
              ;          ;(string-append
              ;          ; (map (lambda (x) (string-append " " x))
              ;          ;      other-args) "\n"))
              ;         (current-output-port))
              (catch #t
		     (lambda () (apply execl module-file module-file 
				       write-fd read-fd config-file 
				       app-window context other-args))
		     (lambda args #t)))
	    (display "Exec failed.\n")
	    (close-port from-module-write)
	    (close-port to-module-read)
	    (exit 0)))
	  
	  ;; parent process
	  
	  (close-port to-module-read)
	  (close-port from-module-write)
	  
	  ;; mark fd-s close-on-exec so other processes won't inherit them
	  (fcntl to-module-write F_SETFD 1)
	  (fcntl from-module-read F_SETFD 1)
	  
	  ;; set o_nonblock for TO_MODULE_WR
	  ;; w/o this scwm will block when writing to a module
	  ;; pipe if that module is not reading;  observed
	  ;; effect to scwm is a hang that can be eliminated
	  ;; by killing the modules
	  (fcntl to-module-write F_SETFL O_NONBLOCK)
	  
	  (add-active-module! fmod)
	  (list-set! fmod 4 #t)
	  
	  (letrec 
	      ((packet-handler
		(lambda ()
		  (catch #t
			 (lambda ()
			   (let* ((packet (fvwm2-module-read-packet 
					   from-module-read))
				  (window-id (car packet))
				  (command (caddr packet)))	
			     (if (optget *debug-fvwm-module*)
				 (begin
				   (display "packet: ")
				   (write packet)
				   (newline)
				   (display "command =")
				   (write command)
				   (newline)
				   (display "fmod = ")
				   (write fmod)
				   (newline)
				   (display "window-id = ")
				   (write window-id)
				   (newline)))
			     (catch #t
				    (lambda ()
				      (if (= window-id 0)
					  (eval-fvwm-command command fmod)
					  (eval-fvwm-command command
							     fmod
							     (id->window 
							      window-id))))
				    ;; GJB:FIXME:MS: Can we get a better error?
				    (lambda args 
                                      (display "Error evaling packet: ")
                                      (write packet)
                                      (newline)
                                      (display "Error was:")
                                      (write args)
                                      (newline))
				    )
			     (if (not (list-ref fmod 4))
				 (remove-input-hook! input-hook-handle))))
			 (lambda args
			   (display "scwm: Error communicating with module: \n")
			   (write fmod)
			   (newline)
                           (display " error is: \n")
                           (write args)
			   (newline)
			   (backtrace)
			   (display " terminating connection.\n")
			   (kill-fvwm2-module fmod))))))
	    
	    (set! input-hook-handle (add-input-hook! from-module-read 
						     packet-handler))
	    fmod))
	(error (string-append "Unable to run fvwm2 module " module-name)))))

      
(define-public (kill-fvwm2-module fmod)
  ((list-ref fmod 5)))
       
(define-public (kill-all-fvwm2-modules)
  (for-each kill-fvwm2-module active-modules))

(define-public (kill-fvwm2-modules-by-name module-name)
  (for-each kill-fvwm2-module (filter (lambda (fmod)  
					(string=? (list-ref fmod 6) module-name)) 
				      active-modules)))

(add-hook! shutdown-hook
	   (lambda (restarting?)
	     (kill-all-fvwm2-modules)))


(define-public (fvwm2-pager-window)
  "Return a fvwm2 pager window, or #f if there is none."
  (let ((pagers
	 (list-windows #:only (win-and?? (class-match?? "FvwmModule") 
					 (resource-match?? "FvwmPager")))))
    (and (pair? pagers) (car pagers))))

(define*-public (raise-fvwm2-pager)
  "Raise a fvwm2 pager window in the stacking order."
  (interactive)
  (let ((w (fvwm2-pager-window)))
    (and w (raise-window w))))

(define*-public (lower-fvwm2-pager)
  "Lower a fvwm2 pager window in the stacking order."
  (interactive)
  (let ((w (fvwm2-pager-window)))
    (and w (lower-window w))))

;; (deiconify-window (fvwm2-pager-window))
;; (iconify-window (fvwm2-pager-window))
