;;;; 	Copyright (C) 1998 Maciej Stachowiak
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
  :use-module (app scwm base)
  :use-module (app scwm module-types)
  :use-module (app scwm bincomm)
  :use-module (app scwm fvwm-eval)
  :use-module (app scwm optargs))



;;   fvwm-module
;; Supports running fvwm2 modules from scwm - this is mostly useful for
;; the pager. Here is an example of running a module in a scwmrc:
;;
;;(define m1 (run-fvwm-module  
;;	    ;; path to the module
;;	    "/mit/windowmanagers/lib/X11/fvwm2/FvwmPager"
;;	    ;; your .fvwm2rc (does not need to exist for most modules)
;;	    "~/.fvwm2rc"
;;	    ;; list of configuration lines
;;	    '("*FvwmPagerBack grey76"
;;	      "*FvwmPagerFore black"
;;	      "*FvwmPagerHilight navyblue"
;;	      "*FvwmPagerFont none"
;;	      "*FvwmPagerDeskTopScale 40"
;;	      "*FvwmPagerLabel 0 Top"
;;	      "*FvwmPagerLabel 1 Bottom"
;;	      "*FvwmPagerSmallFont 5x8")
;;	    '("0" "2")))
;;
;; To later shut down the module, use kill-fvwm-module:
;;
;;   (kill-fvwm-module m1)
;;
;;
;; This has only been tested so far with the FvwmPager module. Other modules
;; should work if all the commands they use are supported by the fvwm-eval
;; module (q.v.).
;;

(define app-window "0")
(define context "0") ; C_NO_CONTEXT

(define display-width (car (display-size)))
(define display-height (cadr (display-size)))

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
		 (lambda args (error "Could not get window id"))))
	 (msglen
	  (catch #t (lambda () (binary-read-long port))
		 (lambda args (error "Could not get msglen"))))
	 (command
	  (catch #t (lambda () (binary-read msglen port))
		 (lambda args (error "Could not get command"))))
	 (keepgoing
	  (catch #t (lambda () (binary-read-long port))
		 (lambda args (error "Could not get keepgoing")))))
	 (list window msglen command keepgoing)))

(define (fvwm2-module-send-config-info config-info port)
  (map (lambda (x) (send-config-info x port))
       config-info)
  (send-end-config-info port))

(define (make-packet-header id body-length)
  (apply string-append
	 (map long->string 
	      (list #xffffffff id (+ body-length 4) (current-time)))))

(define (fvwm2-module-send-packet id body port)
  (if (not (= (modulo (string-length body)  4) 0))
      (error "Bad packet length"))
  (binary-write 
   (string-append (make-packet-header id (/ (string-length body) 4))
		  body) port)
  (force-output port))

(define (send-end-config-info port)
  (fvwm2-module-send-packet M_END_CONFIG_INFO "" port))

(define (send-config-info str port)
  (let* ((data (string-append 
		(long->string 0)
		(long->string 0)
		(long->string 0)
		(pad-string-to-long str))))
    (fvwm2-module-send-packet M_CONFIG_INFO data port)))

(define (fvwm2-module-send-window-list port)
  ;; XXX - sadly, it is necessary to do a gratuitous send of
  ;; the desk and page info for the pager to work right. Icky!
  ;; broadcasting an M_NEW_DESK first can fortunately be avoided.
  (fvwm2-module-send-packet 
   M_NEW_PAGE
   (apply string-append
	  (map (lambda (x) (long->string x)) 
	       (append (viewport-position)
		       (list (current-desk))
		       (list (* (- (car (desk-size)) 1) display-width)
			     (* (- (cadr (desk-size)) 1) display-height)))))
   port)

  ;; more stuff should really be sent here - an M_FOCUS_CHANGE for the
  ;; focused win, per window: M_WINDOW_NAME, M_ICON_NAME, M_ICON_FILE
  ;; (is this possible?)  M_RES_CLASS, M_RES_NAME, M_ICONIFY if
  ;; iconfied and the icon is unmapped (???) M_MINI_ICON, and finally
  ;; the focus win again according to fvwm source.

  (map (lambda (w) (add-window w port)) (list-all-windows))
  (end-window-list port))

;; FIXMS: useful enough to move somewhere public?
(define (id->window id)
  (let ((candidates (list-windows 
		     #:only (lambda (w) (= (window-id w) id)))))
    (if (not (null? candidates))
	(car candidates)
	#f)))

(define (add-window win port)
  (fvwm2-module-send-packet 
   M_ADD_WINDOW 
   (marshal-fvwm2-config-info win)
   port))

(define (end-window-list port)
  (fvwm2-module-send-packet M_END_WINDOWLIST "" port))

(define (add-input-hook-checking port thunk)
  (if (char-ready? port)
      (thunk)
      (add-input-hook (fileno port) thunk)))

(define active-modules '())

(define (add-active-module! fmod) 
  (set! active-modules (cons fmod active-modules)))

(define (remove-active-module! fmod)
  (set! active-modules (delq! fmod active-modules)))

(set! broadcast-hook 
      (lambda (type num-data . args)
	(map (lambda (fmod)
	       (let ((to-module-write (car fmod))
		     (mask (cadr fmod)))
		 (if (logior type mask)
		     (fvwm2-module-send-packet 
		      type
		      (apply string-append
			     (map (lambda (x y) 
				    (long->string x)) 
				  args (iota num-data))) 
		      to-module-write))))
	     active-modules)))

(set! broadcast-config-hook 
      (lambda (type window)
	(map (lambda (fmod)
	       (let ((to-module-write (car fmod))
		     (mask (cadr fmod)))
		 (if (logior type mask)
		     (fvwm2-module-send-packet 
		      type
		      (marshal-fvwm2-config-info window)
		      to-module-write))))
	     active-modules)))

(set! broadcast-name-hook
      (lambda (type data1 data2 data3 name)
	(map (lambda (fmod)
	       (let ((to-module-write (car fmod))
		     (mask (cadr fmod)))
		 (if (logior type mask)
		     (fvwm2-module-send-packet 
		      type
		      (pad-string-to-long
		       (string-append
			(long->string data1) 
			(long->string data2) 
			(long->string data3)
			name))
		      to-module-write))))
	     active-modules)))

(define*-public (run-fvwm-module module-file config-file config-info
				#&optional (other-args ""))
  (let* ((from-module-pipe (pipe))
	 (from-module-read (car from-module-pipe))
	 (from-module-write (cdr from-module-pipe))
	 (to-module-pipe (pipe))
	 (to-module-read (car to-module-pipe))
	 (to-module-write (cdr to-module-pipe))
	 ;; FIXMS: should use a struct, not a list.
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
				(close to-module-write)
				(close from-module-read))
			      (lambda args args))
		       (list-set! fmod 4 #f))))))

    ;; actually, scwm waits on its children so it should be no problem.
    ;; $SIG{'CHLD'} = sub { wait };
    ;; Need to look up how to provide sigchld handler
    (cond
     ((= pid 0)
      ;; child process
      (close-port to-module-write)
      (close-port from-module-read)
      (let ((write-fd (number->string (fileno from-module-write)))
	    (read-fd (number->string (fileno to-module-read))))
;;	(display (string-append 
;;		  "child: " module-file " " write-fd " " read-fd " " config-file 
;;		  " " app-window " " context " " (apply string-append
;;	(map (lambda (x) (string-apppend " " x)) other-args) "\n") 
;;	(current-output-port))
	(apply execl module-file module-file write-fd read-fd config-file 
	       app-window context other-args))
      (display "Exec failed.\n")
      (exit 0)))

    ;; parent process

    (close-port to-module-read)
    (close-port from-module-write)

    ;; mark fd-s close-on-exec so other processes won't inherit them
    (fcntl to-module-write F_SETFD 1)
    (fcntl from-module-read F_SETFD 1)

    ;; FIXGJB: set o_nonblock for TO_MODULE_WR
    ;; GJBFIX: why is that necessary?
    ;; (fctl to-module-write F_SETFL O_NONBLOCK)

    (add-active-module! fmod)
    (list-set! fmod 4 #t)

    (letrec 
	((packet-handler
	  (lambda ()
	    (let* ((packet (fvwm2-module-read-packet from-module-read))
		   (window-id (car packet))
		   (command (caddr packet))
		   ;;(split-result (split-before-char #\space command 
		   ;;				    (lambda args args)))
		   ;; (main-cmd (car split-result))
		   ;;(args (cadr split-result))
		   )	
	      
	      ;; (display "packet: ")
	      ;; (write packet)
	      ;; (newline)

	      (eval-fvwm-command command fmod (id->window window-id))

	    (if (list-ref fmod 4)
		(add-input-hook-checking from-module-read packet-handler))))))
      


      (add-input-hook-checking from-module-read packet-handler)
      fmod)))

(define-public (kill-fvwm-module fmod)
  ((list-ref fmod 5)))











