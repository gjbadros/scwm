;;;; $Id$
;;;; Copyright (C) 1999 Jeff W. Nichols

;;; (use-scwm-modules ui-constraints-composition)
(define-module (app scwm ui-constraints-composition)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm prompt-string)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm ui-constraints-classes)
  :use-module (cassowary constraints)
  :use-module (app scwm window-selection))


;; PRIVATE HELPERS

;; these functions are used by the ui-constraint-composition class to define
;; the functions necessary for a ui-constraint.  They are useful for any 
;; arbitrary compositions because they reference the cnlist

;; macro recording

;; global for storing macro as it is specified

(define-public ui-constraints-composition-constraint-list '())

;; hooks for inserting into the lists

(define (add-constraint-to-composition ui-constraint constraint-args)
  (set! ui-constraints-composition-constraint-list 
	(cons (list ui-constraint constraint-args) ui-constraints-composition-constraint-list)))

(define (remove-constraint-from-arglist ui-constraint arglist)
  (if (null? arglist) '()
      (let ((cn (car (car arglist))))
	(if (eq? cn ui-constraint)
	    (cdr arglist)
	    (remove-constraint-from-arglist ui-constraint arglist)))))

(define (remove-constraint-from-composition ui-constraint)
  (set! ui-constraints-composition-constraint-list
	(remove-constraint-from-arglist ui-constraint ui-constraints-composition-constraint-list)))


;; routines to do the recording

(define-public (ui-constraints-composition-begin) 
  "Begin recording the constraints used for the creation of a composition.
End this recording by calling 'ui-constraints-composition-end'."
  (set! ui-constraints-composition-constraint-list '())
  (add-hook-once! constraint-composition-record-hook add-constraint-to-composition)
  (add-hook-once! constraint-delete-hook remove-constraint-from-composition))


;; PRIVATE HELPERS
;; returns a list of the windows used in the constraints in passed in list
(define (get-cn-windows cnlist)
  (if (null? cnlist)
      '()
      (let* ((cnwinlist '())
	     (winlist (get-cn-windows (cdr cnlist)))
	     (cn (car (car cnlist)))
	     (cnwl (reverse (ui-constraint-windows cn))))
	(for-each (lambda (w) (if (not (member w winlist)) (set! cnwinlist (cons w cnwinlist)))) cnwl)
	(append cnwinlist winlist))))
  
;; map a window object to its position in the list
(define (pos-in-list item lst)
  (let ((sublst (member item lst)))
    (if sublst
	(- (length lst) (length sublst))
	#f)))
	 
;; replace all windows in argument list with '('win <num>)
(define (replace-windows-with-proxy alist wlist)
  (if (null? alist) 
      '()
      (let ((elem (car alist))
	    (rlist (replace-windows-with-proxy (cdr alist) wlist)))
	(cons (cond
	       ((pair? elem) (replace-windows-with-proxy elem wlist))
	       ((window? elem) (list 'win (pos-in-list elem wlist)))
	       (else elem))
	      rlist))))


(define*-public (ui-constraints-composition-end #&optional (cancel? #f))
  "End the recording of a constraint composition.  NAME is the name to be 
given to the new composition.  Optionally, CANCEL may be specified to cancel
the construction of a composition."
  (if (null? ui-constraints-composition-constraint-list) #f
      (let* ((cnlist (reverse ui-constraints-composition-constraint-list))
	     (winlist (get-cn-windows cnlist))
	     (classlist (map (lambda (l) 
			       (let* ((cn (car l))
				      (args (cadr l))
				      (class (ui-constraint-class cn))
				      (classname (ui-constraint-class-name class))
				      (winnumlist (replace-windows-with-proxy args winlist)))
				 (cons classname winnumlist)))
			     cnlist))
	     (ui-constraint-prompter-msgwin ui-constraint-prompter-msgwin))
	
	(remove-hook! constraint-composition-record-hook add-constraint-to-composition)
	(remove-hook! constraint-delete-hook remove-constraint-from-composition)
	
	;; determine windows within constraint (in order they were chosen by the user)
	
	(prompt-string 
	 "Enter name: " 
	 (lambda (name) 
	   (if (and name (> (string-length name) 0))
	       (begin
		 (let ((newclass
			(make-ui-constraint-class 
			 name (string-append name " (User-recorded)") (length winlist) composition-ctr 
			 (eval
			  `(begin
			     (use-scwm-modules ui-constraints-classes)
			     (lambda () 
			       (if (eqv? (length (selected-windows-list)) ,(length winlist))
				   (list (selected-windows-list) ',classlist)
				   (let ((winlst '())
					 (win #t))
				     (if (do ((i 1 (+ i 1)))
					     ((or (> i ,(length winlist)) (not win)) win)
					   (set! win (select-window-interactively 
						      (string-append "Select window #" (number->string i) ": ") 
						      ui-constraint-prompter-msgwin))
					   (set! winlst (cons win winlst)))
					 (list winlst ',classlist)
					 #f))))))
			 composition-draw-proc
			 cl-is-constraint-satisfied?
			 "composition.xpm" #f  ;; 
			 composition-menuname)))
		   ;; write out an expression that can be used to persist
		   ;; this abstraction
		   (write (ui-constraint-class-creator newclass))
		   (newline)))))))))
    
;; (use-scwm-modules constraints ui-constraints ui-constraints-composition ui-constraints-buttons ui-constraints-gtk-toggle-menu ui-constraints-classes)
;; (start-constraints)
;; (ui-constraints-gtk-toggle-menu)
;; (ui-constraints-composition-begin)
;; (define w (ui-constraints-composition-end))
;; (car global-constraint-class-list)
;; (start-ui-constraints-buttons)


;; composition class functions
;; these are the definitions of functions required to make a new ui-constraint-class.

;; constructor
;; arg-list is a list in the format: ((ctr-name arg1 arg2) (ctr-name arg1 arg2))
;; the arguments may be lists of the form ('win <integer>).  These values need
;; to be transferred into window objects from the winlist.

(define-public (cncat list)
  (if (null? list) 
      '()
      (append (ui-constraint-cn (car list)) (cncat (cdr list)))))

;; replace all windows in argument list with '('win <num>)
(define-public (replace-proxies-with-windows alist wlist)
  (if (null? alist) 
      '()
      (let ((elem (car alist))
	    (rlist (replace-proxies-with-windows (cdr alist) wlist)))
	(cons (cond
	       ((pair? elem) 
		(if (eq? (car elem) 'win)
		    (list-ref wlist (cadr elem))
		    (replace-proxies-with-windows elem wlist)))
	       (else elem))
	      rlist))))

(define* (composition-ctr winlist arg-list #&optional (enable? #f))
  (let* ((ui-cns (map (lambda (ctrdef) 
			(let ((arglst (replace-proxies-with-windows (cdr ctrdef) winlist))
			      (class (get-ui-constraint-class-by-name (car ctrdef))))
			  (make-ui-constraint class arglst #:visible? #f)))
		      arg-list))
	 (cns (cncat ui-cns)))
    (and enable? (for-each (lambda (ui-cn) (enable-ui-constraint ui-cn)) ui-cns))
    (list cns winlist ui-cns)))

;; draw-proc

(define (composition-draw-proc ui-constraint enable focus mode)
  (let ((ui-cns (car (ui-constraint-opts ui-constraint))))
    (for-each (lambda (ui-cn)
		(let ((draw-proc (ui-constraint-class-draw-proc (ui-constraint-class ui-cn))))
		  (draw-proc ui-cn enable focus mode)))
	      ui-cns)))

;; menuname-proc

(define (composition-menuname ui-constraint)
  (ui-constraint-class-name (ui-constraint-class ui-constraint)))

