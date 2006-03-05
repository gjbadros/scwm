;; $Id$
;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm register)
  :use-module (app scwm optargs)
  :use-module (app scwm window-configuration)
  :use-module (app scwm message-window)
  :use-module (app scwm winops)
  :use-module (app scwm window-selection)
  :use-module (app scwm modifier-key-bindings)
  :use-module (app scwm base))

(define register-alist '())

(define-public (get-register-alist)
  "Return the register-alist."
  register-alist)

(define*-public (get-register-name #:optional (descriptor #f))
  "Prompt for a register name and return a corresponding symbol.
If DESCRIPTOR is given, then use DECRIPTOR before \"Register?\"
in the prompt."
  (with-message-window-shown 
   (make-message-window-clone-default 
    (string-append 
     (if descriptor (string-append descriptor "") "")
     "Register?"))
   (let* ((event (get-key-event))
	  (keycode (caddr event))
	  (key (keycode->keysym keycode))
	  (shifted? (mod-mask-shift? (cadr event))))
     (if (and key (= 1 (string-length key)))
	 (begin
	   (if shifted? (string-upcase! key))
	   (string->symbol key))
	 #f))))

(define-public (set-register register value)
  "Set contents of Scwm register named REGISTER to VALUE.  Returns VALUE."
  (let ((aelt (assq register register-alist)))
    (if aelt
	(set-cdr! aelt value)
	(begin
	  (set! aelt (cons register value))
	  (set! register-alist (cons aelt register-alist))))
    value))

(define-public (get-register register)
  "Return contents of Scwm register named REGISTER, or #f if none."
  (assq-ref register-alist register))

;;; (set-register 'a 3)
;;; (set-register 'A 5)

;;; (get-register 'a)
;;; (window? (get-register 'a))
;;; (window-configuration? (get-register 'a))
;;; (get-register 'b)
;;; (get-register 'j)
;;; (get-register 'A)
;;; (get-register 'B)

;; (focus-to-register)
;; (window-configuration-to-register)
;; (global-window-configuration-to-register)
;; (jump-to-register)
;; (selected-windows-to-register)

(define*-public (focus-to-register #:optional (register (get-register-name "Focus-to-")))
  "Save the currently-focused window to REGISTER."
  (interactive)
  (if register
      (let ((win (window-with-focus)))
	(set-register register win))))

(define*-public (window-configuration-to-register 
		 #:optional (win (or (window-with-focus) (get-window)))
		 (register (get-register-name "Window-configuration-to-")))
  "Save the configuration of WIN to REGISTER."
  (interactive)
  (if (and win register)
      (set-register register (window-configuration win))))

(define*-public (selected-windows-to-register 
		 #:optional (register (get-register-name "Selected-windows-to-")))
  "Save the current set of selected windows to REGISTER."
  (interactive)
  (if register
      (let ((val (selected-windows-list)))
	(set-register register val))))

(define*-public (global-window-configuration-to-register
		 #:optional (register (get-register-name "Global-configurations-to-")))
  "Save the global configuration of windows to REGISTER."
  (interactive)
  (if register
      (set-register register (global-window-configuration))))

(define-public (list-of-windows? list-of-windows)
  (and-map window? list-of-windows))

(define-public (register-value-type val)
  (cond 
   ((window? val) 'window)
   ((window-configuration? val) 'window-configuration)
   ((list-of-windows? val) 'list-of-windows)
   (val 'global-window-configuration)
   (else #f)))

(define*-public (jump-to-register #:optional (register (get-register-name "Jump-to-")))
  "Restore the state saved in REGISTER."
  (interactive)
  (let ((val (get-register register)))
    (if val
	(case (register-value-type val)
	  ((window) (focus-change-warp-pointer val))
	  ((window-configuration)
	   (begin (copy-window-configuration val (car val))
		  (focus-window (car val))))
	  ((list-of-windows) (set-selected-windows-list! val))
	  ((global-window-configuration) (restore-global-window-configuration val))
	  (else (display-message-briefly "Cannot handle that register")))
	(begin
	  (display-message-briefly "Empty register")
	  #f))))

(define-public (register-type-mapping)
  (map (lambda (cell) 
	 (string-append (symbol->string (car cell))
			"\t"
			(symbol->string (register-value-type (cdr cell)))))
       (sort (get-register-alist) (lambda (cell) (string<? (symbol->string (car cell)))))))

(define-public (register-type-mapping-string)
  (let ((answer ""))
    (for-each (lambda (str) (set! answer (string-append answer str "\n")))
	      (register-type-mapping))
    answer))

(define*-public (popup-register-winlist)
  "Popup a menu displaying all the windows that stored in registers.
Selecting an item from the winlist sets focus to that window."
  (interactive)
  (popup-menu
   (menu
    (map (lambda (regwin-cell)
	   (let ((reg (car regwin-cell))
		 (win (cdr regwin-cell)))
	     (menuitem 
	      (string-append (string-upcase (symbol->string reg)) ".\t" (window-title win))
	      ;;	  #:extra-label (symbol->string reg)
	      #:action (lambda () (focus-change-warp-pointer win))
	      #:hotkey-prefs (symbol->string reg))))
	 (filter-map (lambda (cell) 
		       (let ((sym (car cell))
			     (val (cdr cell)))
			 (and (window? val) cell)))
		     (sort 
		      (get-register-alist)
		      (lambda (a b)
			(let ((s (symbol->string (car a)))
			      (t (symbol->string (car b))))
			  (string<? s t)))))))))

#!
register
(bind-key 'all "H-j" jump-to-register)
(bind-key 'all "H-f" focus-to-register)
(bind-key 'all "H-c" window-configuration-to-register)
(bind-key 'all "H-x" global-window-configuration-to-register)

!#
