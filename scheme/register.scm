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

(define-public (get-register-name)
  "Prompt for a register name and return a corresponding symbol."
  (with-message-window-shown 
   (make-message-window-clone-default "Register?")
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

(define*-public (focus-to-register #&optional (register (get-register-name)))
  "Save the currently-focused window to REGISTER."
  (interactive)
  (if register
      (let ((win (window-with-focus)))
	(set-register register win))))

(define*-public (window-configuration-to-register 
		 #&optional (win (or (window-with-focus) (get-window)))
		 (register (get-register-name)))
  "Save the configuration of WIN to REGISTER."
  (interactive)
  (if (and win register)
      (set-register register (window-configuration win))))

(define*-public (selected-windows-to-register #&optional (register (get-register-name)))
  "Save the current set of selected windows to REGISTER."
  (interactive)
  (if register
      (let ((val (selected-windows-list)))
	(set-register register val))))

(define*-public (global-window-configuration-to-register
		 #&optional (register (get-register-name)))
  "Save the global configuration of windows to REGISTER."
  (interactive)
  (if register
      (set-register register (global-window-configuration))))

(define-public (list-of-windows? list-of-windows)
  (and-map window? list-of-windows))

(define*-public (jump-to-register #&optional (register (get-register-name)))
  "Restore the state saved in REGISTER."
  (interactive)
  (let ((val (get-register register)))
    (if val
	(cond
	 ((window? val) (focus-change-warp-pointer val))
	 ((window-configuration? val) (copy-window-configuration val (car val))
	  (focus-window (car val)))
	 ((list-of-windows? val) (set-selected-windows-list! val))
	 (val (restore-global-window-configuration val)))
	(begin
	  (display-message-briefly "Empty register")
	  #f))))
  
#!
register
(bind-key 'all "H-j" jump-to-register)
(bind-key 'all "H-f" focus-to-register)
(bind-key 'all "H-c" window-configuration-to-register)
(bind-key 'all "H-x" global-window-configuration-to-register)
!#
