;; $Id$

;; scwm-option-variables could ultimately take the place
;; of the doc tools generated `user-options' variable
(define-public scwm-option-variables '())
;;(set! scwm-option-variables '())

;; the setter and getter should be optional
(defmacro-public define-scwm-option (sym docstring value . rest)
  "Define SYM to be a new scwm user option with VALUE as its default value."
  (let ((setter (if (pair? rest) (car rest) #f))
	(getter (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) #f)))
    `(let ((answer (define ,sym ,value)))
       (set-object-property! ,sym 'doc ,docstring)
       (if ,setter (set-object-property! ,sym 'setter ,setter))
       (if ,getter (set-object-property! ,sym 'getter ,getter))
       (set! scwm-option-variables (cons ',sym scwm-option-variables))
       answer)))

(define-public (scwm-option-documentation option-var)
  "Return the documentation for VAR."
  (object-property option-var 'doc))

(define-public (scwm-option-setter option-var)
  "Return the setter for VAR."
  (object-property option-var 'setter))

(define-public (scwm-option-getter option-var)
  "Return the getter for VAR."
  (object-property option-var 'getter))

(defmacro-public scwm-option-set! (var value)
  "Set option VAR to VALUE."
  `(let ((s (scwm-option-setter ,var)))
     (if s (s ,value)
	 (set! ,var ,value))))

(defmacro-public scwm-option-get (var)
  "Get option VAR's value."
  `(let ((g (scwm-option-getter ,var)))
     (if g (g)
	 ,var)))


;; (define A 5)
;; (set! A 9)
;; (define (set-a! val) (set! A val))
;; (define (get-a) A)

;; (define-scwm-option a "A is set through setter/getters" 12 set-a! get-a)
;; (define-scwm-option b "B has a docstring but no setter/getter" 10)

;; (scwm-option-documentation a)
;; (scwm-option-documentation b)
;; ((scwm-option-setter a) 4)
;; (scwm-option-setter b)
;; (scwm-option-getter b)
;; (scwm-option-set! a 9)
;; (scwm-option-set! b 14)
;; ((scwm-option-getter a))
;; (scwm-option-get a)
;; (scwm-option-get b)
;; (scwm-option-documentation b)
;; scwm-option-variables
;; (map (lambda (v) (scwm-option-documentation (eval v))) scwm-option-variables)

;; Why do these not work? --03/24/99 gjb
;; the binding doesn't get made somewhere that I can see it...

;; (define-scwm-option-in-root c "c's new docstring" 13)

(defmacro define-scwm-option-in-root (sym docstring value)
  "Define SYM to be a new scwm user option with VALUE as its default value.
This puts the variable in the root module."
  `(let ((cm (current-module))
	 (answer #f))
      (set-current-module the-root-module)
      (set! answer (define ,sym ,value))
      (set-object-property! ,sym 'doc ,docstring)
      (set-current-module cm)
      answer))

(defmacro define-scwm-option-in-root-safely (sym docstring value)
  "Define SYM to be a new scwm user option with VALUE as its default value.
This puts the variable in the root module."
  `(let ((cm (current-module)))
     (dynamic-wind
      (lambda () (set-current-module the-root-module))
      (lambda () 
	(let ((answer (define ,sym ,value)))
	  (set-object-property! ,sym 'doc ,docstring)
	  answer))
      (lambda () (set-current-module cm)))))
