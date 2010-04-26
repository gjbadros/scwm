;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros
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


;; See also guile's (ice-9 session) module
(define-module (app scwm reflection)
  :use-module (ice-9 session)
  :use-module (ice-9 regex)
  :use-module (srfi srfi-1)
  :use-module (app scwm listops))

(define-public (procedure->string proc)
  "Return a string that represents the name of PROC.
Returns \"<anonymous-procedure>\" for procedures without names and \"<none>\"
if passed something which is not a procedure."
  (if (and proc (procedure? proc))
      (symbol->string (or (procedure-name proc) '<anonymous-procedure>))
      "<none>"))

;; (interpret-as-procedure "describe-key")
;; (interpret-as-procedure 'describe-key)
;; (interpret-as-procedure describe-key)
;; (interpret-as-procedure #f)
;; (interpret-as-procedure #\?)
(define-public (interpret-as-procedure proc-or-string-or-symbol)
  "Return a procedure given its value, its symbol, or its name.
Return #f if PROC-OR-STRING-OR-SYMBOL is none of those things."
  (or (and (procedure? proc-or-string-or-symbol) 
	   proc-or-string-or-symbol)
      (and (string? proc-or-string-or-symbol)
	   (procedure-string->procedure proc-or-string-or-symbol))
      (and (symbol? proc-or-string-or-symbol)
	   (eval proc-or-string-or-symbol (current-module)))))

;; (procedure-string->procedure "push-focus-window")
(define-public (procedure-string->procedure proc-name)
  "Return a procedure given its name (PROC-NAME).
Returns #f if PROC-NAME is not a procedure name."
  (catch #t
	 (lambda ()
	   (eval (string->symbol proc-name) (current-module)))
	 (lambda (key . args)
	   #f)))

(define-public (procedure-arity proc)
  "Return the arity values for PROC.
Three values are returned in a list: (num-required num-optional rest-argument-p)."
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (procedure-property proc 'arity))

(define-public (procedure-num-required-args proc)
  "Return the number of required arguments for PROC."
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (car (procedure-arity proc)))

(define-public (procedure-num-optional-args proc)
  "Return the number of optional arguments for PROC."
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (cadr (procedure-arity proc)))

(define-public (procedure-takes-rest-arg? proc)
  "Return #t iff PROC take a rest argument."
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (caddr (procedure-arity proc)))


(define-public (procedure-formals proc)
  "Return a list of the formal arguments for PROC.
Works for both primitives and procedures. If PROC was
created using an optargs *-format macro, this 
procedure will not provide much useful information.
See instead `procedure-keyword-arguments' and 
`procedure-optional-arguments'."
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (or (procedure-property proc 'arglist)
      (catch #t
	     (lambda ()
	       (let ((source (procedure-source proc)))
		 (if source
		     (cadr source)
		     #f)))
	     (lambda (key . args)
	       #f))))

(define-public (procedure-required-formals proc)
  "Return a list of the required formal arguments for PROC.
Returns #f if PROC is a primitive.  This will not include
any \"lambda*\" formals generated using optargs. See also
`procedure-keyword-arguments' and `procedure-optional-arguments'."
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (let ((formals (procedure-formals proc)))
    (if (or (not formals) (symbol? formals))
	'()
	(begin
	  (filter-map (lambda (f) (if (string-match "^lambda\\*:" 
						    (symbol->string f))
				      #f
				      f)) 
		      (list-head formals (procedure-num-required-args proc)))))))


(define-public (procedure-optargs-arglist proc)
  "BROKEN:Return a list of the optargs keyword arguments for PROC.
Returns #f if PROC was not defined using keyword or optional
arguments (this includes procedures defined using standard . rest
syntax).  Otherwise returns a list such as '(foo #:optional bar).
Note that these currently do not display in their expected format"
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (procedure-property proc 'optargs-arglist))


(define-public (procedure-keyword-formals proc)
  "BROKEN:Returns an a-list of the optargs keyword arguments and default values for PROC."
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (let ((optargs-arglist (procedure-optargs-arglist proc)))
    (if optargs-arglist
	(let ((got-key #f))
	  (filter-map (lambda (i) (if (eq? i '#:key)
				      (set! got-key #t))
			      (and got-key 
				   (pair? i) i)) optargs-arglist))
	#f)))

(define-public (procedure-optional-formals proc)
  "BROKEN:Returns a list of the optional arguments for PROC."
  (and (symbol? proc) (set! proc (eval proc (current-module))))
  (let ((arglist (procedure-property proc 'arglist)))
    (if arglist
	(list-tail arglist (procedure-num-required-args proc))
	(let ((optargs-arglist (procedure-optargs-arglist proc)))
	  (if optargs-arglist
	      (let ((in-optional #f))
		(filter-map (lambda (i) (case i
					  ('#:optional
					   (set! in-optional #t))
					  ('#:key
					   (set! in-optional #f)))
				    (and in-optional
					 (pair? i)
					 (car i))) optargs-arglist))
	      (let ((formals (procedure-formals proc)))
		(if formals
		    (list-tail formals (procedure-num-required-args proc))
		    '() )))))))


;; very slightly changed from ice-9 session's apropos-internal
(define-public (apropos-internal-with-modules rgx)
  "Return a list of accessible variable names and the modules they are defined in.
The list elements are of the form '(module . procedure)"
  (let ((match (make-regexp rgx))
	(modules (cons (current-module)
		       (module-uses (current-module))))
	(recorded (make-vector 61 '()))
	(vars (cons '() '())))
    (let ((last vars))
      (for-each
       (lambda (module)
	 (for-each
	  (lambda (obarray)
	    (array-for-each
	     (lambda (oblist)
	       (for-each
		(lambda (x)
		  (if (and (regexp-exec match (car x))
			   (not (hashq-get-handle recorded (car x))))
		      (begin
			(set-cdr! last (cons (cons (module-name module) (car x)) '()))
			(set! last (cdr last))
			(hashq-set! recorded (car x) #t))))
		oblist))
	     obarray))
	  (if (or (eq? module the-scm-module)
		  (eq? module the-root-module))
	      (list (builtin-weak-bindings)
		    (builtin-bindings))
	      (list (module-obarray module)))))
       modules))
    (cdr vars)))

;; (procedure-is-interactive? get-window)
(define-public (procedure-interactive-spec proc)
  (procedure-property proc 'interactive))

(define-public (procedure-is-interactive? proc)
  "Return #t iff PROC is interactive."
  (->bool (procedure-interactive-spec proc)))

;; (procedure-apropos-with-modules "get-window")
(define-public (procedure-apropos-with-modules rgx)
  "Returns a list of procedures that match RGX along with defined-in modules.
The returned list contains pairs (modulesym . procsym)"
  (filter-map (lambda (p) (let ((m-p (eval (cdr p) (current-module)))) 
			    (if (procedure? m-p) p #f)))
	      (apropos-internal-with-modules rgx)))

;; (procedure-apropos "get-window")
(define-public (procedure-apropos rgx)
  "Returns a list of procedures that match RGX.
This returns a simple list of procedure objects."
  (map (lambda (p) (eval (cdr p) (current-module))) (procedure-apropos-with-modules rgx)))

;; (interactive-procedure-apropos-with-modules "get-window")
(define-public (interactive-procedure-apropos-with-modules rgx)
  "BROKEN: Returns a list of procedures that match RGX and that can take no arguments.
I.e., they are interactive procedures useful for bindings.
The returned list contains pairs (modulesym . procsym)"
  (filter-map (lambda (p) (let ((m-p (eval (cdr p) (current-module)))) 
			    (if (and (procedure? m-p) 
				     (procedure-is-interactive? m-p))
				p
				#f)))
	      (apropos-internal-with-modules rgx)))

;; (interactive-procedure-apropos "get-window")
(define-public (interactive-procedure-apropos rgx)
  "BROKEN: Returns a list of interactive procedures that match RGX.
This returns a simple list of procedure objects."
  (map (lambda (p) (eval (cdr p) (current-module))) (interactive-procedure-apropos-with-modules rgx)))

;(map procedure-required-formals (procedure-apropos "n"))
;(map procedure-optional-formals (procedure-apropos "n"))
;(map procedure-keyword-formals (procedure-apropos "n"))

;(procedure-source current-module)
;(procedure-formals current-module)

;(procedure? current-module)

(define-public (context->brief-context context)
  "NOT TESTED: No doc."
  (cond ((memq 'all context) 'all)
	((= 1 (length context)) (car context))
	(else context)))

(define-public (context->string context)
  "NOT TESTED: No doc."
  (with-output-to-string (lambda () (write context))))

(define-public (raw-binding->string raw-binding)
  "NOT TESTED: No doc."
  (let ((mouse? (list-ref raw-binding 0))
	(context (list-ref raw-binding 1))
	(modmask (list-ref raw-binding 2))
	(keybut (list-ref raw-binding 3))
	(proc1 (list-ref raw-binding 4))
	(proc2 (list-ref raw-binding 5)))
    (let ((brief-context (context->brief-context context))
	  (descriptor
	   (if mouse?
	       (string-append "mouse: "
			      (keymask->string modmask)
			      (number->string keybut))
	       (string-append "key: "
			      (keymask-keycode->string modmask keybut))))
	  (proc1nm (procedure->string proc1))
	  (proc2nm (procedure->string proc2)))
      (string-append "Context " (context->string brief-context) ":: "
		     descriptor " -> " proc1nm ", " proc2nm))))

(define-public (procedure->bindings-description proc)
  "NOT TESTED: No doc."
  (apply
   string-append
   (map (lambda (bnd) (string-append (raw-binding->string bnd) "\n"))
	(lookup-procedure-bindings proc))))

;; (procedure->bindings-description describe-key)
;; (procedure->bindings-description popup-root-start)
