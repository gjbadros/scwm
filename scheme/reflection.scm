;;;; $Id$
;;;; Copyright (C) 1999 Greg J. Badros
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
  :use-module (app scwm listops))

(define-public (procedure-arity proc)
  "Return the arity values for PROC.
Three values are returned in a list: (num-required num-optional rest-argument-p)."
  (and (symbol? proc) (set! proc (eval proc)))
  (procedure-property proc 'arity))

(define-public (procedure-num-required-args proc)
  "Return the number of required arguments for PROC."
  (and (symbol? proc) (set! proc (eval proc)))
  (car (procedure-arity proc)))

(define-public (procedure-num-optional-args proc)
  "Return the number of optional arguments for PROC."
  (and (symbol? proc) (set! proc (eval proc)))
  (cadr (procedure-arity proc)))

(define-public (procedure-takes-rest-arg? proc)
  "Return #t iff PROC take a rest argument."
  (and (symbol? proc) (set! proc (eval proc)))
  (caddr (procedure-arity proc)))

(define-public (procedure-formals proc)
  "Return a list of the formal arguments for PROC.
Works for both primitives and procedures. If PROC was
created using an optargs *-format macro, this 
procedure will not provide much useful information.
See instead `procedure-keyword-arguments' and 
`procedure-optional-arguments'."
  (and (symbol? proc) (set! proc (eval proc)))
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
  (and (symbol? proc) (set! proc (eval proc)))
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
  "Return a list of the optargs keyword arguments for PROC.
Returns #f if PROC was not defined using keyword or optional
arguments (this includes procedures defined using standard . rest
syntax).  Otherwise returns a list such as '(foo #&optional bar).
Note that these currently do not display in their expected format"
  (and (symbol? proc) (set! proc (eval proc)))
  (procedure-property proc 'optargs-arglist))


(define-public (procedure-keyword-formals proc)
  "Returns an a-list of the optargs keyword arguments and default values for PROC."
  (and (symbol? proc) (set! proc (eval proc)))
  (let ((optargs-arglist (procedure-optargs-arglist proc)))
    (if optargs-arglist
	(let ((got-key #f))
	  (filter-map (lambda (i) (if (eq? i '#&key)
				      (set! got-key #t))
			      (and got-key 
				   (pair? i) i)) optargs-arglist))
	#f)))

(define-public (procedure-optional-formals proc)
  "Returns a list of the optional arguments for PROC."
  (and (symbol? proc) (set! proc (eval proc)))
  (let ((arglist (procedure-property proc 'arglist)))
    (if arglist
	(list-tail arglist (procedure-num-required-args proc))
	(let ((optargs-arglist (procedure-optargs-arglist proc)))
	  (if optargs-arglist
	      (let ((in-optional #f))
		(filter-map (lambda (i) (case i
					  ('#&optional
					   (set! in-optional #t))
					  ('#&key
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
  "Return a list of accessible variable names and the modules they are defined in."
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

(define-public (procedure-is-interactive? proc)
  "Return #t iff PROC can take no arguments."
  (let ((arity (procedure-arity proc)))
    (and 
     (eqv? (car arity) 0)
     (eqv? (cadr arity) 0))))

;; (procedure-apropos-with-modules "n")
(define-public (procedure-apropos-with-modules rgx)
  "Returns a list of procedures that match RGX along with defined-in modules.
The returned list contains pairs (modulesym . procsym)"
  (filter-map (lambda (p) (let ((m-p (eval (cdr p)))) 
			    (if (procedure? m-p) p #f)))
	      (apropos-internal-with-modules rgx)))

;; (procedure-apropos "n")
(define-public (procedure-apropos rgx)
  "Returns a list of procedures that match RGX.
This returns a simple list of procedure objects."
  (map (lambda (p) (eval (cdr p))) (procedure-apropos-with-modules rgx)))

;; (interactive-procedure-apropos-with-modules "n")
(define-public (interactive-procedure-apropos-with-modules rgx)
  "Returns a list of procedures that match RGX and that can take no arguments.
I.e., they are interactive procedures useful for bindings.
The returned list contains pairs (modulesym . procsym)"
  (filter-map (lambda (p) (let ((m-p (eval (cdr p)))) 
			    (if (and (procedure? m-p) 
				     (procedure-is-interactive? m-p))
				p
				#f)))
	      (apropos-internal-with-modules rgx)))

;; (interactive-procedure-apropos "n")
(define-public (interactive-procedure-apropos rgx)
  "Returns a list of interactive procedures that match RGX.
This returns a simple list of procedure objects."
  (map (lambda (p) (eval (cdr p))) (interactive-procedure-apropos-with-modules rgx)))

;(map procedure-required-formals (procedure-apropos "n"))
;(map procedure-optional-formals (procedure-apropos "n"))
;(map procedure-keyword-formals (procedure-apropos "n"))

;(procedure-source current-module)
;(procedure-formals current-module)

;(procedure? current-module)
