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
      (let ((source (procedure-source proc)))
	(if source
	    (cadr source)
	    #f))))

(define-public (procedure-required-formals proc)
  "Return a list of the required formal arguments for PROC.
Returns #f if PROC is a primitive.  This will not include
any \"lambda*\" formals generated using optargs. See also
`procedure-keyword-arguments' and `procedure-optional-arguments'."
  (and (symbol? proc) (set! proc (eval proc)))
  (let ((formals (procedure-formals proc)))
    (if (symbol? formals)
	(if (string-match "^lambda\\*:" (symbol->string formals))
	    '()
	    formals)
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
	      #f)))))

