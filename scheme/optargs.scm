;;;; $Id$
;;;; Copyright (C) 1997-1998 Maciej Stachowiak
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



(define-module (app scwm optargs))



;;; {Optional Arguments}
;;;
;;; The C interface for creating Guile procedures has a very handy
;;; "optional argument" feature. This module attempts to provide
;;; similar functionality for procedures defined in Scheme with
;;; a convenient and attractive syntax.
;;;
;;; exported macros and variables are:
;;;   bound?
;;;   let-optional
;;;   let-optional*
;;;   let-keywords
;;;   let-keywords*
;;;   lambda*
;;;   define*
;;;   define*-public   
;;;

;; bound? var
;;   Checks if a variable is bound in the current environment.
;;
;; defined? doesn't quite cut it as it stands, since it only
;; cheks bindings in the top-level environment, not those in
;; local scope only.
;;

(defmacro-public bound? (var)
  `(catch 'misc-error
	  (lambda () 
	    ,var 
	    (not (eq? ,var ,(variable-ref 
			    (make-undefined-variable)))))
	  (lambda args #f)))



;; let-optional rest-arg (binding ...) . body
;; let-optional* rest-arg (binding ...) . body
;;   macros used to bind optional arguments
;;
;; These two macros give you an optional argument interface that
;; is very "Schemey" and introduces no fancy syntax. They are
;; compatible with the scsh macros of the same name, but are slightly
;; extended. Each of binding may be of one of the forms <var> or
;; (<var> <default-value>). rest-arg should be the rest-argument of
;; the procedures these are used from. The items in rest-arg are
;; sequentially bound to the variable namess are given. When rest-arg
;; runs out, the remaining vars are bound either to the default values
;; or left unbound if no default value was specified. rest-arg remains
;; bound to whatever may have been left of rest-arg.
;;

(defmacro-public let-optional (REST-ARG BINDINGS . BODY)
  (let-optional-template REST-ARG BINDINGS BODY 'let))

(defmacro-public let-optional* (REST-ARG BINDINGS . BODY)
  (let-optional-template REST-ARG BINDINGS BODY 'let*))


;; let-keywords rest-arg (binding ...) . body
;; let-keywords* rest-arg (binding ...) . body
;;   macros used to bind keyword arguments
;;
;; These macros pick out keyword arguments from rest-arg, but do not
;; modify it. This is consistent at least with Common Lisp, which
;; duplicates keyword args in the rest arg. More explanation of what
;; keyword arguments in a lambda list look like can be found below in
;; the documentation for lambda*.  Bindings can have the same form as
;; for let-optional.  
;;

(defmacro-public let-keywords (REST-ARG BINDINGS . BODY)
  (let-keywords-template REST-ARG BINDINGS BODY 'let))

(defmacro-public let-keywords* (REST-ARG BINDINGS . BODY)
  (let-keywords-template REST-ARG BINDINGS BODY 'let*))



;; some utility procedures for implementing the various let-forms.

(define (let-o-k-template REST-ARG BINDINGS BODY let-type proc)
  (let ((bindings (map (lambda (x) 
		      (if (list? x)
			  x
			  (list x (variable-ref
				   (make-undefined-variable)))))
		    BINDINGS)))
    `(,let-type ,(map proc bindings) ,@BODY)))
  
(define (let-optional-template REST-ARG BINDINGS BODY let-type)
  (let-o-k-template REST-ARG BINDINGS BODY let-type
		    (lambda (optional) 
		      `(,(car optional) 
			(cond
			 ((not (null? ,REST-ARG))
			  (let ((result (car ,REST-ARG)))
			    ,(list 'set! REST-ARG
				   `(cdr ,REST-ARG))
			    result))
			 (else
			  ,(cadr optional)))))))

(define (let-keywords-template REST-ARG BINDINGS BODY let-type)
  (let-o-k-template REST-ARG BINDINGS BODY let-type
		    (lambda (key)
		      `(,(car key)
			(cond
			 ((memq ,(symbol->keyword (car key)) ,REST-ARG) 
			  => cadr)
			 (else 
			  ,(cadr key)))))))



;;   reader extensions for #&key #&optional
;; These need to be quoted in normal code, but need not be in
;; an extended lambda-list provided by lambda*, define*, or 
;; define*-public (see below). In other words, they act sort of like 
;; symbols, except they aren't. They're being temporarily used until
;; #!optional and #!key are available.

(define the-optional-value 
  ((record-constructor (make-record-type 
			'optional '() (lambda (o p)
					(display "#&optional" p))))))

(define the-key-value 
  ((record-constructor (make-record-type 
			'key '() (lambda (o p)
					(display "#&key" p))))))

(read-hash-extend #\& (lambda (c port)
			(case (read port)
			  ((optional) the-optional-value)
			  ((key) the-key-value)
			  (else (error "Bad #& value.")))))



;; lambda* args . body
;;   lambda extended for optional and keyword arguments
;;   
;; lambda* creates a procedure that takes optional arguments. These
;; are specified by putting them inside brackets at the end of the
;; paramater list, but before any dotted rest argument. For example,
;;   (lambda* (a b #&optional c d . e) '())
;; creates a procedure with fixed arguments a and b, optional arguments c
;; and d, and rest argument e. If the optional arguments are omitted
;; in a call, the variables for them are unbound in the procedure. This
;; can be checked with the bound? macro.
;;
;; lambda* can also take keyword arguments. For example, a procedure
;; defined like this:
;;   (lambda* (#&key xyzzy larch) '())
;; can be called with any of the argument lists (#:xyzzy 11)
;; (#:larch 13) (#:larch 42 #:xyzzy 19) (). Whichever arguments
;; are given as keywords are bound to values.
;;
;; Optional and keyword arguments can also be given default values
;; which they take on when they are not present in a call, by giving a
;; two-item list in place of an optional argument, for example in:
;;   (lambda* (foo #&optional (bar 42) #&key (baz 73)) (list foo bar baz)) 
;; foo is a fixed argument, bar is an optional argument with default
;; value 42, and baz is a keyword argument with default value 73.
;; Default value expressions are not evaluated unless they are needed
;; and until the procedure is called.  
;;


(defmacro-public lambda* (ARGLIST . BODY)
  (if (pair? ARGLIST)
      (let ((rest-arg (cdr (last-pair ARGLIST))))
	;; blatant hack because list ops are unhappy with dotted lists
	(if (not (null? rest-arg))
	    (set-cdr! (last-pair ARGLIST) '())
	    (set! rest-arg #f))
	(let ((opt-pos (memq '#&optional ARGLIST))
	      (key-pos (memq '#&key ARGLIST)))
	  (cond
	   ((or opt-pos key-pos)
	    (if (and (pair? key-pos) (memq '#&optional key-pos))
		(error "#&optional arguments must be before #&key arguments."))
	    (let* ((non-optional-args 
		    (cond
		     (opt-pos (reverse (cdr (memq '#&optional
						  (reverse ARGLIST)))))
		     (key-pos (reverse (cdr (memq '#&key 
						  (reverse ARGLIST)))))
		     (else ARGLIST)))
		   (optionals 
		    (cond
		   ((and opt-pos key-pos) 
		    (reverse (cdr (memq '#&key (reverse (cdr opt-pos))))))
		   (opt-pos (cdr opt-pos))
		   (else '())))
		   (keys
		    (cond (key-pos (cdr key-pos)) (else '()))))
	      (let ((rest-gensym (or rest-arg (gensym))))
		`(lambda (,@non-optional-args . ,rest-gensym)
		   (let-optional* ,rest-gensym
		    ,optionals
		    (let-keywords* ,rest-gensym
		     ,keys
		     ,@(if (and (not rest-arg) (not key-pos))
			   `((if (not (null? ,rest-gensym))
				 (error "Too many arguments.")))
			   '())
		     ,@BODY))))))
	   (else `(lambda (,@ARGLIST . ,(if rest-arg rest-arg '())) 
		    ,@BODY)))))
      `(lambda ,ARGLIST ,@BODY)))



;; define* args . body
;; define*-public args . body
;;   define and define-public extended for optional and keyword arguments
;;
;; define* and define*-public support optional arguments with
;; a similar syntax to lambda*. They also support arbitrary-depth
;; currying, just like Guile's define. Some examples:
;;   (define* (x y #&optional a (z 3) #&key w . u) (display (list y z u)))
;; defines a procedure x with a fixed argument y, an optional agument
;; a, another optional argument z with default value 3, a keyword argument w,
;; and a rest argument u.
;;   (define-public* ((foo #&optional bar) #&optional baz) '())
;; This illustrates currying. A procedure foo is defined, which,
;; when called with an optional argument bar, returns a procedure that
;; takes an optional argument baz. 
;;

(defmacro-public define* (ARGLIST . BODY)
  (define*-guts 'define ARGLIST BODY))

(defmacro-public define*-public (ARGLIST . BODY)
  (define*-guts 'define-public ARGLIST BODY))


;; The guts of define* and define*-public.
(define (define*-guts DT ARGLIST BODY)
  (define (nest-lambda*s arglists)
    (if (null? arglists)
	BODY
	`((lambda* ,(car arglists) ,@(nest-lambda*s (cdr arglists))))))
  (define (define*-guts-helper ARGLIST arglists)
    (let ((first (car ARGLIST))
	  (al (cons (cdr ARGLIST) arglists)))
      (if (symbol? first)
	  `(,DT ,first ,@(nest-lambda*s al))
	  (define*-guts-helper first al))))
  (if (symbol? ARGLIST)
      `(,DT ,ARGLIST ,@BODY)
      (define*-guts-helper ARGLIST '())))


