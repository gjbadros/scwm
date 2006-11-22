;;;; $Id$	-*- scwm -*-
;;;; Copyright (C) 1998 Robert Bihlmeyer
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


(define-module (app scwm advice))



; FIXME: around does not work,
; neither does advising lamdas with variable number of arguemnts
(defmacro-public defadvice (function param . body)
  "Advices FUNCTION with BODY.
\\(defadvice FUNCTION \\(CLASS NAME\\)\\)
FUNCTION is the function to advice.
If CLASS is 'before, execute BODY before the function proper.
If CLASS is 'after, execute BODY after the function proper.
If CLASS is 'around, BODY is executed and must call the function proper.
NAME is the name of this piece of advice.
BODY is the piece of advice itself.
Advice is active immediately.

This is inspired by the Emacs-Lisp function of the same name but does support
only a minimal subset of its features."
  (if (not (defined? function))
      (error "Function not defined:" function))
  (let ((source (procedure-source (eval function))))
    (if (not (eq? (car source) 'lambda))
	(error "Does not evaluate to a lambda expression:" function))
    (let ((orig (string->symbol (string-append "ad-Orig-"
					       (symbol->string function))))
	  (args (cadr source)))
      (list 'begin
	    (if (not (defined? orig))
		`(define ,orig ,function)
		'())
	    (case (car param)
	      ((before) `(define ,function
			   (lambda ,args
			     ,@body ,(cons (eval function)
					   args))))
	      ((after) `(define ,function
			  (lambda ,args ,(cons (eval function) args) ,@body)))
	      ((around) `(define ,function
			   (lambda () (let ((ad-do-it FIXME)) ,@body))))
	      (else (error "CLASS must be 'before, 'around, or 'after:"
			   (car param))))))))

(defmacro-public ad-unadvise (function)
  "Removes all advice defined on FUNCTION."
  (let ((orig (string->symbol (string-append "ad-Orig-"
					      (symbol->string function)))))
    (if (not (defined? orig))
	(error "Function not advised:" function))
    `(begin
       (define ,function ,orig)
       (undefine ,orig))))
