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
  :use-module (ice-9 session))

(define-public (procedure-arity proc)
  "Return the arity values for PROC.
Three values are returned in a list: (num-required num-optional rest-argument-p)."
  (procedure-property proc 'arity))

(define-public (procedure-num-required-args proc)
  "Return the number of required arguments for PROC."
  (car (procedure-arity proc)))

(define-public (procedure-num-optional-args proc)
  "Return the number of optional arguments for PROC."
  (cadr (procedure-arity proc)))

(define-public (procedure-takes-rest-arg? proc)
  "Return #t iff PROC take a rest argument."
  (caddr (procedure-arity proc)))

(define-public (procedure-formals proc)
  "Return a list of the formal arguments for PROC.
Returns #f if PROC is a primitive."
  (let ((source (procedure-source proc)))
    (if source
	(cadr source)
	#f)))

(define-public (procedure-optargs-arglist proc)
  "Return a list of the optargs keyword arguments for PROC.
Returns #f if PROC was not defined using keyword or optional
arguments (this includes procedures defined using standard . rest
syntax).  Otherwise returns a list such as '(foo #&optional bar).
Note that these currently do not display in their expected format"
  (procedure-property proc 'optargs-arglist))

