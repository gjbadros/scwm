;;;; $Id$
;;;; Copyright (C) 2010 Dale P. Smith
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


(define-module (app scwm optargs)
  #:use-module (ice-9 optargs)
  #:re-export (let-optional
	       let-optional*
	       let-keywords
	       let-keywords*
	       defmacro*
	       defmacro*-public)
  #:replace ((lambda*-interactive . lambda*)
	     (define*-interactive . define*)
	     (define*-public-interactive . define*-public)))

(define-syntax lambda*-interactive
  (syntax-rules (interactive)
    ((_ args doc (interactive) body body* ...)
     (let ((proc (lambda* args doc body body* ...)))
       (set-procedure-property! proc 'interactive #t)
       proc))
    
    ((_ args doc (interactive ispec) body body* ...)
     (let ((proc (lambda* args doc body body* ...)))
       (set-procedure-property! proc 'interactive ispec)
       proc))

    ((_ args (interactive) body body* ...)
     (let ((proc (lambda* args body body* ...)))
       (set-procedure-property! proc 'interactive #t)
       proc))
    
    ((_ args (interactive ispec) body body* ...)
     (let ((proc (lambda* args body body* ...)))
       (set-procedure-property! proc 'interactive ispec)
       proc))
    
    ((_ args body body* ...)
     (lambda* args body body* ...))))

(define-syntax define*-interactive
  (syntax-rules (interactive)
    ((_ (name args ...) doc (interactive) body body* ...)
     (begin
       (define* (name args ...) doc body body* ...)
       (set-procedure-property! name 'interactive #t)))

    ((_ (name args ...) doc (interactive ispec) body body* ...)
     (begin
       (define* (name args ...) doc body body* ...)
       (set-procedure-property! name 'interactive ispec)))

    ((_ (name args ...) (interactive) body body* ...)
     (begin
       (define* (name args ...) body body* ...)
       (set-procedure-property! name 'interactive #t)))

    ((_ (name args ...) (interactive ispec) body body* ...)
     (begin
       (define* (name args ...) body body* ...)
       (set-procedure-property! name 'interactive ispec)))

    ((_ (name args ...) body body* ...)
     (define* (name args ...) body body* ...))))

(define-syntax define*-public-interactive
  (syntax-rules ()
    ((_ (name args ...) body body* ...)
     (begin
       (define*-interactive (name args ...) body body* ...)
       (export name)))))
