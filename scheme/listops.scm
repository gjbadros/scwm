;;;; $Id$
;;;; Copyright (C) 1998-1999 Maciej Stachowiak
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



(define-module (app scwm listops))



;;;;
;;;
;;; Useful list operations. Some of these have been defined by other files that
;;; come with Guile, often with different names, or definitions.
;;;

;;; This file takes care to ensure that all definitions are both
;;; efficient and tail recursive (procedures that could obviously be
;;; optimized a bit more are so marked). It also uses traditional
;;; Scheme names where they exist, rather than the pseudo-common-lispy
;;; names of the common-list module, and

;; CRW:FIXME:MS: Above comment is incomplete!


;;(define-public (iota n) 
;;  "Generate a list of the integers from 0 to N-1 in order."
;;  (let loop ((accum '())
;;	     (n n))
;;    (if (<= n 0)
;;	accum
;;	(loop (cons n accum) (1- n)))))

(define-public (reverse-iota n) 
  "Generate a list of the integers from N-1 to 0 in order."
  (reverse! (iota n)))

(define-public (accumulate proc init l)
  "Repeatedly apply PROC to a current value and a member of L. 
The initial current value is INIT. The list is processed from left to
right. The final result is returned. Compare to `reduce'."
  (if (null? l)
      init
      (accumulate proc (proc init (car l)) (cdr l))))

;;(define-public (reduce proc init l)
;;  "Repeatedly apply PROC to a current value and a member of L.  The
;;initial current value is the first member of the list. The list is
;;processed from left to right. The final result is returned, unless the
;;list is empty, in which case INIT is returned. Compare to
;;`accumulate'."
;;  (if (null? l)
;;      init
;;      (accumulate proc (car l) (cdr l))))

;; maybe could be redefined more efficiently - at least the MIT Scheme
;; manual claims so.
;;(define-public (append-map proc first . rest)
;;  "Do the same thing as map, but collect results with `append',  not `cons'.
;;In particular, PROC is applied in succession to each member of FIRST and
;;the corresponding members of REST if any, and is expected to return a
;;list. These lists are concatenated together with `append'. In effect,
;;this is a version of map that allows the mapping function to return
;;any number of elements to be inserted into the result list, including
;;possibly none."
;;  (apply append (apply map proc first rest)))


;;(define-public (filter pred l)
;;  "Return a list of those elements of L for which PRED is true.
;;The elements appear in the result list in the same order in which they
;;appeared in L."
;;  (let loop ((accum '())
;;	     (rest (reverse l)))
;;    (cond
;;     ((null? rest) accum)
;;     ((pred (car rest)) (loop (cons (car rest) accum) (cdr rest)))
;;     (else (loop accum (cdr rest))))))

(define-public (there-exists? l pred) 
  "Return true if PRED is true for at least one elements of L, otherwise false."
  (cond
   ((null? l) #f)
   ((pred (car l)) #t)
   (else (there-exists? (cdr l) pred))))

(define-public (for-all? l pred)
  "Return true if PRED is true for all elements of L, otherwise false."
  (or
   (null? l)
   (and
    (pred (car l))
    (for-all? (cdr l) pred))))

;;(define-public (and-map proc first . rest)
;;  "Apply PROC repeatedly, returning the first false value.
;;PROC is applied to elements of FIRST and the lists comprising REST
;;much as `map' would do it. If proc never returns a false value, return
;;#t instead. If all the lists are empty, return #t."
;;  (if (null? rest)
;;      (let loop ((first first))
;;	(or (null? first)
;;	    (and 
;;	     (proc (car first))
;;	     (loop (cdr first)))))
;;      (let loop ((lists (cons first rest)))
;;	(or (there-exists? lists null?)
;;	    (and
;;	     (apply proc (map car lists))
;;	     (loop (map cdr lists)))))))


;;(define-public (or-map proc first . rest)
;;  "Apply PROC repeatedly, returning the first true value.
;;PROC is applied to elements of FIRST and the lists comprising REST
;;much as `map' would do it. If PROC never returns a true value, return
;;#f instead. If all the lists are empty, return #f."
;;  (if (null? rest)
;;      (let loop ((first first))
;;	(and (not (null? first))
;;	     (or
;;	      (proc (car first))
;;	      (loop (cdr first)))))
;;      (let loop ((lists (cons first rest)))
;;	(and (not (there-exists? lists null?))
;;	     (or
;;	      (apply proc (map car lists))
;;	      (loop (map cdr lists)))))))

;;(define-public (list-index l obj)
;;  "Return the integer position of OBJ in list L."
;;  (let loop ((n 0)
;;	     (l l))
;;    (and (not (null? l))
;;	 (if (eq? (car l) obj)
;;	     n
;;	     (loop (+ n 1) (cdr l))))))

;;; FIXME: the scwm documentation engine should have a way to lie
;;; about the procedure argument list, so handmade optional arguments,
;;; as here, can be properly documented.
;;(define-public (make-list n . init)
;;  "Return a list containing N elements equal to the optional second argument.
;;If INIT is empty indicating the optional argument was not provided,
;;use the empty list as the element."
;;  (if (pair? init) (set! init (car init)))
;;  (let loop ((answer '())
;;	     (n n))
;;    (if (<= n 0)
;;	answer
;;	(loop (cons init answer) (- n 1)))))


;; map-in-order
;;
;; Like map, but guaranteed to process the list in order.
;;
;;(define-public (map-in-order proc first . rest)
;;  "Process FIRST and the lists comprising REST as `map' would.
;;However, PROC is guaranteed to be called on these elements in the
;;order in which they appear in the lists."
;;  (if (null? rest)
;;      (let loop ((accum '())
;;		 (l first))
;;	(if (null? l)
;;	    (reverse accum)
;;	    (loop (cons (proc (car l)) accum) (cdr l))))
;;      (let loop ((accum '())
;;		 (lists (cons first rest)))
;;	(if (there-exists? lists null?)
;;	    (reverse accum)
;;	    (loop (cons (apply proc (map car lists)) accum) (map cdr lists))))))

;; e.g., (filter-map id (list 1 2 3 #f 5 (if #f 0))) => (1 2 3 5)
;;(define-public (filter-map proc first . rest)
;;  "Process FIRST and the lists comprising REST as `map' would.
;;However, do not include any false or unspecified returns from PROC in the result
;;list."
;;  (if (null? rest)
;;      (let loop ((accum '())
;;		 (l first))
;;	(cond
;;	 ((null? l) (reverse accum))
;;	 ((let ((v (proc (car l))))
;;	    (and (not (eq? v (if #f #f))) v))
;;	  => (lambda (x)
;;	       (loop (cons x accum) (cdr l))))
;;	 (else (loop accum (cdr l)))))
;;      (let loop ((accum '())
;;		 (lists (cons first rest)))
;;	(cond
;;	 ((there-exists? lists null?) (reverse accum))
;;	 ((let ((v (apply proc (map car lists)) ))
;;	    (and (not (eq? v (if #f #f))) v))
;;	  => (lambda (x)	     
;;	       (loop (cons x accum) (map cdr lists))))
;;	 (else (loop accum (map cdr lists)))))))

;; (filter-list 1 2 3 #f 5 (if #f 0))
(define-public (filter-list . args)
  "Like `list', but ignore #f and unspecified values.
See also `filter-map'."
  (filter-map identity args))                 ; id

;;(define-public (delete-duplicates l)
;;  "Return a list that has the elements of L with duplicates omitted.
;;The first instance of any alement that appears more than once is retained,
;;all other instances are removed."
;;  (let loop ((l (reverse l))
;;	     (accum '()))
;;    (cond
;;     ((null? l) accum)
;;     ((memq (car l) (cdr l)) (loop (cdr l) accum))
;;     (else (loop (cdr l) (cons (car l) accum))))))



(define-public (list-without-elem l e)
  "Return the list L with element E deleted.
Uses `eq?' for testing equality with E."
  (cond ((null? l) l)
	((eq? (car l) e) (cdr l))
	(else (cons (car l) (list-without-elem (cdr l) e)))))

(define-public (repeat . args)
  "Builds a list of repeating values.
ARGS must contain NUMBERs followed by VALUEs. Each VALUE is repeated NUMBER
times in the resulting list. E.g. (repeat 3 'a 2 'b 1 'c) => (a a a b b c)"
  (cond ((null? args) '())
	((zero? (car args)) (apply repeat (cddr args)))
	(else (cons (cadr args) (apply repeat (append! (list (1- (car args)))
						       (cdr args)))))))

;; From Springer & Friedman, Scheme and the Art of Programming
;; Program 4.14, p. 111
(define-public (flatten list-of-lists)
  "Return LIST-OF-LISTS as a single flat list."
  (cond ((null? list-of-lists) '())
	((pair? (car list-of-lists))
	 (append (flatten (car list-of-lists)) (flatten (cdr list-of-lists))))
	(else (cons (car list-of-lists) (flatten (cdr list-of-lists))))))

#!
(use-scwm-modules test-case)
(test-case "Empty"
	   (flatten '()) => '())
(test-case "Simple"
	   (flatten '(1)) => '(1))
(test-case "Linear"
	   (flatten '(1 2 3)) => '(1 2 3))
(test-case "Nested"
	   (flatten '((1 2 (3 4)) (5 6))) => '(1 2 3 4 5 6))
!#
