;; $Id$
;; By Greg J. Badros -- 5-Sept-1998
;; This may just be temporary until we start packaging
;; some other useful procs from, e.g., slib
;; --09/05/98 gjb

(define-module (app scwm sort)
  :use-module (app scwm optargs))

(re-export sort)

;; Modified from Springer & Friedman,
;; _Scheme_and_the_Art_of_Programming_

(define (merge l1 l2 compare)
  (cond ((null? l1) l2)
	((null? l2) l1)
	((not (compare (car l2) (car l1)))
	 (cons (car l1)
	       (merge (cdr l1) l2 compare)))
	(else
	 (cons (car l2)
	       (merge l1 (cdr l2) compare)))))

(define (make-groups ls compare)
  (cond
   ((null? ls) '())
   ((null? (cdr ls)) (list ls))
   (else (let ((a (car ls))
	       (gp (make-groups (cdr ls) compare)))
	   (if (compare (cadr ls) a)
	       (cons (list a) gp)
	       (cons (cons a (car gp)) (cdr gp)))))))

(define (pair-merge sublists compare)
  (cond
   ((null? sublists) '())
   ((null? (cdr sublists)) sublists)
   (else (cons (merge (car sublists) (cadr sublists) compare)
	       (pair-merge (cddr sublists) compare)))))

;; natural merge sort
(define* (sort ls #:optional (compare <))
  "Returned LS sorted according to COMPARE (defaults to ascending numerical order)."
  (if (null? ls)
      '()
      (letrec ((sortrec (lambda (gps compare)
			  (if (null? (cdr gps))
			      (car gps)
			      (sortrec (pair-merge gps compare) compare)))))
	(sortrec (make-groups ls compare) compare))))

;; (sort '(4 5 1 3 4 5 7) >)
;; (psort '(4 5 1 3 4 5 7) >)
;; (sort '(4 5 1 3 4 5 7) <)
;; (psort '(4 5 1 3 4 5 7) <)
