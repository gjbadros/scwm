;;;; $Id$
;;;; Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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



(define-module (app scwm style-options)
  :use-module (app scwm optargs))



(define window-style-options (make-hash-table 60))


;;; Three typle are allowed: 'normal 'hint 'splicing

(define*-public (add-window-style-option key handler #&optional 
					 (type 'normal) (cumulative? #f))
  (hashq-set! window-style-options (keyword->symbol key) 
	      (vector handler type cumulative?)))

(define (make-bool-handler t-handler f-handler)
  (let ((proc (lambda (val win)
		(if val (t-handler win) (f-handler win)))))
    (set-procedure-property! proc 'name (symbol-append (procedure-name t-handler)
						       '-or-
						       (procedure-name f-handler)
						       '-handler))
    proc))

;; Convenience wrappers for backwards-compatiblity
(define*-public (add-boolean-style-option key th fh #&optional (cumulative? #f))
  (add-window-style-option key (make-bool-handler th fh) 'normal cumulative?))

(define*-public (add-window-hint-option key handler #&optional (cumulative? #f))
  (add-window-style-option key handler 'hint cumulative?))

(define*-public (add-boolean-hint-option key th fh #&optional (cumulative? #f))
  (add-window-hint-option key (make-bool-handler th fh) cumulative?))

(define*-public (add-window-both-option key handler #&optional (cumulative? #f))
  (add-window-style-option key handler 'both cumulative?))

(define*-public (add-boolean-both-option key th fh #&optional (cumulative? #f))
  (add-window-style-option key (make-bool-handler th fh) 'both cumulative?))


(define (make-property-handler property)
  (let ((proc (lambda (val win)
		(set-window-property! win property val))))
    (set-procedure-property! proc 'name (symbol-append property
						       '-style-handler))
    proc))
    

(define*-public (add-property-style-option key property #&optional 
					    (cumulative? #f))
  (add-window-style-option key (make-property-handler property) 
			   'normal cumulative?))

(define vector-first (lambda (v) (vector-ref v 0)))
(define vector-second (lambda (v) (vector-ref v 1)))
(define vector-third (lambda (v) (vector-ref v 2)))
 
;; MS:FIXME:: style options are accessed by symbol but defined by
;; keyword, is this bad?

(define-public (style-option:handler key)
  (and=> (hashq-ref window-style-options key) vector-first))
(define-public (style-option:type key)
  (and=> (hashq-ref window-style-options key) vector-second))
(define-public (style-option:cumulative? window-style-options key)
  (and=> (hashq-ref window-style-options key) vector-third))
