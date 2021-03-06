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



;;;**CONCEPT:Style Option
;;; A style option is a option which can be applied to a window
;;; to control its appearance or behavior.  Each style option has
;;; a key which is used to access it.  Whenever a style option is
;;; applied to a window, the key will have an associated value for
;;; that window.  This all works by using a handler procedure to
;;; apply the style option to the window when necessary.
;;; There are three basic types of style options, 'normal, 'hint,
;;; and 'splicing.  Additionally, a style option can be both of
;;; type 'normal and 'hint which is indicated with the type 'both.
;;; A 'normal option is one that is applied after the window is
;;; created.  A 'hint option is one that can only be applied before
;;; the window is created.  A 'both option can be applied after a
;;; window is created, but saves effort by being applied before the
;;; window is created.  A 'splicing style option indicates a style
;;; option composed of other style options.  See also the concepts
;;; 'Style' and 'Style Entry'.


;;; Public for debugging purposes.
(define-public window-style-options (make-hash-table 60))


;;; Four types are allowed: 'normal 'hint 'both 'splicing
(define*-public (add-window-style-option key handler #:optional 
					 (type 'normal) (cumulative? #f))
  "Add a window style.
KEY is the key to use in 'window-style' calls.
HANDLER is the procedure to call to implement the style.
TYPE is one of 'normal, 'hint, 'both, or 'splicing.
CUMULATIVE indicates that the option can be specified multiple times
  and all instances of the option should be used."
  (hashq-set! window-style-options (keyword->symbol key) 
	      (vector handler type cumulative?)))

(define (make-bool-handler t-handler f-handler)
  (let ((proc->symbol
	 (lambda (proc)
	   (string->symbol (string-append "A" (object->string (object-address proc))))))
	(proc (lambda (val win)
		(if val (t-handler win) (f-handler win)))))
    (set-procedure-property! proc 'name (symbol-append (or (procedure-name t-handler)
							   (proc->symbol t-handler))
						       '-or-
						       (or (procedure-name f-handler)
							   (proc->symbol f-handler))
						       '-handler))
    proc))

;; Convenience wrappers for backwards-compatiblity
(define*-public (add-boolean-style-option key th fh #:optional (cumulative? #f))
  "Adds a boolean style option with type 'normal.
The option is added with key KEY and handlers TH and FH
for true/false values, respectively.  See 'add-window-style-option'."
  (add-window-style-option key (make-bool-handler th fh) 'normal cumulative?))

(define*-public (add-window-hint-option key handler #:optional (cumulative? #f))
  "Adds a window style option with type 'hint.
The option is added with key KEY and handler HANDLER.
See 'add-window-style-option'."
  (add-window-style-option key handler 'hint cumulative?))

(define*-public (add-boolean-hint-option key th fh #:optional (cumulative? #f))
  "Adds a boolean style option with type 'hint.
The option is added with key KEY and handlers TH and FH
for true/false values, respectively.  See 'add-window-style-option'."
  (add-window-hint-option key (make-bool-handler th fh) cumulative?))

(define*-public (add-window-both-option key handler #:optional (cumulative? #f))
  "Adds a window style option with type 'both.
The option is added with key KEY and handler HANDLER.
See 'add-window-style-option'."
  (add-window-style-option key handler 'both cumulative?))

(define*-public (add-boolean-both-option key th fh #:optional (cumulative? #f))
  "Adds a boolean style option with type 'both.
The option is added with key KEY and handlers TH and FH
for true/false values, respectively.  See 'add-window-style-option'."
  (add-window-style-option key (make-bool-handler th fh) 'both cumulative?))


(define (make-property-handler property)
  (let ((proc (lambda (val win)
		(set-window-property! win property val))))
    (set-procedure-property! proc 'name (symbol-append property
						       '-style-handler))
    proc))
    

(define*-public (add-property-style-option key property #:optional 
					    (cumulative? #f))
  (add-window-style-option key (make-property-handler property) 
			   'normal cumulative?))

(define vector-first  (lambda (v) (vector-ref v 0)))
(define vector-second (lambda (v) (vector-ref v 1)))
(define vector-third  (lambda (v) (vector-ref v 2)))
 
;; MS:FIXME:: style options are accessed by symbol but defined by
;; keyword, is this bad?

(define-public (style-option:handler key)
  "Return the handler associated with the option KEY.
KEY should be a symbol generated by applying 'keyword->symbol' on the
KEY used when adding this style option.
Return #f if the key is not found."
  (and=> (hashq-ref window-style-options key) vector-first))
(define-public (style-option:type key)
  "Return the type associated with the option KEY.
KEY should be a symbol generated by applying 'keyword->symbol' on the
KEY used when adding this style option.
Return #f if the key is not found."
  (and=> (hashq-ref window-style-options key) vector-second))
(define-public (style-option:cumulative? key)
  "Return whether the option associated with KEY is cumulative.
KEY should be a symbol generated by applying 'keyword->symbol' on the
KEY used when adding this style option.
Return #f if the key is not found."
  (and=> (hashq-ref window-style-options key) vector-third))
