;;;; 	Copyright (C) 1997 Maciej Stachowiak
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



(define-module (app scwm face)
  :use-module (app scwm base)
  :use-module (app scwm optargs))




(define*-public (title-style #&key font height justify
			     (active-up '()) 
			     (active-down '()) 
			     (inactive '()) . rest)
  (if (bound? font)
      (set-window-font! font))
  (if (bound? height) 
      (set-title-height! height))
  (if (bound? justify)
      (set-title-justify! justify))
  (act-on-face-specs set-title-face! parse-title-face-specs
		     parse-title-face-flags rest active-up
		     active-down inactive))


(define*-public (border-style #&key (active '())  
			      (inactive '()) . rest)
  (act-on-face-specs (lambda* (active &optional ignore inactive)
			      (if (bound? inactive)
				  (set-border-face! active ignore inactive)
				  (set-border-face! active)))
		     parse-border-face-specs parse-border-face-flags
		     rest active '() inactive))


(define*-public (button-style button #&key mwm
			      (active-up '()) 
			      (active-down '()) 
			      (inactive '()) . rest)
  (if (bound? mwm)
      (set-button-mwm-flag! mwm))
  (act-on-face-specs (lambda args
		       (apply set-button-face! button args))
		     parse-button-face-specs parse-button-face-flags 
		     rest active-up active-down inactive))


(define (default-handler key arg)
  `((,(keyword->symbol key) ,arg)))

(define (tiled-only-pixmap-handler key arg)
  `((pixmap (tiled ,arg))))

(define (h-gradient-handler key arg)
  `((gradient (horizontal ,@arg))))

(define (v-gradient-handler key arg)
  `((gradient (vertical ,@arg))))

(define title-face-flag-handlers
  `((#:relief . ,default-handler)))

(define title-face-spec-handlers
  `((#:solid . ,default-handler) 
    (#:gradient . ,default-handler)
    (#:h-gradient . ,h-gradient-handler) 
    (#:v-gradient . ,v-gradient-handler)
    (#:pixmap . ,tiled-only-pixmap-handler)))

(define border-face-flag-handlers
  `((#:hidden-handles . ,default-handler)
    (#:no-inset . ,default-handler)))

(define border-face-spec-handlers
  `((#:pixmap . ,tiled-only-pixmap-handler)))

(define button-face-flag-handlers
  `((#:justify . ,default-handler)
    (#:relief . ,default-handler)
    (#:vertical-justify . ,default-handler)
    (#:use-style-of . ,default-handler)))

(define button-face-spec-handlers
  `((#:solid . ,default-handler)
    (#:gradient . ,default-handler)
    (#:h-gradient . ,h-gradient-handler)
    (#:v-gradient . ,v-gradient-handler)
    (#:relief-pattern . ,default-handler)
    (#:vector . ,(lambda (key arg) (default-handler #:relief-pattern arg)))
    (#:pixmap . ,default-handler)
    (#:tiled-pixmap . ,tiled-only-pixmap-handler)))

(define (generic-parse-specs-or-flags handlers specs-or-flags)
  (define (helper specs-or-flags result)
    (if (or (null? specs-or-flags) (null? (cdr specs-or-flags)))
	result
	(let ((first (car specs-or-flags))
	      (second (cadr specs-or-flags))
	      (rest (cddr specs-or-flags)))
	  (helper rest
		  (append result (cond
				  ((assoc first handlers) 
				   => (lambda (x)
					((cdr x)
					 first second)))
				  (else ())))))))
  (helper specs-or-flags '()))

(define (parse-title-face-flags flags)
  (generic-parse-specs-or-flags title-face-flag-handlers flags))

(define (parse-title-face-specs specs)
  (generic-parse-specs-or-flags title-face-spec-handlers specs))

(define (parse-border-face-flags flags)
  (generic-parse-specs-or-flags border-face-flag-handlers flags))

(define (parse-border-face-specs specs)
  (generic-parse-specs-or-flags border-face-spec-handlers specs))

(define (parse-button-face-flags flags)
  (generic-parse-specs-or-flags button-face-flag-handlers flags))

(define (parse-button-face-specs specs)
  (generic-parse-specs-or-flags button-face-spec-handlers specs))

(define (act-on-face-specs setter-proc spec-parser flag-parser 
			   rest active-up active-down inactive)
  (let* ((all-specs (spec-parser rest))
	 (all-flags (flag-parser rest))
	 (active-up-specs (append all-specs (spec-parser active-up)))
	 (active-up-flags (append all-flags (flag-parser active-up)))
	 (active-down-specs (append all-specs (spec-parser active-down)))
	 (active-down-flags (append all-flags (flag-parser active-down)))
	 (inactive-specs (append all-specs (spec-parser inactive)))
	 (inactive-flags (append all-flags (flag-parser inactive))))
    (if (not (and (null? active-up-specs) (null? active-up-flags)
		  (null? active-down-specs) (null? active-down-flags)
		  (null? inactive-flags) (null? inactive-specs)))
	(if (and (null? active-up) (null? active-down) (null? inactive))
	    (setter-proc (peek 'face (make-face all-flags all-specs)))
	    (setter-proc (make-face active-up-flags active-up-specs)
			 (make-face active-down-flags active-down-specs)
			 (make-face inactive-flags inactive-specs))))))


