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



(define-module (app scwm face)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  :use-module (app scwm optargs))


(define-scwm-group face "Decorations")

(define-scwm-option *window-font* (make-font "fixed")
  "The default window titlebar font."
  #:type 'font
  #:group 'face
  #:setter (lambda (font) (set-title-font! font))
  #:getter (lambda () (title-font)))

(define*-public (title-style #&key font height justify
			     (active-up '()) 
			     (active-down '()) 
			     (inactive '()) #&allow-other-keys #&rest rest)
  "Set the title style in the current decor.
FONT is the window title font, a font object or a string.
HEIGHT is the height of the title bar, in points.
JUSTIFY is one of 'left, 'right, or 'center.
This function also takes the keyword arguments #:relief, #:solid,
#:gradient, #:h-gradient, #:v-gradient, and #:pixmap, with effects
as described under the `Face Flags' and `Face Specification Flags'
concepts (except that #:pixmap is always tiled, and #:h-gradient
and #:v-gradient are horizontal and vertical gradients).  These
extra keyword arguments can either be included in the main argument
list, or lists of these arguments can be used as the ACTIVE-UP,
ACTIVE-DOWN, or INACTIVE arguments.  For instance:
  (title-style #:solid \"red\" #:inactive (list #:solid \"green\"))
would set the title to be red in either active state, and green
in the inactive state, leaving FONT, HEIGHT, and JUSTIFY in their
current state, and returning everything else about the title
to their default state."
  (if (bound? font)
      (set-title-font! font))
  (if (bound? height) 
      (set-title-height! height))
  (if (bound? justify)
      (set-title-justify! justify))
  (act-on-face-specs set-title-face! parse-title-face-specs
		     parse-title-face-flags rest active-up
		     active-down inactive))


(define*-public (border-style #&key (active '())  
			      (inactive '()) #&allow-other-keys #&rest rest)
  "Set the border style in the current decor.
This function takes the keyword arguments #:hidden-handles, 
#:no-inset, and #:pixmap, with effects as described under the
`Face Flags' and `Face Specification Flags' concepts (except
that #:pixmap is always tiled).  These keyword arguments can
either be included in the main argument list, or lists of these
arguments can be used as the ACTIVE or INACTIVE arguments."
  (act-on-face-specs (lambda* (active #&optional ignore inactive)
			      (if (bound? inactive)
				  (set-border-face! active inactive)
				  (set-border-face! active)))
		     parse-border-face-specs parse-border-face-flags
		     rest active '() inactive))

(define*-public (set-left-button-face! button active-up #&optional
				      (active-down #f) (inactive #f))
  "Set the button face for the left-button numbered BUTTON.
E.g., if BUTTON is 1, this will set the leftmost button of your
titlebar.  See `set-button-face!' for a description of ACTIVE-UP,
ACTIVE-DOWN, and INACTIVE."
  (set-button-face! (+ (* (- button 1) 2) 1)
		    active-up active-down inactive))

(define*-public (set-right-button-face! button active-up #&optional
				       (active-down #f) (inactive #f))
  "Set the button face for the right-button numbered BUTTON.
E.g., if BUTTON is 1, this will set the rightmost button of your
titlebar.  See `set-button-face!' for a description of ACTIVE-UP,
ACTIVE-DOWN, and INACTIVE."
  (set-button-face! (+ (* (- button 1) 2) 2)
		    active-up active-down inactive))


(define*-public (button-style button #&key mwm
			      (active-up '()) 
			      (active-down '()) 
			      (inactive '()) #&allow-other-keys #&rest rest)
  "Set the button style for button number BUTTON in the current decor.
MWM sets the button's mwm flag (see `set-button-mwm-flag!'.
This function also takes the keyword arguments #:justify, #:relief,
#:vertical-justify, #:use-style-of, #:solid, #:gradient, #:h-gradient,
#:v-gradient, #:relief-pattern, #:vector, #:pixmap, and #:tiled-pixmap,
with effects as described under the `Face Flags' and `Face
Specification Flags' concepts (with the additions that #:tiled-pixmap
is an always-tiled pixmap, #:vector is a synonym for #:relief-pattern,
and #:h-gradient and #:v-gradient are horizontal and vertical
gradients).  These extra keyword arguments can either be included
in the main argument list, or lists of these arguments can be used
as the ACTIVE-UP, ACTIVE-DOWN, or INACTIVE arguments."
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
	    (setter-proc (make-face all-flags all-specs))
	    (setter-proc (make-face active-up-flags active-up-specs)
			 (make-face active-down-flags active-down-specs)
			 (make-face inactive-flags inactive-specs))))))
