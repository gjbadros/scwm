;;;; $Id$
;;;; (C) 1999 Greg J. Badros
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

(define-module (app scwm defoption)
  :use-module (ice-9 common-list)
  :use-module (app scwm optargs)
  )

;; scwm-options takes the place of the doc tools-generated
;; `user-options' variable
(if (not (feature? 'scwm-options))
    (begin
      (define-public scwm-options '())))
;;(set! scwm-options '())

(provide 'scwm-options)


;; GJB:FIXME:MS: Is this bad scheme style?
(define unspecified (if #f #f))

(defmacro-public define-scwm-option (sym default docstring . rest)
  "Define VAR to be a new scwm user option with DEFAULT as its default value.
DOCSTRING documents the option.  REST includes keyword arguments including:
#:type - one of 'boolean,
                'integer, 'percent, 'real
                'position, 'position-delta
                'time 'time-delta
                'string, 'directory, 'path, 'command, 'hostname
                'string-list
                'interactive-proc, 'proc
                'color
     (numerous of these types are not yet supported)
#:group - an option group, or a list of group, subgroup, subsubgroup, etc.
#:permit-disable - string labels for the forced-on, forced-off states (#t/#f used as value)
     (permit-disable is not yet supported -- intention is for stuff like
      opaque-move-percent which has a percent but also may be always on/off)
#:range - a cons cell (low . high), both inclusive with low <= high.
#:layout-hint - any object as a hint to the type-layout engine (e.g., 'horizontal)
     (layout hint is not yet supported)
#:favorites - a list of favorite possibilities for this variable.
#:setter - the setter procedure, if any.
#:getter - the getter procedure, if any."
  `(let ((answer (define-public ,sym ,default)))
     (apply define-scwm-option-proc (list ,sym ',sym ,docstring ,default ,@rest))
     answer))

#!
(define-scwm-option *desk-width* 2
  "The virtual desktop width, in units of physical screen size."
  #:type 'integer
  #:group 'virtual
  #:range '(1 . 10)
  #:favorites '(1 2 3 4 5)
  #:setter (lambda (x) (set-desk-size! x (cadr (desk-size))))
  #:getter (lambda () (car (desk-size))))
;; (set! scwm-options '())

;; (define sym (car scwm-options))
;; (object-properties '*desk-width*)
;; (object-properties *desk-width*) ;; WRONG!
;; (object-properties sym)

;; These two are macro syntaxes
(scwm-option-set! *desk-width* 5)
(scwm-option-get *desk-width*)

;; the rest are procs-- must quote the option symbol
(scwm-option-symset! '*desk-width* 9)
(scwm-option-symget '*desk-width*)
(scwm-option-getter *desk-width*) ;; ERROR!
(scwm-option-getter '*desk-width*)
(scwm-option-setter '*desk-width*)
(scwm-option-name '*desk-width*)
(scwm-option-documentation '*desk-width*)
(scwm-option-favorites '*desk-width*)
(scwm-option-group '*desk-width*)
(scwm-option-type '*desk-width*)
(scwm-option-range '*desk-width*)
(scwm-option-favorites '*auto-raise*)
!#

(define*-public (define-scwm-option-proc var sym docstring default #&key
		  (type #f)
		  (permit-disable #f)
		  (range #f)
		  (favorites #f)
		  (group #f)
		  (layout-hint #f)
		  (setter #f)
		  (getter #f))
  "Helper procedure for `define-scwm-option'.
See `define-scwm-option'."
  (if (not (string? docstring)) (error "Must give a docstring!"))
  (if (not type) (error "Must specify a type!"))
  (set-object-property! sym 'doc docstring)
  (set-object-property! sym 'name (symbol->string sym))
  (if setter (set-object-property! sym 'setter setter))
  (if getter (set-object-property! sym 'getter getter))
  (if range (set-object-property! sym 'range range))
  (if favorites (set-object-property! sym 'favorites favorites)
      (if (eq? type 'boolean)
	  (set-object-property! sym 'favorites (list #t #f))))
  (if permit-disable (set-object-property! sym 'permit-disable permit-disable))
  (if layout-hint (set-object-property! sym 'layout-hint layout-hint))
  (if type (set-object-property! sym 'type type))
  (if group (set-object-property! sym 'group group))
  (set! scwm-options (uniq (cons sym scwm-options)))
  ;; GJB:FIXME:MS: using unspecified, below
  (set! var (if getter unspecified default))
  (scwm-option-symset! sym default))

(define-public (scwm-option-name sym)
  "Return the name of SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'name))

(define-public (scwm-option-documentation sym)
  "Return the documentation for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'doc))

(define-public (scwm-option-setter sym)
  "Return the setter for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'setter))

(define-public (scwm-option-getter sym)
  "Return the getter for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'getter))

(define-public (scwm-option-range sym)
  "Return the range for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'range))

(define-public (scwm-option-favorites sym)
  "Return the favorites for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'favorites))

(define-public (scwm-option-group sym)
  "Return the group for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'group))

(define-public (scwm-option-permit-disable sym)
  "Return the permit-disable flag for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'permit-disable))

(define-public (scwm-option-layout-hint sym)
  "Return the permit-disable flag for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'layout-hint))

(define-public (scwm-option-type sym)
  "Return the type for SYM."
  (or (symbol? sym) (error "SYM option must be a symbol"))
  (object-property sym 'type))

(defmacro-public scwm-option-set! (var value)
  "Set option VAR to VALUE."
  `(let ((s (scwm-option-setter ',var)))
     (if s (s ,value)
	 (set! ,var ,value))))

(defmacro-public scwm-option-get (var)
  "Get option VAR's value."
  `(let ((g (scwm-option-getter ',var)))
     (if g (g)
	 ,var)))

(defmacro-public optget (var)
  "Get option VAR's value.
Shorthand for scwm-option-get."
  `(let ((g (scwm-option-getter ',var)))
     (if g (g)
	 ,var)))


(define-public (scwm-option-symset! sym value)
  "Set option SYM to VALUE."
  (let ((s (scwm-option-setter sym)))
    (if s (s value)
	;; GJB:FIXME:MS: Am I using macros strangely
	;; and incompletely thus necessitating such a strange
	;; incantation?
	(module-set! (current-module) sym value))))

(define-public (scwm-option-symget sym)
  "Get option SYM's value."
  (let ((g (scwm-option-getter sym)))
    (if g (g)
	(eval sym))))

;; (scwm-option-symset! *theme-path* (string-with-colons->path-list "foo:bar:baz"))
