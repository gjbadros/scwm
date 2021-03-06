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


(define-module (app scwm style)
  :use-module (srfi srfi-1)
  :use-module (app scwm minimal)
  :use-module (app scwm base)
  :use-module (app scwm listops)
  :use-module (app scwm style-options)
  :use-module (app scwm winops)
  :use-module (app scwm wininfo))

(export
 debug-style-handler-applications
 global-conditional-style
 style-one-window
 window-style
 window-unstyle
 clear-window-style
 make-style
 default-style-condition-handler
 make-conditional-style
 apply-style
 set-window-placement-proc!
 set-window-transient-placement-proc!)

;;;; Definitions for style type.

;;; A style is simply a list of style-entries.

(define (x-make-style style-entries)
  style-entries)

(define (style:style-entries style)
  style)

;; Set this to #t for help with debugging
(define debug-style-handler-applications #f)

;; GJB:FIXME:: this is public only for debugging
(define global-conditional-style (x-make-style '()))

(define (style-one-window win . args)
  "Add style options ARGS to the style for WIN.
See 'window-style' for more details on style options."
  (let ((new-style (apply make-conditional-style always? args)))
    (apply-style new-style win)))

(define (window-style condition . args) 
  "Specify various properties for windows matching CONDITION.
ARGS are extra agruments to the 'window-style' function.  These
style options should come in pairs of key followed by value
as separate arguments.  There are many possible style options.
See the `Window Style' section for details.  Styling windows
is cumulative, so if you apply one style to a window and then
apply a seoncd style, everything not specified in the second
style will use the values from the first style.  Hint styles
will not be applied to already existing windows by this function.
CONDITION can be a string in which case `default-style-condition-handler'
is used, or it can be a list of two elements in which case the first
is the X class name to match and the second is the X resource name to
match (both must match).
CONDITION can also be an arbitrary predicate that takes a window
and returns #t iff that window is to be considered a match."
  (let ((new-style (apply make-conditional-style condition args)))
    (for-each (lambda (w) (apply-style new-style w)) (list-all-windows))
    (set! global-conditional-style
	  (merge-styles global-conditional-style new-style))))

(define (window-unstyle . style)
  "Remove STYLE definition from list of window styles.
STYLE must be given exactly the same way as on invocation of `window-style'.
If style is present multiple times, every instance is removed."
  (for-each (lambda (cond-style)
              (set! global-conditional-style
                    (delete! cond-style
                             global-conditional-style)))
           (apply make-conditional-style style)))

(define (clear-window-style)
  "Clear all previous 'window-style' directives.
This will not affect windows already on the screen."
  (set! global-conditional-style (x-make-style '())))

(define (make-style . args)
  "Make an unconditional window style using ARGS as the style options."
  (apply make-conditional-style always? args))

;;; Produce an error giving WARNING about key KEY
(define (mcs-complain warning key)
  (error (string-append 
	  warning " style option: "
	  (objects->string key))))

;;; Produce a style corresponding to window condition CONDITION
;;;   and style options ARGS.  CONDITION should already have been
;;;   converted into a predicate.  ARGS should be a list of style
;;;   options as given by the user, i.e.
;;;       (list key1 val1 key2 val2 ... )
(define (mcs-parse-args condition args)
  (let loop ((l args)
	     (style-accum '())
	     (hint-accum '())
	     (already '()))
    (cond
     ((null? l) 
      (x-make-style (append already
			  (list
			   (make-style-entry
			    condition (reverse style-accum) 
			    (reverse hint-accum))))))
     ((null? (cdr l)) (mcs-complain "No value for" (car l)))
     (else 
      (let ((key (car l)))
	(if (not (keyword? key))
	    (mcs-complain "Invalid" key)
	    (let ((key (keyword->symbol key))
		  (value (cadr l)))
	      (case (style-option:type key)
		((normal)
		 (loop (cddr l) (acons key value style-accum) hint-accum 
		       already))
		((hint) 
		 (loop (cddr l) style-accum (acons key value hint-accum)
		       already))
		((both)
		 (loop (cddr l) (acons key value style-accum) (acons key value hint-accum)
		       already))
		((splicing) 
		 (loop (cddr l) '() '()
		       (append already
			       (list
				(make-style-entry condition (reverse style-accum)
						  (reverse hint-accum)))
			       ((style-option:handler key) condition value))))
		(else (mcs-complain "Unknown" (symbol->keyword key)))))))))))


(define default-style-condition-handler resource-match??)
;;(define-public default-style-condition-handler class-match??)
;;(define-public default-style-condition-handler window-match??)

(define (condition->predicate condition)
  (cond
   ((or (eq? #t condition) 
	(and (string? condition) (string=? "*" condition))) 
    always?)
   ((eq? #f condition) never?)
   ((string? condition) (default-style-condition-handler condition))
   ((pair? condition) (win-and?? (class-match?? (car condition)) (resource-match?? (cadr condition))))
   ((procedure? condition) condition)
   (else (error "Bad window specifier for window-style."))))

(define (make-conditional-style condition . args)
  "Make a style that only applies to windows matching CONDITION.
ARGS is style options to use in the new style.
See 'window-style' for more information."
  (simplify-style (mcs-parse-args (condition->predicate condition) args)))

(define (apply-style style win)
  "Apply STYLE to WIN.
This only applies normal and both styles from STYLE.
Hint styles are not applied."
  (apply-style-internal style win style-entry:window-options))

(define (apply-hint-style style win)
  (apply-style-internal style win style-entry:hint-options))

(define (apply-style-internal style win stype)
  (for-each 
   (lambda (se)
     (if ((style-entry:condition se) win) 
	 (for-each 
	  (lambda (o)
	    (if debug-style-handler-applications
		(begin
		  (display "applying style handler: ")
		  (write (style-option:handler (car o)))
		  (newline)))
	    ((style-option:handler (car o)) (cdr o) win))
	  (stype se))))
   (style:style-entries style)))

(add-hook! after-new-window-hook
	   (lambda (win)
	     (apply-style global-conditional-style win)))

(add-hook! before-new-window-hook
	   (lambda (win)
	     (apply-hint-style global-conditional-style win)))


;;;**CONCEPT:Style Entry
;;; A style entry is a collection of style options and an associated condition
;;; under which to apply them.  The condition is a predicate which takes a window
;;; and indicates whether to apply its style options to the window.
;;; See the concepts 'Style Option' and 'Window Style'.

(define (make-style-entry condition window-options hint-options)
  (vector condition window-options hint-options))

(define (style-entry:condition      style-entry)
  (vector-ref style-entry 0))
(define (style-entry:window-options style-entry) 
  (vector-ref style-entry 1))
(define (style-entry:hint-options   style-entry)
  (vector-ref style-entry 2))

(define (objects->string . objs)
  (with-output-to-string 
    (lambda () (map write objs))))

(define (merge-styles style1 style2)
  (x-make-style (append (style:style-entries style1)
			(style:style-entries style2))))

;; MS:FIXME:: Add more actual simplification later.
(define (simplify-style style)
  (x-make-style
   (filter-map
    (lambda (se)
      (if (and (null? (style-entry:window-options se)) 
               (null? (style-entry:hint-options   se)))
          #f
          se))
    (style:style-entries style))))
  
(add-window-style-option 
 #:use-style (lambda (condition style) 
	       (x-make-style (map 
			      (lambda (s-entry)
				(make-style-entry 
				 (win-and?? condition 
					    (style-entry:condition s-entry))
				 (style-entry:window-options s-entry)
				 (style-entry:hint-options s-entry)))
			      (style:style-entries style))))
 'splicing)

;; some useful style options
(add-window-style-option #:border-width set-border-width!)
(add-window-style-option #:background set-window-background!)
(add-window-style-option #:bg set-window-background!)
(add-window-style-option #:foreground set-window-foreground!)
(add-window-style-option #:fg set-window-foreground!)
(add-window-style-option #:highlight-background
			 set-window-highlight-background!)
(add-window-style-option #:highlight-bg set-window-highlight-background!)
(add-window-style-option #:highlight-foreground
			 set-window-highlight-foreground!)
(add-window-style-option #:highlight-fg set-window-highlight-foreground!)
(add-window-style-option #:focus set-window-focus!)
(add-boolean-style-option #:plain-border plain-border normal-border)
;;; SRL:FIXME:: BROKEN, remove this line when set-icon-title! gets fixed.
(add-window-style-option #:icon-title set-icon-title!)
(add-window-style-option #:icon-box (lambda (args w)
				      (apply set-icon-box! (append args 
								   (list w)))))
(add-boolean-style-option #:sticky-icon stick-icon unstick-icon)
;;; SRL:FIXME:: BROKEN.  Crashes scwm often.  Tries to access the icon
;;;   stuff before the icon window even gets created.  Commenting out for now.
;(add-boolean-style-option #:start-iconified iconify-window noop)
(add-boolean-style-option #:kept-on-top keep-on-top un-keep-on-top)
(add-boolean-style-option #:sticky stick-window unstick-window)

(add-boolean-both-option #:no-titlebar hide-titlebar show-titlebar)
(add-boolean-both-option #:no-side-decorations hide-side-decorations show-side-decorations)
(add-property-style-option #:squashed-titlebar 'squashed-titlebar)

; clashes with maximized so make it hint-only for now
(add-window-hint-option #:mwm-buttons set-mwm-buttons!)

(add-window-style-option #:mwm-border set-mwm-border!)

;;; SRL:FIXME:: Only works as hint, but should work as style.
;;;   Also seems to work in going from no icon to icon.
(add-window-hint-option #:show-icon set-show-icon!)
(add-window-style-option #:force-icon set-force-icon!)
;;; SRL:FIXME:: Only works as hint, but should work as style.
(add-window-hint-option #:icon set-icon!)
(add-window-style-option #:mini-icon set-mini-icon!)

;; MS:FIXME:: Figure out a better way to do this
;;; SRL:FIXME:: #:button will only show the button if the button is bound
;;;   to something.  Not intuitive.
;;; SRL:FIXME:: Propagate note about above behavior to docs somehow.
(add-window-hint-option #:button (lambda (n w) (set-window-button! n #t w))
			#t) ; cumulative
(add-window-hint-option #:no-button (lambda (n w) (set-window-button! n #f w))
			#t) ; cumulative

;;; SRL:FIXME:: Does not override border visibility hint.  Partially BROKEN.
;;;   Test with OL hints.
(add-window-hint-option #:hint-override set-hint-override!)
;;; SRL:FIXME:: Broken.  Propagated to C code but C code displays
;;;   incorrectly.
(add-window-hint-option #:decorate-transient set-decorate-transient!)
(add-window-hint-option #:mwm-decor-hint set-mwm-decor-hint!)
;;; SRL:FIXME:: NOT TESTED YET
(add-window-hint-option #:mwm-func-hint set-mwm-func-hint!)
(add-window-hint-option #:PPosition-hint set-PPosition-hint!)
;;; SRL:FIXME:: NOT TESTED YET
(add-window-hint-option #:OL-decor-hint set-OL-decor-hint!)

(add-window-hint-option #:start-on-desk set-start-on-desk!)
(add-window-hint-option #:skip-mapping set-skip-mapping!)
(add-window-hint-option #:lenience set-lenience!)


;; CRW:FIXME:GJB: Should there be a way to document window properties?
;; The only reason for the existence of the following functions is to
;; have a place to hang the documentation...

(define (set-window-placement-proc! proc win)
  "Set the 'placement-proc property of WIN to PROC.
When the window manager tries to place WIN, it will call PROC to
actually set its position.  This function must be called before the 
window is placed (i.e., from before-new-window-hook); see `window-style'
for a way to make sure this function is called at the correct time."
  (set-object-property! win 'placement-proc proc))

(define (set-window-transient-placement-proc! proc win)
  "Like `set-window-placement-proc!', but for transient windows."
  (set-object-property! win 'transient-placement-proc proc))

;; placement
(add-window-hint-option #:placement-proc set-window-placement-proc!)
(add-window-hint-option #:transient-placement-proc
			set-window-transient-placement-proc!)

;; random stuff
(add-boolean-style-option #:start-lowered lower-window raise-window)
(add-boolean-style-option #:start-window-shaded shade-window unshade-window)
(add-window-style-option #:other-proc (lambda (val w) (val w)) 'normal
			 #t) ; cumulative
(add-window-hint-option #:other-hint-proc (lambda (val w) (val w))
			#t) ; cumulative
