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



(define-module (app scwm style)
  :use-module (app scwm base)
  :use-module (app scwm style-options)
  :use-module (app scwm wininfo)
  :use-module (ice-9 common-list))



;; Also, this may be better placed somewhere else
;; Make make-pixmap and make-bitmap aliases for make-image;
;; Prefer and encourage make-image, though!
(define-public make-pixmap make-image)

(define-public make-bitmap make-image)

;; MSFIX: should use X Class name, or X Instance name first,
;; not wildcard matcher!  My xterm-s track the running program
;; name in the title bar, so just because I start an "xbiff"
;; from an XTERM does not mean I want that xterm, whose title
;; may be something like "<hostname> xbiff", to have an xbiff-like
;; window style! --11/08/97 gjb
;; FIXMS: OK, I guess you should be able to choose which of {title,
;; resource name, resource class} you would like to be matched, currently
;; all three are matched unconditionally. Note, however, that
;; a window titled "<hostname> xbiff" willl not match an "xbiff" wildcard,
;; although it _will_ match "*xbiff".

(define-public (window-style condition . args)
  (let ((predicate (cond
		    ((or (eq? #t condition) 
			 (and (string? condition) (string=? "*" condition))) 
		     (lambda (w) #t))
		    ((string? condition) (wildcard-matcher condition))
		    ((procedure? condition) condition)
		    (else (error "Bad window specifier for window-style.")))))
    (let* ((the-style (apply make-style args))
	   (style-proc (car the-style))
	   (hint-proc (cdr the-style))
	   (new-style-hook
	     (lambda (w) 
	       (if (predicate w)
		   (style-proc w))))
	   (new-hint-hook
	     (lambda (w) 
	       (if (predicate w)
		   (hint-proc w)))))
      (set! window-style-hooks (append window-style-hooks 
				       (list new-style-hook)))
      (set! window-hint-hooks (append window-hint-hooks 
				      (list new-hint-hook)))
      (for-each new-style-hook (list-all-windows)))))


(define-public (style-one-window w . args)
  (let ((the-style (apply make-style args)))
    ((car the-style) w)))

(define-public (make-style . args)
  (let ((style-procs-and-args 
	 (pick id (map-by-twos 
		   (lambda (key val)
		     (let ((hr (hashq-ref window-style-options key)))
		       (if hr (cons hr val) #f))) args)))
	(hint-procs-and-args 
	 (pick id (map-by-twos 
		   (lambda (key val)
		     (let ((hr (hashq-ref window-hint-options key)))
		       (if hr (cons hr val) #f))) args))))
    (cons
     (lambda (w)
       (for-each (lambda (x) 
		   ((car x) (cdr x) w)) style-procs-and-args))
     (lambda (w)
       (for-each (lambda (x) ((car x) (cdr x) w)) hint-procs-and-args)))))
    

(define (map-by-twos proc l)
  (if (or (null? l) (null? (cdr l)))
      '()
      (cons (proc (car l) (cadr l))
	    (map-by-twos proc (cddr l)))))



(define old-new-window-handler #f)
(define old-new-window-hint-handler #f)

(define window-style-hooks '())
(define window-hint-hooks '())


(set! old-new-window-handler 
      (bind-event 'new-window
		  (lambda ()
		    (if old-new-window-handler
			(old-new-window-handler))
		    (for-each (lambda (hook) 
				(hook (get-window)))
			      window-style-hooks))))

(set! old-new-window-hint-handler 
      (bind-event 'new-window-hint
		  (lambda ()
		    (if old-new-window-hint-handler
			(old-new-window-hint-handler))
		    (for-each (lambda (hook) 
				(hook (get-window))) 
			      window-hint-hooks))))


;; some useful style options
(add-window-style-option #:border-width set-border-width!)
(add-window-style-option #:background set-window-background!)
(add-window-style-option #:bg set-window-background!)
(add-window-style-option #:foreground set-window-foreground!)
(add-window-style-option #:fg set-window-foreground!)
(add-window-style-option #:focus set-window-focus!)
(add-boolean-style-option #:plain-border plain-border normal-border)
(add-window-style-option #:icon-title set-icon-title!)
(add-window-style-option #:icon-box (lambda (args w)
				      (apply set-icon-box! (append args 
								   (list w)))))
(add-boolean-style-option #:sticky-icon stick-icon unstick-icon)
(add-boolean-style-option #:start-iconified iconify deiconify)
(add-boolean-style-option #:kept-on-top keep-on-top un-keep-on-top)
(add-boolean-style-option #:sticky stick unstick)

(add-boolean-style-option #:no-titlebar hide-titlebar show-titlebar)

; clashes with maximized so make it hint-only for now
(add-window-hint-option #:mwm-buttons set-mwm-buttons!)

(add-window-style-option #:mwm-border set-mwm-border!)

;; MSFIX: did I do this right?
;; MS: would rather do this in the primitives at least for now.

;(define (set-icon-maybe-name! arg w)
;  (set-icon!
;   (if (string? arg)
;       (make-image arg)
;       arg)
;   w))
  
;(define (set-mini-icon-maybe-name! arg w)
;  (set-mini-icon! 
;   (if (string? arg)
;       (make-image arg)
;       arg)
;   w))

;(define (set-mini-icon-pixmap-name! arg w)
;  (and (string? arg)
;       (set-mini-icon! (string-append "mini-" arg ".xpm") w))
;  (display "Set it!\n"))
   

;; Use the sugared versions from above
;; MS: the primitive versions now handle strings, IMO this is more
;; consistent with other interfaces.
(add-window-style-option #:icon set-icon!)
(add-window-style-option #:mini-icon set-mini-icon!)

;; MSFIX: how do I add a new keword argument like this, to use
;; the above function?
;; GJBFIX: what you have here should be right, but I am deeply
;; supspicious of this; I think specifying the whole filename
;; for the mini-icon is much cleaner, and stuff like this in
;; scwmrc files will be confusing.
;;(add-window-style-option #:minipix set-mini-icon-pixmap-name!)

(add-window-hint-option #:random-placement set-random-placement!)
(add-window-hint-option #:smart-placement set-smart-placement!)
(add-window-hint-option #:button (lambda (n w) (set-window-button! n #t w)))
(add-window-hint-option #:no-button (lambda (n w) (set-window-button! n #f w)))
(add-window-hint-option #:hint-override set-hint-override!)
(add-window-hint-option #:decorate-transient set-decorate-transient!)
(add-window-hint-option #:mwm-decor-hint set-mwm-decor-hint!)
(add-window-hint-option #:mwm-func-hint set-mwm-func-hint!)
(add-window-hint-option #:PPosition-hint set-PPosition-hint!)
(add-window-hint-option #:OL-decor-hint set-OL-decor-hint!)

(add-window-hint-option #:start-on-desk set-start-on-desk!)
(add-window-hint-option #:skip-mapping set-skip-mapping!)
(add-window-hint-option #:lenience set-lenience!)

(add-window-style-option #:use-style 
			 (lambda (the-style w) ((car the-style) w)))
(add-window-hint-option #:use-style 			 
			(lambda (the-style w) ((cdr the-style) w)))

;; some extra style options not available in fvwm
(add-boolean-style-option #:start-lowered lower-window raise-window)
(add-boolean-style-option #:start-window-shaded window-shade un-window-shade)
(add-window-style-option #:other-proc (lambda (val w) (val w)))
(add-window-hint-option #:other-hint-proc (lambda (val w) (val w)))


