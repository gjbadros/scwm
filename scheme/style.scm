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
  :use-module (app scwm winlist)
  :use-module (app scwm winops)
  :use-module (app scwm wininfo))



(define window-style-options (make-hash-table 40))
(define window-hint-options (make-hash-table 10))


(define-public (add-window-style-option key handler)
  (hashq-set! window-style-options key handler))

(define-public (add-boolean-style-option key t-handler f-handler)
  (hashq-set! window-style-options key (lambda (val win)
		    (if val
			(t-handler win)
			(f-handler win)))))

(define-public (add-window-hint-option key handler)
  (hashq-set! window-hint-options key handler))


(define (map-by-twos proc l)
  (if (or (null? l) (null? (cdr l)))
      #f
      (cons (proc (car l) (cadr l))
	    (map-by-twos proc (cddr l)))))

(define-public (make-style . args)
  (let ((style-procs-and-args 
	 (filter id (map-by-twos 
		     (lambda (key val)
		       (let ((hr (hashq-ref window-style-options key)))
			 (if hr (cons hr val) #f))))))
	(hint-procs-and-args 
	 (filter id (map-by-twos 
		     (lambda (key val)
		       (let ((hr (hashq-ref window-hint-options key)))
			 (if hr (cons hr val) #f)))))))
    (cons
     (lambda (w)
       (map (lambda (x) ((car x) (cdr x) w)) style-procs-and-args))
     (lambda (w)
       (map (lambda (x) ((car x) (cdr x) w)) hint-procs-and-args)))))
    

(define-public (style-one-window w . args)
  (let ((the-style (apply make-style args)))
    ((car the-style) w)))


(define old-new-window-handler #f)
(define old-new-window-hint-handler #f)

(define window-style-hooks '())
(define window-hint-hooks '())

(define (window-style condition . args)
  (let ((predicate (cond
		    ((string? condition) (make-wildcard-matcher condition))
		    ((boolean? condition) (lambda (w) condition))
		    ((procedure? condition) condition)
		    (else (error "Bad window specifier for window-style.")))))
    (let* ((the-style (apply make-style args))
	   (style-proc (car the-style))
	   (hint-proc (cdr the-style))
	   (new-style-hook
	     (lambda () 
	       (let ((w (get-window)))
		 (if (predicate w)
		     (style-proc w)))))
	   (new-hint-hook
	     (lambda () 
	       (let ((w (get-window)))
		 (if (predicate w)
		     (hint-proc w))))))
      (append! window-style-hooks new-style-hook)
      (append! window-hint-hooks new-hint-hook)
      (map new-style-hook (list-all-windows)))))

(set! old-new-window-handler 
      (bind-event 'new-window
		  (lambda ()
		    (map (lambda (hook) (hook)) window-style-hooks)
		    (if old-new-window-handler
			(old-new-window-handler)))))

(set! old-new-window-hint-handler 
      (bind-event 'new-window-hint
		  (lambda ()
		    (map (lambda (hook) (hook)) window-hint-hooks)
		    (if old-new-hint-window-handler
			(old-new-window-hint-handler)))))


;; some useful style options
(add-window-style-option #:border-width set-border-width!)
(add-window-style-option #:background set-window-background!)
(add-window-style-option #:bg set-window-background!)
(add-window-style-option #:foreground set-window-foreground!)
(add-window-style-option #:fg set-window-foreground!)
(add-boolean-style-option #:circulate-skip circulate-skip circulate-hit)
(add-boolean-style-option #:circulate-skip-icon 
			  circulate-skip-icon circulate-hit-icon)
(add-window-style-option #:focus set-window-focus!)
(add-boolean-style-option #:plain-border plain-border normal-border)
(add-window-style-option #:icon-title set-icon-title!)
(add-window-style-option #:icon-box (lambda args
				      (apply set-icon-box! args)))
(add-boolean-style-option #:sticky-icon stick-icon unstick-icon)
(add-boolean-style-option #:start-iconified iconify deiconify)
(add-boolean-style-option #:kept-on-top keep-on-top un-keep-on-top)
(add-boolean-style-option #:sticky stick unstick)

(add-boolean-style-option #:no-titlebar hide-titlebar show-titlebar)
(add-boolean-style-option #:winlist-skip winlist-skip winlist-hit)
(add-window-style-option #:mwm-buttons set-mwm-buttons!)
(add-window-style-option #:mwm-border set-mwm-border!)
(add-window-style-option #:icon set-icon!)
(add-window-style-option #:mini-icon set-mini-icon!)

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


(add-window-style-option #:use-style 
			 (lambda (the-style w) ((car the-style w))))
(add-window-hint-option #:use-style 			 
			(lambda (the-style w) ((cdr the-style w))))

;; use-decor not implemented for now

;; some extra style options not available in fvwm
(add-window-style-option #:start-maximized 
			  (lambda (arg w) 
			    (if arg
				(apply maximize (append arg (list w)))
				(unmaximize w))))
(add-boolean-style-option #:start-lowered lower-window raise-window)
(add-boolean-style-option #:start-window-shaded window-shade un-window-shade)


(add-window-style-option #:other-proc (lambda (val w) (val w)))
(add-window-hint-option #:other-hint-proc (lambda (val w) (val w)))












