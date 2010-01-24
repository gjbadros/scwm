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


(define-module (app scwm wininfo)
  :use-module (srfi srfi-1)
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm listops)
  :use-module (app scwm winlist)
  :use-module (app scwm minimal)
  :use-module (ice-9 regex)
  :export (on-desk?
           on-current-desk?
           in-viewport-any-desk?
           visible?
           percent-visible
           window-geometry-string
           window-last-focus-time-string
           window-client-machine-name
           wildcard-matcher
           wildcard-match?
           define-string-matcher))



;; on-desk?


(define* (on-desk? n #:optional (win (get-window)))
  "Return #t if WIN is on desk N, else #f."
  (if win (= n (window-desk win))))

(define*-public ((on-desk-n? n) #:optional (win (get-window)))
  "Returns a function which takes WIN and returns #t if WIN is on desk N, else #f."
  (on-desk? n win))

(define* (on-current-desk? #:optional (win (get-window)))
  "Return #t if WIN is on the current desk."
  (on-desk? (current-desk) win))

(define-public (rectangle-overlap? x1 y1 w1 h1 x2 y2 w2 h2)
  "Returns #t iff the two specified rectangles overlap.
X1,Y1 and W1 x H1 are the position and size of the first rectangle.
X2,Y2 and W2 x H2 are the position and size of the second rectangle."
  (> (intersection-area x1 y1 w1 h1 x2 y2 w2 h2) 0))

(define-public (intersection-area x1 y1 w1 h1 x2 y2 w2 h2)
  "Returns the size of the intersection of two rectangles.
X1,Y1 and W1 x H1 are the position and size of the first rectangle.
X2,Y2 and W2 x H2 are the position and size of the second rectangle."
  (let ((x1-r (- (+ x1 w1) 1))
	(y1-r (- (+ y1 h1) 1))
	(x2-r (- (+ x2 w2) 1))
	(y2-r (- (+ y2 h2) 1)))
    (if (or (< x1-r x2)
	    (< x2-r x1)
	    (< y1-r y2)
	    (< y2-r y1))
	0
	(let ((xmax (max x1 x2))
	      (xmin (min x1-r x2-r))
	      (ymax (max y1 y2))
	      (ymin (min y1-r y2-r)))
	  (let ((w (+ (- xmin xmax) 1))
		(h (+ (- ymin ymax) 1)))
	    (* w h))))))

(define* (in-viewport-any-desk? #:optional (win (get-window)))
  "Return #t if WIN is in the current viewport ignoring the desk, else #f."
  (if win (apply rectangle-overlap? 
		 (append
		  (window-viewport-position win)
		  (window-frame-size win)
		  (list 0 0)
		  (map (lambda (p) (- p 1)) (display-size))))))


(define-public (windows-overlap? win win2)
  "Return #t if WIN and WIN2 overlap at all, else #f.
I.e., returns #t if the intersection of the windows' areas
is non-empty.  If either WIN or WIN2 is iconified this
will definitely return #f."
  (->bool
   (and
    (= (window-desk win) (window-desk win2))
    (not (iconified-window? win)) (not (iconified-window? win2))
    (apply rectangle-overlap?
	   (append (window-virtual-position win) (window-frame-size win)
		   (window-virtual-position win2) (window-frame-size win2))))))

(define*-public (window-overlaps-window? #:optional (win (get-window)))
  "Return a function that takes WIN2 and returns #t if it overlaps WIN."
  (lambda* (#:optional (win2 (get-window)))
    (windows-overlap? win win2)))

(define-public (list-overlapping-windows win)
  "Return a list of windows that overlap WIN.
Iconified windows that would overlap when deiconified
are not included.  See also `windows-overlap?'."
  (list-windows #:only (window-overlaps-window? win)))

(define-public (list-non-overlapping-windows win)
  "Return a list of windows that do not overlap WIN.
Iconified windows are ignored. See also `windows-overlap?'."
  (list-windows #:only (lambda (w) (and (not (windows-overlap? win w)) 
					(not (eq? w win))
					(not (iconified-window? w))))))


(define* (visible? #:optional (win (get-window)))
  "Return #t if any of WIN is currently potentially visible, else #f.
Note that this just checks if WIN is in the current viewport
and on the current desk.  It may still return #t if WIN is completely
obscured by other windows."
  (if win (and (not (iconified-window? win))
	       (on-current-desk? win)
	       (in-viewport-any-desk? win))))

(define* (percent-visible #:optional (win (get-window)))
  "Return the percent of WIN currently in the viewport as a real in [0,100].
Note that this does not consider other windows which may
obscure WIN;  it only checks what fraction of WIN would be visible
if it were on top (unobscured)."
  (if (not (on-current-desk? win))
      0 ;; none visible if on wrong desk
      (/ (* 100 
	    (apply intersection-area
		   (append
		    (window-viewport-position win)
		    (window-frame-size win)
		    (list 0 0)
		    (display-size))))
	 (apply * (window-frame-size win)))))

;; GJB:FIXME:: get rid of this -- window-center-middle returns viewport position
;; but otherwise does the same thing.
(define-public (window-center-position win)
  "Return the virtual coordinates of the center of WIN as a list of the X and Y coordinate."
  (map (lambda (wp ws) 
	 (+ wp (round/ ws 2)))
       (window-position win)
       (window-frame-size win)))

(define* (window-geometry-string #:optional (win (get-window)))
  "Return a string corresponding to the geometry specifications for WIN.
The virtual position and the frame size are used.  The resulting string
looks like \"157x133+200+306\".  If WIN is iconified, the string
returned is in parentheses."
  (if win (let ((i (iconified-window? win))
		(pos (window-virtual-position win))
		(size (window-size win)))
	    (string-append (if i "(" "")
			   (number->string (caddr size))
			   "x" (number->string (cadddr size))
			   "+" (number->string (car pos)) 
			   "+" (number->string (cadr pos))
			   (if i ")" "")))))

(define-public (time-t->seconds-ago timet)
  "Return the number of seconds that have passed since TIMET was the current time."
  (- (current-time) timet))

(define* (window-last-focus-time-string #:optional (win (get-window)))
  "Return a string corresponding to the last focus time for WIN."
  (string-append 
   (number->string (time-t->seconds-ago (window-last-focus-time win)))
   " sec"))


;; quote all regexp meta-characters, then turn \* and \? into
;; .* and . respectively.
(define-public (wildcard->regexp wildcard)
  "Return the regular expresision string corresponding to WILDCARD.
This involves quoting meta characters and replacing the wildcard
meta-characters \"*\" with \".*\" and \"?\" with \".\"."
  (regexp-substitute/global 
   #f "(\\\\)?\\\\([*?])" 
   (regexp-quote wildcard) 
   'pre
   (lambda (match)
     (if (match:start match 1)
	 (match:substring match 2)
	 (case (string-ref (match:string match) (match:start match 2))
	   ((#\*) ".*")
	   ((#\?) "."))))
   'post))

(define* (window-client-machine-name #:optional (win (get-window)))
  "Return the name of the client machine on which WIN is running."
  (if win
      (let ((prop (X-property-get win "WM_CLIENT_MACHINE")))
	(and (list? prop) (car prop)))
      #f))

(define* (wildcard-matcher wildcard #:key (full-regexp #f)
				  (regexp-options `(,regexp/icase)))
  "Return a procedure that matches WILDCARD against a window.
REGEXP-OPTIONS is passed to `make-regexp'.  If FULL-REGEXP is #t, 
the WILDCARD is considered to be a regular-expression instead of 
a shell-like wildcard.  The returned procedure takes a window
and returns #t if WILDCARD matches the title, class, or resource
of the window."
  (let ((wc-rgx (apply 
		 make-regexp 
		 (if full-regexp
		     wildcard
		     (wildcard->regexp wildcard))
		 regexp-options)))
    (lambda*
     (#:optional (win (get-window)))
     (or
      (let* ((title (window-title win))
	     (result (regexp-exec wc-rgx title)))
	(and result (= (match:end result) (string-length title))))
      (let* ((class (window-class win))
	     (result (regexp-exec wc-rgx class)))
	(and result (= (match:end result) (string-length class))))
      (let* ((resource (window-resource win))
	     (result (regexp-exec wc-rgx resource)))
	(and result (= (match:end result) (string-length resource))))))))

(define* (wildcard-match? wildcard #:optional (win (get-window))
				 #:key (full-regexp #f)
				 (regexp-options `(,regexp/icase)))
  "Returns #t if WILDCARD matches WIN (in the sense of `wildcard-matcher').
See `wildcard-matcher' for the meanings of FULL-REGEXP and REGEXP-OPTIONS."
  ((wildcard-matcher wildcard #:full-regexp full-regexp 
		     #:regexp-options regexp-options) win))


;;;; FIXMS: This file should probably be split into wininfo and
;;;; winpredicates or wintests or something like that.


(define predicate-hash (make-weak-value-hash-table 30))

;;; probably should invalidate all entries in the hash when redefining a 
;;; predicate maker

(define (undot lst)
  (if (list? lst)
      lst
      (let loop ((lst lst))
	(cond
	 ((null? lst) '())
	 ((pair? lst) (cons (car lst) (loop (cdr lst))))
	 (else (cons lst '()))))))

(defmacro define-predicate-maker (deftype decl . body)
  (let ((tag (car decl))
	(args (filter-map (lambda (x)
			    (cond 
			     ((symbol? x) x)
			     ((list? x) (car x))
			     (else #f)))
			  (undot (cdr decl)))))
    `(,deftype ,decl
       ;; Save the docstring
       ,@(if (string? (car body)) (list (car body)) '())
       (or (hash-ref predicate-hash (list ',tag ,@args))
	   (let ((pred (begin ,@body)))
	     (hash-set! predicate-hash (list ',tag ,@args) pred))))))

(define-public default-matcher-type 'wildcard)
(define-public default-matcher-case-sensitive #f)

(defmacro*-public define-string-matcher
  (name docstring-or-accessor #:optional accessor)
  (let ((docstring-list (if accessor 
			    (list docstring-or-accessor) '()))
	(accessor (if accessor accessor docstring-or-accessor)))
    `(define-predicate-maker define*-public
       (,name string #:optional 
	     (type default-matcher-type) 
	     (case-sensitive default-matcher-case-sensitive))
       ,@docstring-list
       (case type
	 ((exact) 
	  (let ((compare? (if case-sensitive string=? string-ci=?)))
	    (lambda (win)
	      (compare? (,accessor win) string))))
	 ((regexp) 
	  (let ((match-regexp 
		 (apply make-regexp string (if case-sensitive
					       '()
					       (list regexp/icase)))))
	    (lambda (win)
	      (let* ((target (,accessor win))
		     (result (regexp-exec match-regexp target)))
		(and result (= (match:end result) 
			       (string-length target)))))))
	 ((wildcard) 
	  (,name (wildcard->regexp string) 'regexp case-sensitive))))))
      

(define-string-matcher title-match?? 
  "Return a predicate that tests a window's title.  
When applied to a window, this predicate will return true if the title
matches STRING in the manner specified by the optional argument TYPE,
which may be 'exact, 'regexp, or 'wildcard. The optional
CASE-SENSITIVE argument determines whether the matching is
case-sensitive or not."
  window-title)

(define-string-matcher class-match?? 
  "Return a predicate that tests a window's resource class.  
When applied to a window, this predicate will return true if the
resource class matches STRING in the manner specified by the optional
argument TYPE, which may be 'exact, 'regexp, or 'wildcard. The
optional CASE-SENSITIVE argument determines whether the matching is
case-sensitive or not."
  window-class)

(define-string-matcher resource-match?? 
  "Return a predicate that tests a window's resource instance.  
When applied to a window, this predicate will return true if the
resource instance matches STRING in the manner specified by the
optional argument TYPE, which may be 'exact, 'regexp, or
'wildcard. The optional CASE-SENSITIVE argument determines whether the
matching is case-sensitive or not."  
  window-resource)

(define-string-matcher icon-title-match?? 
  "Return a predicate that tests a window's icon title.  
When applied to a window, this predicate will return true if the icon
title matches STRING in the manner specified by the optional argument
TYPE, which may be 'exact, 'regexp, or 'wildcard. The optional
CASE-SENSITIVE argument determines whether the matching is
case-sensitive or not."
  window-icon-title)

(define-string-matcher client-hostname-match?? 
  "Return a predicate that tests a window's client hostname.  
When applied to a window, this predicate will return true if the
client hostname matches STRING in the manner specified by the optional
argument TYPE, which may be 'exact, 'regexp, or 'wildcard. The
optional CASE-SENSITIVE argument determines whether the matching is
case-sensitive or not."  
  window-client-machine-name)

(define-public always? (lambda (w) #t))
(define-public never? (lambda (w) #f))

(define-predicate-maker 
  define-public (win-and?? . predicates)
  "Return a predicate which is the logical and of PREDICATES when applied to a window."
  (let ((predicates
	 (append-map
	  (lambda (pred)
	    (cond
	     ((object-property pred 'win-and??) 
	      => identity)
	     (else (list pred))))
	  ;;; FIXME: need a copy of uniq
	  (delete-duplicates (delq always? predicates)))))
    (internal-win-and?? predicates)))

(define-predicate-maker 
  define (internal-win-and?? predicates)
  (cond
   ((null? predicates) always?)
   ((memq never? predicates) never?)
   ((null? (cdr predicates)) (car predicates))
   (else (let ((return (lambda (w)
			 (and-map (lambda (p) (p w)) predicates))))
	   (set-object-property! return 'win-and?? predicates)
	   return))))

(define-predicate-maker 
  define-public (win-or?? . predicates)
  "Return a predicate which is the logical or of PREDICATES when applied to a window."
  (let ((predicates
	 (append-map
	  (lambda (pred)
	    (cond
	     ((object-property pred 'style-or) 
	      => identity)
	     (else (list pred))))
	  (delete-duplicates (delq never? predicates)))))
    (internal-win-or?? predicates)))

(define-predicate-maker 
  define (internal-win-or?? predicates)
  (cond
   ((null? predicates) never?)
   ((memq always? predicates) always?)
   ((null? (cdr predicates)) (car predicates))
   (else (let ((return (lambda (w)
			 (or-map (lambda (p) (p w)) predicates))))
	   (set-object-property! return 'style-or predicates)
	   return))))

(define-predicate-maker 
  define-public (win-not?? predicate)
  "Return a predicate which is the logical not of PREDICATE when applied to a window."
  (cond
   ((object-property 'style-not predicate) => identity)
   ((eq? always? predicate) never?)
   ((eq? never? predicate) always?)
   (else
    (let ((return
	   (lambda (w)
	     (not (predicate w)))))
      (set-object-property! return 'style-not predicate)
      return))))


(define-predicate-maker
  define*-public (window-match?? string #:optional 
				 (type default-matcher-type) 
				 (case-sensitive 
				  default-matcher-case-sensitive))
  ;;; FIXMS: Document me!
  (win-or?? (title-match?? string type case-sensitive) 
	    (class-match?? string type case-sensitive) 
	    (resource-match?? string type case-sensitive)))


;; from Harvey Stein; rewritten by Carl Witty
(define-public (find-window-by pred)
  "Return a window satisfying predicate PRED.
If there are multiple such windows, an unspecified one of them
will be returned."
  (let ((wlist (list-windows #:only pred)))
    (if (not (null? wlist))
	(car wlist)
	#f)))

(define-public (find-window-by-name window-name)
  "Return a window with name WINDOW-NAME.
If there are multiple such windows, an unspecified one of them
will be returned.  See also `find-window-by' and `title-match??'."
  (find-window-by (title-match?? window-name)))

(define-public (find-window-by-class-resource class resource)
  "Return a window by its CLASS and RESOURCE names (as strings).
If there are multiple such windows, an unspecified one of them
will be returned. See also `find-window-by', `class-match??' 
and `title-match??'"
  (find-window-by (win-and?? 
		   (class-match?? class)
		   (resource-match?? resource))))
