;;;; $Id$
;;;; Copyright (C) 1997-1998 Maciej Stachowiak and Greg J. Badros
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
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (ice-9 regex))



;; on-desk?


(define*-public (on-desk? n #&optional (win (get-window)))
  "Return #t if WIN is on desk N, else #f."
  (if win (= n (window-desk win))))

(define*-public ((on-desk-n? n) #&optional (win (get-window)))
  (on-desk? n win))

(define*-public (on-current-desk? #&optional (win (get-window)))
  "Return #t if WIN is on the current desk."
  (on-desk? (current-desk) win))

(define (rectangle-overlap? x1-1 y1-1 w1 h1 x2-1 y2-1 w2 h2)
  (> (intersection-area x1-1 y1-1 w1 h1 x2-1 y2-1 w2 h2) 0))

(define (intersection-area x1-1 y1-1 w1 h1 x2-1 y2-1 w2 h2)
  (let ((x1-2 (- (+ x1-1 w1) 1))
	(y1-2 (- (+ y1-1 h1) 1))
	(x2-2 (- (+ x2-1 w2) 1))
	(y2-2 (- (+ y2-1 h2) 1)))
    (if (or (< x1-2 x2-1)
	    (< x2-2 x1-1)
	    (< y1-2 y2-1)
	    (< y2-2 y1-1)) 
	0
	(let ((x1 (max x1-1 x2-1))
	      (x2 (min x1-2 x2-2))
	      (y1 (max y1-1 y2-1))
	      (y2 (min y1-2 y2-2)))
	  (let ((w (+ (- x2 x1) 1))
		(h (+ (- y2 y1) 1)))
	    (* w h))))))

(define*-public (in-viewport-any-desk? #&optional (win (get-window)))
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
is non-empty."
  (and
   (= (window-desk win) (window-desk win2))
   (apply rectangle-overlap?
	  (append (window-position win) (window-frame-size win)
		  (window-position win2) (window-frame-size win2)))))

;;; MSFIX: Why is this separate instead of having the above
;;; have optional arguments.
(define*-public ((window-overlaps-window? #&optional (win (get-window))) 
		 #&optional (win2 (get-window)))
  (windows-overlap? win win2))

(define*-public (visible? #&optional (win (get-window)))
  "Return #t if any of WIN is currently potentially visible, else #f.
Note that this just checks if WIN is in the current viewport
and on the current desk.  It may still return #t if WIN is completely
obscured by other windows."
  (if win (and (on-current-desk? win)
	       (in-viewport-any-desk? win))))

(define*-public (percent-visible #&optional (win (get-window)))
  "Return the percent of WIN currently in the viewport as a real in [0,100].
Note that this does not discount for other windows which may
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
	 (apply * (window-frame-size win))))

(define*-public (window-geometry-string #&optional (win (get-window)))
  "Return a string corresponding to the geometry specifications for WIN.
The virtual position and the frame size are used.  The resulting string
looks like, e.g., FIXGJB."
  (if win (let ((i (iconified? win))
		(pos (window-position win))
		(size (window-size win)))
	    (string-append (if i "(" "")
			   (number->string (caddr size))
			   "x" (number->string (cadddr size))
			   "+" (number->string (car pos)) 
			   "+" (number->string (cadr pos))
			   (if i ")" "")))))


;; quote all regexp meta-characters, then turn \* and \? into
;; .* and . respectively.
(define-public (wildcard->regexp wildcard)
  "Return the string real regular expresision corresponding to WILDCARD.
This involves quoting meta characters and replacing the wildcard
meta-characters \"*\" with \".*\" and \"?\" with \".\"."
  (regexp-substitute/global 
   #f "\\\\\\*|\\\\\\?" 
   (regexp-quote wildcard) 
   'pre 
   (lambda (match) 
     (case (string-ref (match:string match) (1+ (match:start match))) 
       ((#\*) ".*")
       ((#\?) "."))) ;; changed from .? --08/04/98 gjb
   'post))


(define*-public (window-client-machine-name #&optional (win (get-window)))
  "Return the name of the client machine on which WIN is running."
  (if win
      (let ((prop (X-property-get win "WM_CLIENT_MACHINE")))
	(and (list? prop) (car prop)))
      #f))

(define*-public (wildcard-matcher wildcard #&key (full-regexp #f)
				  (regexp-options `(,regexp/icase)))
  "Return a procedure that matches WILDCARD using the supplied options.
If FULL-REGEXP is #t, the WILDCARD is considered to be a regular-expression
instead of a shell-like wildcard."
  (let ((wc-rgx (apply 
		 make-regexp 
		 (if full-regexp
		     wildcard
		     (wildcard->regexp wildcard))
		 regexp-options)))
    (lambda*
     (#&optional (win (get-window)))
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

(define*-public (wildcard-match? wildcard #&optional (win (get-window))
				 #&key (full-regexp #f)
				 (regexp-options `(,regexp/icase)))
  ((wildcard-matcher wildcard #:full-regexp full-regexp 
		     #:regexp-options regexp-options) win))
