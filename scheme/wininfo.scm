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



(define-module (app scwm wininfo)
  :use-module (app scwm optargs)
  :use-module (ice-9 regex))



;; on-desk?


(define*-public (on-desk? n #&optional (w (get-window)))
  (if w (= n (window-desk w))))

(define*-public ((on-desk-n? n) #&optional (w (get-window)))
  (on-desk? n w))

(define*-public (on-current-desk? #&optional (w (get-window)))
  (on-desk? (current-desk) w))

(define (rectangle-overlap? x1-1 y1-1 w1 h1 x2-1 y2-1 w2 h2)
  (let ((x1-2 (+ x1-1 w1))
	(y1-2 (+ y1-1 h1))
	(x2-2 (+ x2-1 w2))
	(y2-2 (+ y2-1 h2)))
    (or (and (>= x1-1 x2-1) (<= x1-1 x2-2) (>= y1-1 y2-1) (<= y1-1 y2-2))
	(and (>= x1-1 x2-1) (<= x1-1 x2-2) (>= y1-2 y2-1) (<= y1-2 y2-2))
	(and (>= x1-2 x2-1) (<= x1-2 x2-2) (>= y1-1 y2-1) (<= y1-1 y2-2))
	(and (>= x1-2 x2-1) (<= x1-2 x2-2) (>= y1-2 y2-1) (<= y1-2 y2-2)))))


(define*-public (in-viewport-any-desk? #&optional (w (get-window)))
  (if w (apply rectangle-overlap? 
	       (append
		(window-position w)
		(window-size w)
		(list 0 0)
		(display-size)))))

(define*-public (visible? #&optional (w (get-window)))
  (if w (and (on-current-desk? w)
	     (in-viewport-any-desk? w))))

(define*-public (window-geometry-string #&optional (w (get-window)))
  (if w (let ((i (iconified? w))
	      (pos (window-position w))
	      (size (window-size w)))
	  (string-append (if i "(" "")
			 (number->string (car size))
			 "x" (number->string (cadr size))
			 "+" (number->string (car pos)) 
			 "+" (number->string (cadr pos))
			 (if i ")" "")))))


;; quote all regexp meta-characters, then turn \* and \? into .* and
;; .? respectively.
;; MSFIX: isn't a ? in a wildcard the same as ".", not ".?"? --08/02/98 gjb
(define (wildcard->regexp wildcard)
  (regexp-substitute/global 
   #f "\\\\\\*|\\\\\\?" 
   (regexp-quote wildcard) 
   'pre 
   (lambda (match) 
     (case (string-ref (match:string match) (1+ (match:start match))) 
       ((#\*) ".*")
       ((#\?) ".?"))) 
   'post))


(define*-public (window-machine-name #&optional (win (get-window)))
  "Return the name of the client machine on which WIN is running."
  (if win (xproperty->string (window-xproperty win "WM_CLIENT_MACHINE"))
      #f))

(define*-public (wildcard-matcher wildcard #&key (full-regexp #f)
				  (regexp-options `(,regexp/icase))) 
  (let ((wc-rgx (apply 
		 make-regexp 
		 (if full-regexp
		     wildcard
		     (wildcard->regexp wildcard))
		 regexp-options)))
    (lambda* (#&optional (w (get-window)))
      (or
       (let* ((title (window-title w))
	      (result (regexp-exec wc-rgx title)))
	 (and result (= (match:end result) (string-length title))))
       (let* ((class (window-class w))
	      (result (regexp-exec wc-rgx class)))
	 (and result (= (match:end result) (string-length class))))
       (let* ((resource (window-resource w))
	      (result (regexp-exec wc-rgx resource)))
	 (and result (= (match:end result) (string-length resource))))))))

(define*-public (wildcard-match? wildcard #&optional (w (get-window))
				 #&key (full-regexp #f)
				 (regexp-options `(,regexp/icase)))
  ((wildcard-matcher wildcard #:full-regexp full-regexp 
		     #:regexp-options regexp-options) w))
