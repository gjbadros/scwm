;;;; $Id$
;;;; Copyright (C) 1999 Greg J. Badros
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


(define-module (app scwm number-convert))

(define aval (char->integer #\A))
(define 0val (char->integer #\0))

(define-public (char-value ch)
  (let ((i (char->integer (char-upcase ch))))
    (if (>= i aval)
	(+ (- i aval) 10)
	(- i 0val))))

;; (char-value #\B)
;; (char-value #\1)

(define-public (number-in-base number base value)
  (let ((sl (string-length number)))
    (if (= sl 0) value
	(number-in-base (substring number 1) base
			(+ (* base value) 
			   (char-value (string-ref number 0)))))))

;; (number-in-base "a" 16 0)
;; (number-in-base "A" 16 0)
;; (number-in-base "1A" 16 0)
;; (number-in-base "12" 16 0)
;; (number-in-base "1d" 16 0)
;; (number-in-base "100" 16 0)

(define-public (hex number)
  (number-in-base number 16 0))

(define-public (oct number)
  (number-in-base number 8 0))

(define-public (binary number)
  (number-in-base number 2 0))
