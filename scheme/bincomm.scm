;;;; $Id$
;;;; Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
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



(define-module (app scwm bincomm)
  :use-module (app scwm optargs))



;; Functions for binary conversion and I/O - these should work on
;; 32-bit and 64-bit systems, both big and little endian, but have
;; only been tested on i386-*-linux so far.

#!
(define old-integer->char integer->char)

(define (integer->char i)
  (if (< i 0) (set! i 0))
  (old-integer->char i))
!#

(define (test-bitness-and-endianness)
  (let* ((p (pipe))
	 (read-pipe (car p))
	 (write-pipe (cdr p))
	 (u (make-uniform-array #x0f0000ff 1)))
    (uniform-array-write u write-pipe)
    (close-port write-pipe)
    (let ((s
	   (let loop ((c (read-char read-pipe))
		      (s ""))
	     (if (eof-object? c)
		 s
		 (loop (read-char read-pipe)
		       (string-append s (string c)))))))
      (close-port read-pipe)
      (cons (* 8 (string-length s)) 
	    (char=? (string-ref s 0) (integer->char 255))))))

(define bitness #f)
(define endianness #f)

(let ((b&e (test-bitness-and-endianness)))
  (set! bitness (car b&e))
  (set! endianness (cdr b&e)))


;; converts a number to the binary representation of a long and
;; returns a string of it.
(define-public long->string 
  (if (= bitness 32)
      (if endianness
	  ;; 32-bit little-endian
	  (lambda (int)
	    (let* ((s (make-string 4 #\nul))
		   (intx (if (> int 2147483647)
			     (- int 4294967296)
			     int)))
	      (string-set! s 3 
			   (integer->char (ash intx -24)))
	      (string-set! s 2 
			   (integer->char (logand #x000000ff (ash intx -16))))
	      (string-set! s 1 
			   (integer->char (logand #x000000ff (ash intx -8))))
	      (string-set! s 0 
			   (integer->char (logand #x000000ff intx)))
	      s))
	  ;; 32-bit big-endian
	  (lambda (int)
	    (let* ((s (make-string 4 #\nul))
		   (intx (if (> int 2147483647)
			     (- int 4294967296)
			     int)))
	      (string-set! s 0 
			   (integer->char (ash intx -24)))
	      (string-set! s 1 
			   (integer->char (logand #x000000ff (ash intx -16))))
	      (string-set! s 2 
			   (integer->char (logand #x000000ff (ash intx -8))))
	      (string-set! s 3 
			   (integer->char (logand #x000000ff intx)))
	      s)))
      (if endianness
	  ;; 64-bit little-endian
	  (lambda (int)
	    (let* ((s (make-string 8 #\nul))
		   (intx (if (> int 9223372036854775809)
			     (- int 18446744073709551616)
			     int)))
	      (string-set! s 7 
			   (integer->char (ash intx -56)))
	      (string-set! s 6 
			   (integer->char (logand #x000000ff (ash intx -48))))
	      (string-set! s 5 
			   (integer->char (logand #x000000ff (ash intx -40))))
	      (string-set! s 4 
			   (integer->char (logand #x000000ff (ash intx -32))))
	      (string-set! s 3 
			   (integer->char (logand #x000000ff (ash intx -24))))
	      (string-set! s 2 
			   (integer->char (logand #x000000ff (ash intx -16))))
	      (string-set! s 1 
			   (integer->char (logand #x000000ff (ash intx -8))))
	      (string-set! s 0 
			   (integer->char (logand #x000000ff intx)))
	      s))
	  ;; 64-bit big-endian
	  (lambda (int)
	    (let* ((s (make-string 8 #\nul))
		   (intx (if (> int 9223372036854775809)
			     (- int 18446744073709551616)
			     int)))
	      (string-set! s 0 
			   (integer->char (ash intx -56)))
	      (string-set! s 1 
			   (integer->char (logand #x000000ff (ash intx -48))))
	      (string-set! s 2 
			   (integer->char (logand #x000000ff (ash intx -40))))
	      (string-set! s 3 
			   (integer->char (logand #x000000ff (ash intx -32))))
	      (string-set! s 4 
			   (integer->char (logand #x000000ff (ash intx -24))))
	      (string-set! s 5 
			   (integer->char (logand #x000000ff (ash intx -16))))
	      (string-set! s 6 
			   (integer->char (logand #x000000ff (ash intx -8))))
	      (string-set! s 7 
			   (integer->char (logand #x000000ff intx)))
	      s)))))

;; takes a scheme string, null terminates it, and pads with nulls at
;; the end to a multiple of sizeof(long)
(define-public pad-string-to-long
  (if (= bitness 32)
      ;; 32 bit
      (lambda (str)
	(let* ((length (* 4 (+ 1 (inexact->exact 
				  (floor (/ (string-length str) 4))))))
	       (pad (- length (string-length str))))
	  (string-append 
	   str 
	   (apply string (make-list pad #\nul)))))
      ;; 64 bit
      (lambda (str)
	(let* ((length (* 8 (+ 1 (inexact->exact 
				  (floor (/ (string-length str) 8))))))
	       (pad (- length (string-length str))))
	  (string-append 
	   str 
	   (apply string (make-list pad #\nul)))))))

;; writes the string as binary data to the port
(define*-public (binary-write str #&optional (port (current-output-port)))
  "Writes STR as binary data to PORT."
  (uniform-array-write str port))

;; reads length bytes of binary data and returns it as a string
(define*-public (binary-read length #&optional (port (current-input-port)))
  "Reads LENGTH bytes of binary data from PORT and return it as a string."
  (let* ((s (make-string length))
	 (result (uniform-array-read! s port)))
    (if (< result length)
	(throw 'too-little)
	s)))

;; reads the binary representation of a C long from the port and
;; returns it as a Scheme number.
(define*-public (binary-read-long #&optional (port (current-input-port)))
  "Reads a binary representation of a C long and return as a scheme number.
The value is read from PORT, or the current-input-port."
  (let* ((u (make-uniform-array #xfffffff 1))
	 (result (uniform-array-read! u port)))
    (if (< result 1)
	(throw 'too-little)
	(array-ref u 0))))
