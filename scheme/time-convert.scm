;;;; $Id$
;;;; flux.scm
;;;; Copyright (C) 1999 Greg J. Badros and Maciej Stachowiak
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

(define-module (app scwm time-convert))

;; (use-modules (app scwm time-convert))
(define-public (sec->usec sec)
  "Convert SEC seconds into an equivalent number of microseconds.
Especially useful for add-hook! and other timing related procedures
that take microseconds."
  (* 1000000 sec))

(define-public (ms->usec ms)
  "Convert MS milliseconds into an equivalent number of microseconds.
Especially useful for add-hook! and other timing related procedures
that take microseconds."
  (* 1000 ms))

(define-public (usec->ms usec)
  "Convert USEC microseconds into an equivalent number of milliseconds."
  (/ usec 1000))

(define-public (usec->sec usec)
  "Convert USEC microseconds into an equivalent number of seconds."
  (/ usec 1000000))
