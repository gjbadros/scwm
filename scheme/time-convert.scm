;;;; $Id$
;;;; time-convert.scm
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
(define-public (sec->msec sec)
  "Convert SEC seconds into an equivalent number of milliseconds.
Especially useful for `add-timer-hook!' and other timing related procedures
that take milliseconds."
  (* 1000 sec))

(define-public (msec->usec msec)
  "Convert MSEC milliseconds into an equivalent number of microseconds.
Especially useful for `usleep' and other timing related procedures
that take microseconds."
  (* 1000 msec))

(define-public (sec->usec sec)
  "Convert SEC seconds into an equivalent number of microseconds.
Especially useful for `usleep' and other timing related procedures
that take microseconds.  See also `sleep'."
  (* 1000000 sec))
