;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros
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

(define-module (app scwm rgb-database)
  :use-module (app scwm base))

(if (> guile-version 1.3)
    (use-modules (ice-9 popen)))

(define*-public (read-and-append-to p #:optional (l '()))
  "Read in the lines from port P and return them.
L is a the tail of the accumulating list. "
  (let ((s (read p)))
    (if (eof-object? s)
	l
	(read-and-append-to p (cons s l)))))

(define-public (rgb-colors)
  "Read in the rgb-colors database.
Requires uniq, awk, /usr/X11R6/lib/X11/rgb.txt."
  (let ((p (open-input-pipe "uniq -w12 /usr/X11R6/lib/X11/rgb.txt | awk '{print $4}'")))
    (read p)
    (let ((answer (read-and-append-to p)))
      (close-pipe p)
      (map symbol->string answer))))

