;;;; $Id$
;;;; Copyright (C) 1998 Maciej Stachowiak
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



(define-module (app scwm theme-impl)
  :use-module (app scwm style)
  :use-module (app scwm optargs))




(define-public (load-theme-image fname)
  (or (make-image (string-append "./" fname))
      (make-image fname)))

(define*-public (make-theme name #&key (window-style (make-style #t))
			   (background-style (lambda () ())))
  (vector name window-style background-style))

(define-public (theme:name theme)
  (vector-ref theme 0))

(define-public (theme:window-style theme)
  (vector-ref theme 1))

(define-public (theme:background-style theme)
  (vector-ref theme 2))
