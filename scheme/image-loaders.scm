;;;; $Id$
;;;; Copyright (C) 1998 Maciej Stachowiak and Greg J. Badros
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



(define-module (app scwm image-loaders))



;; Uses the "convert" tool from the ImageMagick package to try to
;; convert the file to an xpm, then attempts to load it as such.
(define-public (ImageMagick-loader fname)
  (let ((t (string-append (tmpnam) ".xpm")))
    (catch #t
	   (lambda () (system (string-append "convert " fname " " t)))
	   (lambda args #f))
    (load-xpm t)))

;; Uses the "anytopnm" and "ppmtoxpm" tools from the netpbm package to
;; try to convert the file to an xpm, then attempts to load it as
;; such.
(define-public (netpbm-loader fname)
  (let ((t (string-append (tmpnam) ".xpm")))
    (catch #t
	   (lambda () 
	     (system (string-append "anytopnm " fname " | ppmtoxpm > " t)))
	   (lambda args #f))
    (load-xpm t)))

;; Try all available loaders.
(define-public (try-everything-loader fname)
  (cond
   ((ImageMagick-loader fname) => id)
   (else (netpbm-loader fname))))

;; Just a convenience wrapper to make the try-everything-loader
;; the default.
(define-public (support-image-conversion)
  (register-image-loader "default" try-everything-loader))
