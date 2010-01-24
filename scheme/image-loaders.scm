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



;; CRW:FIXME:: We should delete these temporary files at some point...

(define-public (ImageMagick-loader fname)
  "Tries to load an arbitrary image using ImageMagick's `convert'.
Uses `convert' to try to convert the file to an xpm, then
attempts to load it as such."
  (let ((t (string-append (tmpnam) ".xpm")))
    (catch #t
	   (lambda () (system (string-append "convert " fname " " t)))
	   (lambda args #f))
    (load-xpm t)))

(define-public (netpbm-loader fname)
  "Tries to load an arbitrary image using the netpbm packge.
Uses `anytoppm' and `ppmtoxpm' to try to convert the file to an xpm,
then attempts to load it as such."
  (let ((t (string-append (tmpnam) ".xpm")))
    (catch #t
	   (lambda () 
	     (system (string-append "anytopnm " fname " | ppmtoxpm > " t)))
	   (lambda args #f))
    (load-xpm t)))

(define-public (try-everything-loader fname)
  "Tries to load an arbitrary image, using any available loader."
  (cond
   ((ImageMagick-loader fname) => identity)
   (else (netpbm-loader fname))))

(define-public (support-image-conversion)
  "Set things up to try to load arbitrary images.
Works by setting `try-everything-loader' as the image loader for
unknown extensions.  Generally not needed with ImLib."
  (if (not (defined? 'load-imlib-image))
      (register-image-loader "default" try-everything-loader)))

