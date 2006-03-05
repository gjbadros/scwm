;;;; $Id$
;;;; Copyright (C) 1998-1999 Maciej Stachowiak
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




;;;**CONCEPT:Creating themes
;;; Currently, the best documentation on themes is the source code;
;;; however, here are a few notes.
;;; To create a theme, create a new subdirectory of a directory in
;;; `*theme-path*' (you'll probably want to add a private directory to 
;;; `*theme-path*').  This subdirectory should be named the same as the
;;; theme.  This subdirectory must contain (at least) a file named
;;; theme.scm.  This file must create a module named
;;; (app scwm theme your-theme-name), and define (in this module)
;;; a theme object named `the-theme'.  See the existing themes for
;;; examples of what you can do when building `the-theme'.

;; CRW:FIXME:MS: I assume that load-theme-image is supposed to be used
;; by a theme file for loading images.  However, it only works if
;; the theme loads all its images when the theme itself is loaded.
;; I've got an idea for another implementation:
;; There is a global variable theme-dir (exported from, say, themes.scm).
;; This is dynamically bound to the theme directory when loading
;; .../theme.scm.
;; Each theme module says (define theme-dir theme-dir); now the module
;; has a record of what directory it was loaded from.
;; load-theme-image changes to (load-theme-image theme-dir fname);
;; the first branch of the or changes to (string-append theme-dir fname).
;; Unfortunately, this conflicts with the desire to erase the unpacked
;; forms of .tar/.tar.gz/.tgz themes.  (As far as I can tell, these
;; currently stick around forever?)

(define-public (load-theme-image fname)
  (or (make-image (string-append "./" fname))
      (make-image fname)))

;; CRW:FIXME:: The following docstring is totally redundant with the function
;; name and argument list.  Can we do better?
(define*-public (make-theme name #:key (window-style (make-style #t))
			   (background-style (lambda () '())))
  "Creates a theme object with the given NAME, WINDOW-STYLE, and BACKGROUND-STYLE."
  (vector name window-style background-style))

(define-public (theme:name theme)
  (vector-ref theme 0))

(define-public (theme:window-style theme)
  (vector-ref theme 1))

(define-public (theme:background-style theme)
  (vector-ref theme 2))
