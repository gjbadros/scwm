;;;; $Id$
;;;; Copyright (C) 1997-1998 Maciej Stachowiak and Greg J. Badros
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



(define-module (app scwm decor)
  :use-module (app scwm style)
  :use-module (app scwm style-options))



(define-public (call-with-decor decor thunk)
  "Eval THUNK using DECOR as the current decor."
  (let* ((old-decor (current-decor))
         (swap-in-decor (lambda () 
                          (set-current-decor! decor)))
         (swap-out-decor (lambda ()
                           (set-current-decor! old-decor))))
    (dynamic-wind swap-in-decor thunk swap-out-decor)))

(defmacro-public with-decor (decor . body)
  "Evaluate BODY with DECOR set as the current decor."
  `(call-with-decor ,decor (lambda () ,@body)))

(add-window-both-option #:use-decor (lambda (d w) (set-window-decor! w d)))
