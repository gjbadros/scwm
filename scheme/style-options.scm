;;;; 	Copyright (C) 1997 Maciej Stachowiak
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



(define-module (app scwm style-options))



(define-public window-style-options (make-hash-table 40))
(define-public window-hint-options (make-hash-table 10))

(define-public (add-window-style-option key handler)
  (hashq-set! window-style-options key handler))

(define-public (add-boolean-style-option key t-handler f-handler)
  (hashq-set! window-style-options key (lambda (val win)
		    (if val
			(t-handler win)
			(f-handler win)))))

(define-public (add-window-hint-option key handler)
  (hashq-set! window-hint-options key handler))

(define-public (add-boolean-hint-option key t-handler f-handler)
  (hashq-set! window-hint-options key (lambda (val win)
		    (if val
			(t-handler win)
			(f-handler win)))))


