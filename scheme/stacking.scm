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



(define-module (app scwm stacking)
  :use-module (app scwm optargs)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm base))



(define*-public (list-windows-above w #&key (only ()) (except()))
  "List the windows above w from bottom to top.
Returns a list in the reverse of the stacking order of the windows
above W, in other words, from the one immediately above, to the topmost
window; the ONLY and EXCEPT keyword arguments operate as is usual for
procedures that deal with the window list."
  (let ((memq-result (memq w (reverse (list-windows #:only only #:except except
						    #:by-stacking #t)))))
    (if memq-result
	(cdr memq-result)
	())))

(define*-public (list-windows-below w #&key (only ()) (except()))
  "List the windows below w from top to bottom.
Returns a list in the stacking order of the windows below in, in other
words, from the one immediately below, to the bottommost window; the
ONLY and EXCEPT keyword arguments operate as is usual for procedures
that deal with the window list."
  (let ((memq-result (memq w (list-windows #:only only #:except except
					   #:by-stacking #t))))
    (if memq-result
	(cdr memq-result)
	())))

(define-public (restack-window-below w w2)
  "Restack window W immediately below W2."
  (restack-windows (list w2 w)))

(define-public (restack-window-above w w2)
  "Restack window W immediately above W2."
  (let ((windows-above-w2 (list-windows-above w2)))
    (if (null? windows-above-w2)
	(raise-window w)
	(restack-windows (list (car windows-above-w2) w)))))

;; Deprecated as of post 0.99.6.2
(define-public lower-window-below restack-window-below)
(define-public raise-window-above restack-window-above)

(define*-public (lower-by-one #&optional (w (get-window)))
  "Lower window W below the next window down that overlaps it.
W defaults to the window context in the usual way."
  (let ((windows-below  (list-windows-below w #:only 
					    (window-overlaps-window? w))))
    (if (not (null? windows-below))
	(restack-window-below w (car windows-below)))))

(define*-public (raise-by-one #&optional (w (get-window)))
  "Raise window W above the next window up that overlaps it.
W defaults to the window context in the usual way."
  (let ((windows-above (list-windows-above w #:only 
					   (window-overlaps-window? w))))

    (if (not (null? windows-above))
	(restack-window-above w (car windows-above)))))
