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



(define-module (app scwm base)
  :use-module (app scwm optargs))



;; Convenience procedures for specifying positions and sizes.
(define-public (%x x)
  (inexact->exact (truncate (/ (* x (car (display-size))) 100))))

(define-public (%y y)
  (inexact->exact (truncate (/ (* y (cadr (display-size))) 100))))

(define-public (x- x)
  (- (car (display-size)) x))

(define-public (y- y)
  (- (cadr (display-size)) y))

(define-public (%x- x)
  (inexact->exact (truncate (/ (* (- 100 x) (car (display-size))) 100))))

(define-public (%y- y)
  (inexact->exact (truncate (/ (* (- 100 y) (cadr (display-size))) 100))))

(define*-public (w%x x #&optional (w (get-window)))
  (inexact->exact (truncate (/ (* x (car (window-size w))) 100))))

(define*-public (w%y y #&optional (w (get-window)))
  (inexact->exact (truncate (/ (* y (cadr (window-size w))) 100))))

(define-public (execute command) 
  (system (string-append "exec " command " &")))

(define-public (program-exists? program-name)
  (= 0 (system (string-append "which " program-name " >/dev/null" ))))

(define-public (set-menu-foreground! fg) (set-menu-colors! fg))
(define-public (set-menu-background! bg) (set-menu-colors! #f bg))
(define-public (set-menu-stipple! st) (set-menu-colors! #f #f st))

(define*-public (set-window-foreground! fg #&optional (w (get-window)))
  (set-window-colors! fg #f w))

(define*-public (set-window-background! bg #&optional (w (get-window))) 
  (set-window-colors! #f bg w))

;; relative versions of absolute move procedures.
(define-public (move-pointer x y)
  (let ((pos (pointer-position)))
    (move-pointer-to (+ x (car pos)) (+ y (cadr pos)))))

(define-public (move-viewport x y)
  (let ((pos (viewport-position)))
    (set-viewport-position! (+ x (car pos)) (+ y (cadr pos)))))

(define*-public (menu-style #&key 
		     (fg #f) (foreground #f)
		     (bg #f) (background #f)
		     (stipple #f) font mwm mwm-style)
  (set-menu-colors! (or fg foreground) (or bg background) stipple)
  (if (bound? font)
      (set-menu-font! font))
  (if (bound? mwm)
      (set-menu-mwm-style! mwm))
  (if (bound? mwm-style)
      (set-menu-mwm-style! mwm-style)))

;; A subset of the real title-style which is here so people don't have
;; to load all of face.scm to get at it; will probably go away in the
;; future.

(define*-public (simple-title-style #&key font height justify)
  (if (bound? font)
      (set-window-font! font))
  (if (bound? height) 
      (set-title-height! height))
  (if (bound? justify)
      (set-title-justify! justify)))

(define-public menu-separator
  (make-menuitem "" #f))

(define-public menu-title menu-separator)

;; should this be public?
(define (hotkeys-from-name label)
  (let ((char-list (string->list label))
	(return-key-char-list ())
	(return-label-char-list ()))
    (while (not (null? char-list))
	   (if (equal? (car char-list) #\&)
	       (set! return-key-char-list (cons (cadr char-list) 
						return-key-char-list))
	       (set! return-label-char-list (cons (car char-list)
						  return-label-char-list)))
	   (set! char-list (cdr char-list)))
    (list (list->string (reverse return-label-char-list))
	  (list->string (reverse return-key-char-list)))))

(define*-public (menuitem label #&key image-above image-left
			  extra-label action hover-action unhover-action
			  hotkey-prefs)
  (if (or (bound? hotkey-prefs)
	  (string=? label ""))
      ()
      (let ((result (hotkeys-from-name label)))
	(set! label (car result))
	(set! hotkey-prefs (cadr result))))
  (if (string? image-above)		;; permit "foo.xpm" to mean (make-image "foo.xpm")
      (set! image-above (make-image image-above)))
  (if (string? image-left)
      (set! image-left (make-image image-left)))
  (if (string? action)			;; permit "xterm" to mean (execute "xterm")
      (let ((program-name action))
	(set! action (lambda () (execute program-name)))))
  (make-menuitem label action extra-label image-above image-left
		  hover-action unhover-action hotkey-prefs))

(define-public menu-bg-color (load-color "gray80"))
(define-public menu-text-color (load-color "black"))
(define-public menu-font (load-font "fixed"))

(define*-public (menu list-of-menuitems #&key
		      image-side 
		      (color-bg-image-side 'menu-bg-color)
		      (image-bg #f)
		      (color-text 'menu-text-color)
		      (color-bg 'menu-bg-color)
		      (font 'menu-font))
  (if (string? image-side)
      (set! image-side (make-image image-side)))
  (if (string? color-bg)
      (set! color-bg (load-color color-bg)))
  (if (string? color-text)
      (set! color-text (load-color color-text)))
  (if (string? color-bg-image-side)
      (set! color-bg-image-side (load-color color-bg-image-side)))
  (make-menu list-of-menuitems image-side color-bg-image-side
	     color-bg color-text image-bg font))

(define (image-property key image)
  (cdr (assoc key (image-properties image))))
