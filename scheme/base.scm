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


;;; FIXMS: disgusting hack for now to get these in the root module.

(define menu-bg-color (make-color "gray80"))
(define menu-text-color (make-color "black"))
(define menu-font (make-font "fixed"))



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


;; FIXMS: gross hack alert!
(let ((old-smfg! set-menu-foreground!))
  (set! set-menu-foreground! 
	(lambda (fg) 
	  (old-smfg! fg) 
	  (set! menu-text-color (make-color fg)))))

;; (define-public (set-menu-foreground! fg) (set-menu-colors! fg))
(let ((old-smbg! set-menu-background!))
  (set! set-menu-background!
	(lambda (bg) 
	  (old-smbg! bg)
	  (set! menu-bg-color (make-color bg)))))

;; (define-public (set-menu-background! bg) (set-menu-colors! #f bg))
;; (define-public (set-menu-stipple! st) (set-menu-colors! #f #f st))

;;(define*-public (set-window-foreground! fg #&optional (w (get-window)))
;;  (set-window-colors! fg #f w))

;;(define*-public (set-window-background! bg #&optional (w (get-window))) 
;;  (set-window-colors! #f bg w))

(define*-public (set-window-colors! #&optional (bg #f) (fg #f) (w (get-window)))
  (if bg (set-window-background! bg w))
  (if fg (set-window-foreground! fg w)))

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
  (if (or fg foreground) (set-menu-foreground! (or fg foreground)))
  (if (or bg background) (set-menu-background! (or bg background)))
  (if stipple (set-menu-stipple! stipple))
  (if (bound? font)
      (set! menu-font font))
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
      (set! color-bg (make-color color-bg)))
  (if (string? color-text)
      (set! color-text (make-color color-text)))
  (if (string? color-bg-image-side)
      (set! color-bg-image-side (make-color color-bg-image-side)))
  (make-menu list-of-menuitems image-side color-bg-image-side
	     color-bg color-text image-bg font))

(define-public (image-property key image)
  (cdr (assoc key (image-properties image))))

(define-public (font-property key font)
  (cdr (assoc key (font-properties font))))

;; for compatability
(define-public load-font make-font)

;;; ----------------------------------------------
;;; General functionality for splitting long menus
;;; ----------------------------------------------
; the max number of lines in a menu
(define-public default-max-fold-lines 30)		

(define (split-list ls max)
  (let ((le (length ls)) (tt ()) (t1 ()))
    (cond ((< le max) (list ls))
	  (#t (set! tt (list-tail ls (- max 1))) (set! t1 (cdr tt))
	      (set-cdr! tt ()) (cons ls (split-list t1 max))))))

(define*-public (fold-menu-list 
		ml #&optional (max-lines default-max-fold-lines))
  ; split the menu list into groups of max-lines
  (if (<= (length ml) max-lines) ml
      (map (lambda (lm) (menuitem "more..." #:action (menu lm)))
	   (split-list ml max-lines))))

;; Convenience function to return a precedure that will run a system
;; command.
(define-public (exe command) 
  (lambda () (execute command)))

(define-public xterm-command "xterm ")
(define-public (run-in-xterm cmd) (exe (string-append xterm-command cmd)))

;; MSFIX:
;; this is redundant w/ below
;; (defmacro-public remove-hook! (var proc)
;;   `(if (memq ,proc ,var)
;;        (set! ,var (delq! proc var))))

;; Only define if not already defined by Guile
(if (not (defined? 'remove-hook!))
	   (defmacro-public remove-hook! (hook proc)
	     `(if (memq ,proc ,hook)
		  (set! ,hook
			(delq! ,proc ,hook)))))

(defmacro-public thunk (proc)
  `(lambda args (apply ,proc args)))


;; add-hook! and remove-hook! are defined in guile's boot-9.scm
;; we still need a reset-hook! though
(defmacro-public reset-hook! (hook)
  `(set! ,hook ()))

(defmacro-public bell ()
  `(beep))

(add-hook! invalid-interaction-hook
	   (lambda () (beep) (display "inv interaction\n")))

(add-hook! cannot-grab-hook
	   (lambda () (beep) (display "cannot grab\n")))
