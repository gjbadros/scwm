;;;; $Id$
;;;; Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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



(define-module (app scwm menus-extras)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  :use-module (app scwm sort)  ;; GJB:FIXME:G1.3.2: not needed in guile-1.3.2
  :use-module (app scwm optargs))




(define-public (menu-max-fold-lines)
  "Return an approximation of the number of menuitems that will fit vertically on screen."
  (let* ((menu-font-property menu-font)
	 (menu-font (if (symbol? menu-font-property) (eval menu-font-property) menu-font-property))
	 (menu-font-height (assoc-ref (font-properties menu-font) 'height)))
    (round/ (cadr (display-size)) (+ 7 menu-font-height))))

(define-public (sorted-by-car-string l)
  "Sort the elements of list L based on the string value of their `car'."
  (sort l (lambda (a b) (string>? (car a) (car b)))))

;;; ----------------------------------------------
;;; General functionality for splitting long menus
;;; ----------------------------------------------
;;; the max number of lines in a menu
(define-scwm-option *menu-max-fold-lines* 30
  "The default number of items that menus are split into by `fold-menu-list'"
  #:type 'integer
  #:group 'menu
  #:range '(5 . 100)
  #:favorites '(10 20 25 30 35 40 50))

(define (split-list ls max)
  (let ((le (length ls)) (tt ()) (t1 ()))
    (cond ((< le max) (list ls))
	  (#t (set! tt (list-tail ls (- max 1))) (set! t1 (cdr tt))
	      (set-cdr! tt ()) (cons ls (split-list t1 max))))))

(define*-public (fold-menu-list
		ml #&optional (max-lines (optget *menu-max-fold-lines*)))
  "Split ML into chained menus of no more than MAX-LINES items.
ML is a list of menuitem objects. MAX-LINES is a number, which
defaults to `default-menu-max-fold-lines'."
  (if (<= (length ml) max-lines) ml
      (map (lambda (lm) (menuitem "more..." #:action (menu lm)))
	   (split-list ml max-lines))))

(define*-public (split-list-by-group ls #&optional (rest #f))
  "Split the a-list LS into groups according to the car of each of its cells."
  (cond ((null? ls) rest)
	((and rest (string=? (caar ls) (caar rest)))
	 (begin
	   (set-cdr! (car rest) (cons (cdar ls) (cdar rest)))
	   (split-list-by-group (cdr ls) rest)))
	(else (split-list-by-group (cdr ls) (cons (cons (caar ls) (list (cdar ls))) 
						  (if rest rest '()))))))
;; menus-extras
;; (split-list-by-group '() '(("Emacs" "em1")))
;; (split-list-by-group '(("Emacs" . "em1")) #f)
;; (split-list-by-group '(("Emacs" . "em1") ("XTerm" . "xt1")) #f)
;; (split-list-by-group '(("Emacs" . "em1") ("Emacs" . "em2") ("XTerm" . "xt1") ("XLogo" . "xl1") ("XLogo" . "xl2")) #f)
;; (split-list-by-group '(("Emacs" . "em1") ("Emacs" . "em2") ("Emacs" . "em3") ("XTerm" . "xt1") ("XLogo" . "xl1") ("XLogo" . "xl2")) #f)
;; (define answer '(("Emacs" "em3" "em2" "em1") ("XTerm" "xt1") ("XLogo" "xl2" "xl1")))

(define*-public (fold-menu-list-by-group ml-cons #&rest rest)
  "Split ML-CONS into chained menus based on their group.
ML-CONS is a list of lists. Each sublist's car is the name of the
group, and the cdr is a list of the menuitems for that group.
The list must be sorted by GROUP.  Returns a list of menu items
for each group, popping up the group's MENUITEMs."
  (map (lambda (lm)
	 (if (null? (cddr lm))
	     (cadr lm)
	     (menuitem (car lm) #:submenu (lambda () (apply menu (cons (cdr lm) rest))))))
       (split-list-by-group ml-cons)))
