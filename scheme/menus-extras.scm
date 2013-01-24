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
  :use-module (app scwm optargs))




(define-public (menu-max-fold-lines)
  "Return an approximation of the number of menuitems that will fit vertically on screen."
  (let* ((menu-font-property menu-font)
	 (menu-font (if (symbol? menu-font-property)
                        (eval menu-font-property (current-module))
                        menu-font-property))
	 (menu-font-height (assoc-ref (font-properties menu-font) 'height)))
    (scwm-round/ (cadr (display-size)) (+ 7 menu-font-height))))

(define-public (sorted-by-car-string l)
  "Sort the elements of list L based on the string value of their `car'.
Uses string<? to compare the elements which results in using alphabetical
order."
  (sort l (lambda (a b) (string<? (car a) (car b)))))

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

;;; Returns a list of lists with the same elements as LS.
;;; The returned list will have the same elements in the same order.  Each
;;; sublist returned will have no more than max elements
;;; This function destroys the original list.
(define (split-list! ls max)
  (let ((le (length ls)) (tt '()) (t1 '()))
    (cond ((<= le max) (list ls))
	  (#t (set! tt (list-tail ls (- max 1))) (set! t1 (cdr tt))
	      (set-cdr! tt '()) (cons ls (split-list! t1 max))))))

(define*-public (fold-menu-list!
		ml #:optional (max-lines (optget *menu-max-fold-lines*)))
  "Split ML into chained menus of no more than MAX-LINES items.
ML is a list of menuitem objects. MAX-LINES is a number, which
defaults to `*menu-max-fold-lines*'.  Destroys the argument ML."
  (if (<= (length ml) max-lines) ml
      (map (lambda (lm) (menuitem "more..." #:submenu (lambda () (menu lm))))
	   (split-list! ml max-lines))))

;;; DEPRECATED
(define-public fold-menu-list fold-menu-list!)

;;; SRL:FIXME::Can't we just call 'sorted-by-car-string' here since
;;;   all the callers have to call it anyway for this to work?
(define*-public (split-list-by-group ls)
  "Split the association list LS into groups according to the keys of the
list.  The elements of LS must already be sorted such that equal keys are
next to each other in the list (see 'sorted-by-car-string').  Returns an
association list where the items from LS with the same key share a single
entry in the new list and the items with that key are stored as a list of
values.  Uses 'string=?' to compare keys.  Maintains the relative order of
the elements of LS in the same group.  Maintains the order of the groups."
  (letrec ((slbg-help 
             (lambda (ls result)
               (cond ((null? ls) result)
                     ((and (not (null? result))
                           result (string=? (caar ls) (caar result)))
                      (begin
                        (set-cdr! (car result) (cons (cdar ls) (cdar result)))
                        (slbg-help (cdr ls) result)))
                     (else (slbg-help (cdr ls) 
                                      (cons (cons (caar ls) (list (cdar ls)))
                                            result)))))))
           (reverse 
            (map (lambda (kv) (cons (car kv) (reverse (cdr kv))))
                 (slbg-help ls '())))))

;; menus-extras
;; (split-list-by-group '(("Emacs" . "em1")))
;; (split-list-by-group '(("Emacs" . "em1") ("XTerm" . "xt1")))
;; (split-list-by-group '(("Emacs" . "em1") ("Emacs" . "em2") ("XTerm" . "xt1") ("XLogo" . "xl1") ("XLogo" . "xl2")))
;; (split-list-by-group '(("Emacs" . "em1") ("Emacs" . "em2") ("Emacs" . "em3") ("XTerm" . "xt1") ("XLogo" . "xl1") ("XLogo" . "xl2")))
;; (define answer '(("Emacs" "em1" "em2" "em3") ("XTerm" "xt1") ("XLogo" "xl1" "xl2")))

;;; SRL:FIXME:: This interface stinks.  Fix it.
(define*-public (fold-menu-list-by-group ml-cons #:rest rest)
  "Split ML-CONS into chained menus based on their group.
ML-CONS is a association list. Each sublist's car is the name of the
group, and the cdr is a menuitem for that group.  See 'sorted-by-car-string'
and 'split-list-by-group'.  The list must be sorted by GROUP.  Returns a list of menu items
for each group, popping up the group's MENUITEMs.  REST are optional arguments
to be used when creating the sub-menus."
  (map (lambda (lm)
	 (if (null? (cddr lm))
	     (cadr lm)
	     (menuitem (car lm) #:submenu (lambda () (apply menu (cons (cdr lm) rest))))))
       (split-list-by-group ml-cons)))
