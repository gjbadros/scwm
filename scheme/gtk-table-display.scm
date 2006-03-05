;;;; $Id$
;;;; Copyright (C) 2000 Greg J. Badros
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


(define-module (app scwm gtk-table-display)
  :use-module (ice-9 regex)
  :use-module (ice-9 string-fun)
  :use-module (gtk gtk)
  :use-module (app scwm gtk)
  :use-module (app scwm base)
  :use-module (app scwm stringops)
  :use-module (app scwm listops)
  :use-module (app scwm optargs))

(define-public (for-each-column string char proc)
  (map proc (separate-fields-discarding-char char string list)))

;; workaround a bug in gtk-clist-append not handling shared substrings
;; (make-real-strings (list "foo" "bar"))
(define (make-real-strings list-of-strings)
  (if (pair? list-of-strings)
      (cons (string (car list-of-strings))
	    (make-real-strings (cdr list-of-strings)))
      '()))

;; (define str "Foo\tBar\nBaz\tBong\n")
;; (gtk-table-from-string str #:select-proc (lambda (vals) noop))
;; (define row1 (car (separate-fields-discarding-char #\newline str list)))
;; (gtk-clist-append clist (separate-fields-discarding-char #\tab row1 list))
;; (make-real-strings (separate-fields-discarding-char #\tab row1 list))
;; (gtk-table-from-string str #:select-proc (lambda (vals) (write vals) (newline) (netscape-goto-url (car vals))))
;; (use-scwm-modules file)
;; (define str (output-of-system-cmd "bookmark-grep extreme"))
;; (set! str (make-shared-substring str 0 (1- (string-length str))))

(define*-public (gtk-table-from-string string #:key
				       (select-proc #f) 
				       (titles #("URL" "Title")) 
				       (window-title "URLs"))
  (let* ((toplevel (gtk-window-new 'dialog))
	 (vbox (gtk-vbox-new #f 5))
	 (hbox-buttons (gtk-hbutton-box-new))
	 (okbut (gtk-button-new-with-label "Dismiss"))
	 (clist (gtk-clist-new-with-titles titles))
	 )
    (for-each-column 
     string #\newline
     (lambda (line)
       (let ((row-strings (make-real-strings (separate-fields-discarding-char #\tab line list))))
	 (if (not (and (= 1 (length row-strings)) (string=? (car row-strings) "")))
	     (gtk-clist-append clist row-strings)))))
    (gtk-window-set-title toplevel window-title)
    (gtk-box-pack-start hbox-buttons okbut #t #f)
    (gtk-box-pack-start vbox clist #t #t)
    (gtk-box-pack-start vbox hbox-buttons #f #f)
    (gtk-clist-set-column-width clist 0 300)
    (gtk-clist-set-column-width clist 1 300)
    (gtk-container-add toplevel vbox)
    (gtk-signal-connect okbut "clicked" 
			(lambda () 
			  (gtk-widget-destroy toplevel)))
    (gtk-signal-connect clist "select_row"
			(lambda (row col event)
			  (let ((vals (gtk-clist-get-row-values clist row 0)))
			    (select-proc vals))))
    (let ((pp (pointer-position)))
      (gtk-widget-set-uposition toplevel (- (car pp) 150) (cadr pp)))
    (gtk-widget-show-all toplevel)))

;; (define clist (gtk-clist-new-with-titles #("URL" "title")))
;; (gtk-clist-append clist (list "foo" "bar"))
;; (gtk-clist-append clist (list (make-shared-substring "foo") "bar"))
;; (gtk-clist-append clist (list (string (make-shared-substring "foo")) "bar"))
