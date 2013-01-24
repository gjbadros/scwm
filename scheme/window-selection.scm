;;; $Id$
;;; window-selection.scm
;;; Copyright (C) 1999, 2000 Greg J. Badros, Jeff W. Nichols
;;;
;;; Functions for extended window selction capabilities
;;;



(define-module (app scwm window-selection)
  :use-module (srfi srfi-1)
  :use-module (app scwm base)
  :use-module (app scwm hooks)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm message-window)
  :use-module (app scwm flash-window)
  :use-module (app scwm listops)
  :use-module (app scwm time-convert)
  :use-module (app scwm path-cache)
  :use-module (app scwm nonants)
  :use-module (app scwm optargs))

(export
 place-nonant-marker
 window-is-selected?
 select-window-toggle
 select-window-add
 select-window-remove
 remove-nonant-marker
 )


;; hook procedures

(define-scwm-hook window-selection-add-hook 1
  "Run when a window is added to the set of selected windows.
Invoked as (proc WINDOW-NOW-SELECTED).")

(define-scwm-hook window-selection-remove-hook 1
  "Run when a window is removed from the set of selected windows.
Invoked as (proc WINDOW-NO-LONGER-SELECTED).")

(define-public selected-windows '())
;;(set! selected-windows '())

(define show-nonant-flag #f)

(define* (window-is-selected? #:optional (w (get-window)))
  "Return #t if W is in the selected window list, else #f.
See also `select-window-add', `select-window-remove', and `selected-windows-list'."
  (member w selected-windows))

(define-public (selected-windows-list)
  "Returns the list of windows selected by `select-window-interactively-and-highlight'."
  (filter-map (lambda (w) (if (window-valid? w) w #f)) selected-windows))

;;(define w (select-window-interactively))
;;(filter (lambda (x) (not (eq? w x ))) selected-windows)
;;(use-scwm-modules optargs flash-window listops)

(define* (select-window-toggle #:optional (w (get-window-with-nonant)))
  "Toggle the selectedness of W.
See `select-window-add' and `select-window-remove'."
  (interactive)
  (if (window-is-selected? w)
      (select-window-remove w)
      (select-window-add w)))

 
(define* (select-window-add #:optional (w (get-window-with-nonant)))
  "Select a single window, highlight it, and add it to the selected-windows-list.
The selected window is returned and will remain highlighted
until `unflash-window' is called on that window.  The selected
window is also added to a selected-windows list that can be
accessed via `selected-windows-list'."
  (interactive)
  (if (not (object-property w 'nonant)) (set-object-property! w 'nonant 4))
  (if (not (window-is-selected? w))
      (begin
	(flash-window w #:unflash-delay #f)
	(run-hook window-selection-add-hook w)
	(if show-nonant-flag (place-nonant-marker w))
	(set! selected-windows (cons w selected-windows))
	w)))

(define* (select-window-remove #:optional (w (get-window-with-nonant)))
  "Unselect a single window, de-highlight it, and remove it from the selected-windows-list.
The selected window is removed from the selected-windows list that can be
accessed via `selected-windows-list'."
  (interactive)
  (if (not (object-property w 'nonant)) (set-object-property! w 'nonant 4))
  (if (window-is-selected? w)
      (begin
	(unflash-window w)
	(remove-nonant-marker w)
	(run-hook window-selection-remove-hook w)
	(set! selected-windows (list-without-elem selected-windows w)))))


;; (select-matching-windows (resource-match?? "xterm"))
(define*-public (select-matching-windows proc)
  "Select windows for which predicate PROC returns #t.
PROC might be one of `resource-match??', `class-match??', etc."
  (for-each select-window-add (list-windows #:only proc)))

;; (unselect-matching-windows (resource-match?? "xterm"))
(define*-public (unselect-matching-windows proc)
  "Unselect windows for which predicate PROC returns #t.
PROC might be one of `resource-match??', `class-match??', etc."
  (for-each select-window-remove (list-windows #:only proc)))


;; (unflash-window)
;; (member (get-window) selected-windows)

;; (begin (move-group-relative 10 10 selected-windows) (unselect-all-windows))

(define*-public (unselect-all-windows)
  "Unselect all windows selected via `select-window-add'."
  (interactive)
  (for-each (lambda (w) (if (window-valid? w) (unflash-window w))) selected-windows)
  (catch #t 
	 (lambda ()
	   (for-each (lambda (w) 
		       (remove-nonant-marker w)
		       (run-hook window-selection-remove-hook w))
		     selected-windows))
	 (lambda args noop))
  (set! selected-windows '()))

;; (bind-mouse 'all "H-1" (thunk select-window-add))


;; Returns them in reverse the order they were selected
;; should probably turn off the invalid interaction hook
;; or provide a way of telling select-window-interactively that
;; the root window is not an error
(define*-public (select-multiple-windows-interactively
		 #:optional (max 32000) (proc-when-selected #f))
  "Return a list of user-selected windows, up to MAX.
The list is in the reverse order from the way by which they were selected.
PROC-WHEN-SELECTED will be run on each window as it is selected."
  (if (not (integer? max))
      (set! max 32000))
  (do ((w '())
       (wlist '() (cons w wlist))
       (i 0 (+ 1 i)))
      ((or (not w) (>= i max))
       (if w wlist
	   (cdr wlist)))
    (X-server-synchronize)
    (set! w (select-window-interactively 
	     (string-append "select #" (number->string i))
	     default-message-window))
    (if (and proc-when-selected w)
	(proc-when-selected w))
    (X-server-synchronize) ;; GJB:FIXME:: Race condition...
    (add-timer-hook! (sec->msec 1) (lambda () (X-server-synchronize)))))

;;(use-scwm-modules time-convert)
;; e.g.
;;(select-multiple-windows-interactively 10)
;;(restack-windows (select-multiple-windows-interactively 3))

;; nonant marker procedures
;; this also gets used in nonants.scm for get-window-with-nonant-interactively
(define-public (set-markwin-offset! win nonant markwin)
  (let* ((marksize (message-window-size markwin))
	 (winsize  (window-frame-size win))
	 (winpos   (window-viewport-position win))
	 (xoffset  (round (* 0.4 (car winsize))))
	 (yoffset  (round (* 0.4 (cadr winsize))))
	 (xnon     (- (remainder nonant 3) 1))
	 (ynon     (- (quotient nonant 3) 1))
	 (xpos     (+ (car winpos) (quotient (car winsize) 2)))
	 (ypox     (+ (cadr winpos) (quotient (cadr winsize) 2))))
    (message-window-set-position! markwin (+ xpos (* xoffset xnon)) (+ ypox (* yoffset ynon)))))



(define* (place-nonant-marker #:optional (w (get-window-with-nonant)))
  "Place a nonant marker on W."
  (if (and (window? w) (object-property w 'nonant))
      (let ((nonant (object-property w 'nonant))
	    (markwin (if (message-window? (object-property w 'markwin))
			 (object-property w 'markwin)
			 (make-message-window-with-image (make-image "anchor.xpm")))))
	(set-markwin-offset! w nonant markwin)
	(message-window-show! markwin)
	(set-object-property! w 'markwin markwin))))

	     
(define* (remove-nonant-marker #:optional (w (get-window)))
  "Remove a nonant marker from W."
  (let ((markwin (object-property w 'markwin)))
    (if (message-window? markwin)
	(begin 
	  (set-object-property! w 'markwin #f)
	  (message-window-hide! markwin)))))


;; hook routines to ensure nonant markers behave correctly

(define (reset-position w)
  (let ((markwin (object-property w 'markwin))
	(nonant  (object-property w 'nonant)))
    (if (message-window? markwin)
	(set-markwin-offset! w nonant markwin))))

(define (change-hook x y dx dy)
  (for-each reset-position selected-windows))

(add-hook! viewport-position-change-hook change-hook)
;; (remove-hook! viewport-position-change-hook change-hook)      

(define (move-hook win new-x new-y) 
  (reset-position win))

(add-hook! interactive-move-new-position-hook move-hook)
;; (remove-hook! interactive-move-new-position-hook move-hook)

(define (resize-hook win x y new-w new-h new-wu new-hu) 
  (reset-position win))

(add-hook! interactive-resize-new-size-hook resize-hook)
;; (remove-hook! interactive-resize-new-size-hook resize-hook)

(define (desk-hook new old)
  (for-each (lambda (w)
	      (let ((desk (window-desk w))
		    (markwin (object-property w 'markwin)))
		(if (message-window? markwin)
		    (if (eqv? desk new)
			(message-window-show! markwin)
			(message-window-hide! markwin)))))
	      selected-windows))
  
(add-hook! change-desk-hook desk-hook)
;; (remove-hook! change-desk-hook desk-hook)

;; toggle on and off the nonant markers

(define-public (show-selected-nonants)
  "Show nonant markers on window when they are selected."
  (set! show-nonant-flag #t)
  (for-each place-nonant-marker selected-windows))

(define-public (hide-selected-nonants)
  "Hide nonant markers on window when they are selected."
  (set! show-nonant-flag #f)
  (for-each remove-nonant-marker selected-windows))

(define-public (flash-selected-windows-on)
  "Make sure that the selected windows are marked as selected."
  (for-each flash-window (selected-windows-list)))

(define-public (set-selected-windows-list! list-of-windows)
  "Make LIST be the set of selected windows."
  (unselect-all-windows)
  (for-each select-window-add list-of-windows))
