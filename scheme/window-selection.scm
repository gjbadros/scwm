;;; $Id$
;;; window-selection.scm
;;; Copyright (C) 1999 Greg J. Badros
;;;
;;; Functions for extended window selction capabilities
;;;



(define-module (app scwm window-selection)
  :use-module (app scwm base)
  :use-module (app scwm wininfo)
  :use-module (app scwm winlist)
  :use-module (app scwm message-window)
  :use-module (app scwm flash-window)
  :use-module (app scwm listops)
  :use-module (app scwm group)
  :use-module (app scwm time-convert)
  :use-module (app scwm path-cache)
  :use-module (app scwm optargs))



;; hook procedures

(define window-selection-add-hook '())
(define window-selection-remove-hook '())

(define selected-windows '())
;;(set! selected-windows '())

(define*-public (window-is-selected? #&optional (w (get-window)))
  "Return #t if W is in the selected window list, else #f.
See also `select-window-add-selection' and `selected-windows-list'."
  (member w selected-windows))

(define-public (selected-windows-list)
  "Returns the list of windows selected by `select-window-interactively-and-highlight'."
  selected-windows)

;;(define w (select-window-interactively))
;;(filter (lambda (x) (not (eq? w x ))) selected-windows)

(define*-public (select-window-add-selection #&optional (w (get-window)))
  "Select a single window, highlight it, and add it to the seelcted-windows-list.
The selected window is returned and will remain highlighted
until `unflash-window' is called on that window.  The selected
window is also added to a selected-windows list that can be
accessed via `selected-windows-list'."
  (if (member w selected-windows)
      (begin
	(unflash-window w)
	(call-hook-procedures window-selection-remove-hook (list w))
	(set! selected-windows (list-without-elem selected-windows w)))
      (begin
	(flash-window w #:unflash-delay #f)
	(call-hook-procedures window-selection-add-hook (list w))
	(set! selected-windows (cons w selected-windows))
	w)))

;; (unflash-window)
;; (member (get-window) selected-windows)

;; (begin (move-group-relative 10 10 selected-windows) (unselect-all-windows))

(define-public (unselect-all-windows)
  "Unselect all windows selected via `select-window-add-selecttion'."
  (for-each unflash-window selected-windows)
  (for-each (lambda (w) (call-hook-procedures window-selection-remove-hook (list w))) selected-windows)
  (set! selected-windows '()))

;; (bind-mouse 'all "H-1" (thunk select-window-add-selection))


;; Returns them in reverse the order they were selected
;; should probably turn off the invalid interaction hook
;; or provide a way of telling select-window-interactively that
;; the root window is not an error
(define*-public (select-multiple-windows-interactively
		 #&optional (max 32000) (proc-when-selected #f))
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
    (handle-pending-events)
    (set! w (select-window-interactively 
	     (string-append "select #" (number->string i))
	     default-message-window))
    (if (and proc-when-selected w)
	(proc-when-selected w))
    (handle-pending-events) ;; GJB:FIXME:: Race condition...
    (add-timer-hook! (sec->usec 1) (lambda () (handle-pending-events)))))

;;(use-scwm-modules time-convert)
;; e.g.
;;(select-multiple-windows-interactively 10)
;;(restack-windows (select-multiple-windows-interactively 3))

;; adding and removing hooks

(define-public (add-window-selection-add-hook! hook)
  "Add a HOOK to be called when a window is added to the selection list.
HOOK should take a single parameter which is the window selected."
  (set! window-selection-add-hook (cons hook window-selection-add-hook)))

(define-public (add-window-selection-remove-hook! hook)
  "Add a HOOK to be called when a window is removed from the selection list.
HOOK should take a single parameter which is the window removed."
  (set! window-selection-remove-hook (cons hook window-selection-remove-hook)))

(define-public (remove-window-selection-add-hook! hook)
  "Remove a HOOK to be called when a window is added to the selection list.
HOOK should take a single parameter which is the window selected."
  (set! window-selection-add-hook (delq hook window-selection-add-hook)))

(define-public (remove-window-selection-remove-hook! hook)
  "Remove a HOOK to be called when a window is removed from the selection list.
HOOK should take a single parameter which is the window removed."
  (set! window-selection-remove-hook (delq hook window-selection-remove-hook)))
