;; $Id$
;;;; Copyright (C) 1999 Maciej Stachowiak
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



(define-module (app scwm gnome-hints))




;; Hint properties

;;;; Supported protocols:
(define _WIN_PROTOCOLS           (string->X-atom "_WIN_PROTOCOLS"))

;;;; Multiple desktops
(define _WIN_WORKSPACE           (string->X-atom "_WIN_WORKSPACE")) 
(define _WIN_WORKSPACE_COUNT     (string->X-atom "_WIN_WORKSPACE_COUNT"))
(define _WIN_WORKSPACE_NAMES     (string->X-atom "_WIN_WORKSPACE_NAMES"))

;;;; Miscellaneous window settings
(define _WIN_STATE               (string->X-atom "_WIN_STATE"))
(define _WIN_HINTS               (string->X-atom "_WIN_HINTS"))

;;;; Layers
(define _WIN_LAYER               (string->X-atom "_WIN_LAYER"))

;;;; List of managed client windows
(define _WIN_CLIENT_LIST         (string->X-atom "_WIN_CLIENT_LIST"))

;;;; Announce hint support
(define _WIN_SUPPORTING_WM_CHECK (string->X-atom "_WIN_SUPPORTING_WM_CHECK"))

;;;; Undocumented, appears to be support for large scrollable virtual desktops
(define _WIN_AREA                (string->X-atom "_WIN_AREA"))
(define _WIN_AREA_COUNT          (string->X-atom"_WIN_AREA_COUNT"))

;;;; Undocumented and unsupported
;; (define _WIN_APP_STATE           (string->X-atom "_WIN_APP_STATE"))
;; (define _WIN_EXPANDED_SIZE       (string->X-atom "_WIN_EXPANDED_SIZE"))
;; (define _WIN_CLIENT_MOVING       (string->X-atom "_WIN_CLIENT_MOVING"))
;; (define _WIN_WORKAREA            (string->X-atom "_WIN_WORKAREA"))
;; (define _WIN_ICONS               (string->X-atom "_WIN_ICONS"))


;; hint bits
(define WIN_HINTS_SKIP_FOCUS      1)  ;; 'circulate-skip
(define WIN_HINTS_SKIP_WINLIST    2)  ;; 'winlist-skip
(define WIN_HINTS_SKIP_TASKBAR    4)  ;; 'taskbar-skip, but really, can be ignored
(define WIN_HINTS_GROUP_TRANSIENT 8)  ;; /* ??????? */
(define WIN_HINTS_FOCUS_ON_CLICK 16)  ;; 'focus --> 'click (ignore this)


;; state bits

;;  WIN_STATE_STICKY          = (1<<0), /* everyone knows sticky */
;;  WIN_STATE_MINIMIZED       = (1<<1), /* ??? */
;;  WIN_STATE_MAXIMIZED_VERT  = (1<<2), /* window in maximized V state */
;;  WIN_STATE_MAXIMIZED_HORIZ = (1<<3), /* window in maximized H state */
;;  WIN_STATE_HIDDEN          = (1<<4), /* not on taskbar but window visible */
;;  WIN_STATE_SHADED          = (1<<5), /* shaded (NeXT style) */
;;  WIN_STATE_HID_WORKSPACE   = (1<<6), /* not on current desktop */
;;  WIN_STATE_HID_TRANSIENT   = (1<<7), /* owner of transient is hidden */
;;  WIN_STATE_FIXED_POSITION  = (1<<8), /* window is fixed in position even */
;;  WIN_STATE_ARRANGE_IGNORE  = (1<<9)  /* ignore for auto arranging */




(define gnome-supported-protocols 
  (vector _WIN_CLIENT_LIST _WIN_WORKSPACE _WIN_WORKSPACE_COUNT _WIN_WORKSPACE_NAMES _WIN_HINTS))


(define client-window-id-list '())

;;;; Helper procedures (maybe some should be commonized w/ KDE stuff)

(define (string-append/null-separated . strings)
  (apply string-append (map (lambda (x) (string-append x (string #\nul))) strings)))

(define (X-property-value win prop)
    (let ((pval (X-property-get win prop)))
      (and pval (car pval))))

(define (X-property-numeric-value win prop)
    (let ((pval (X-property-get win prop)))
      (and pval (vector-ref (car pval) 0))))

(define (X-root-property-numeric-value prop)
  (X-property-numeric-value 'root-window prop))

;;;; Setters for the various hints

(define (gnome-set-workspace! desk-num)
  (let ((prop (X-root-property-numeric-value _WIN_WORKSPACE)))
    (if (not (eqv? desk-num prop))
        (X-property-set! 'root-window _WIN_WORKSPACE (vector desk-num) "CARDINAL" 32))))

(define (gnome-set-workspace-count! n)
  (X-property-set! 'root-window _WIN_WORKSPACE_COUNT (vector n) "CARDINAL" 32))

(define (gnome-set-workspace-names! names)
  (X-property-set! 'root-window _WIN_WORKSPACE_NAMES 
		   (apply string-append/null-separated names) "STRING" 8))

(define (gnome-set-win-workspace! win desk)
  (X-property-set! win _WIN_WORKSPACE (vector desk) "CARDINAL" 32))

(define (gnome-set-client-list! clist)
  (X-property-set! 'root-window _WIN_CLIENT_LIST
                   (list->vector clist) "CARDINAL" 32))

(define (gnome-set-protocols! proto)
  (X-property-set! 'root-window _WIN_PROTOCOLS 
                   proto "ATOM" 32))

(define (nonzero? x)
  (not (eqv? 0 x)))


(define (gnome-init-workspace-params num-desks)
  (gnome-set-workspace! (current-desk))
  (gnome-set-workspace-count! num-desks)
  (gnome-set-workspace-names! (map (lambda (x) (number->string (+ 1 x))) (iota num-desks))))

;;;; update assorted state in response to hints

(define (gnome-update-workspace)
  (let* ((prop (X-root-property-numeric-value _WIN_WORKSPACE)))
    (if (not (eqv? (current-desk) prop))
        (set-current-desk! prop))))

(define (gnome-update-win-workspace win)
  (let ((prop (X-property-numeric-value win _WIN_WORKSPACE)))
    (if prop
	(move-window-to-desk prop win))))

(define (gnome-update-hints win)
  (let ((prop (X-property-numeric-value win _WIN_HINTS)))
    (cond 
     (prop
      (set-object-property! win 'circulate-skip (nonzero? (logand prop WIN_HINTS_SKIP_FOCUS)))
      (set-object-property! win 'winlist-skip (nonzero? (logand prop WIN_HINTS_SKIP_WINLIST)))
      (set-window-property! win 'taskbar-skip (nonzero? (logand prop WIN_HINTS_SKIP_TASKBAR)))
      ;; ignore GROUP_TRANSIENT
      ;; ignore FOCUS_ON_CLICK
      ))))



(define (gnome-X-root-PropertyNotify-hook prop deleted?)
  (cond
   ((eqv? prop _WIN_WORKSPACE)
    (gnome-update-workspace))))


(define (gnome-X-PropertyNotify-hook prop win)
  (cond
   ((and (eqv? prop _WIN_HINTS))
    (gnome-update-hints win))))

(define (gnome-X-MapRequest-hook win)
  (gnome-update-hints win)
  (gnome-update-win-workspace win)
  (gnome-set-win-workspace! win (window-desk win)))

(define (gnome-new-window-hook win)
  (set! client-window-id-list (append! client-window-id-list (list (window-id win))))
  (gnome-set-client-list! client-window-id-list))

(define (gnome-destroy-notify-hook win)
  (gnome-update-win-hints win)
  (gnome-update-win-workspace win)
  (set! client-window-id-list 
        (delq! (window-id win) client-window-id-list))
  (gnome-set-client-list! client-window-id-list))

(define (gnome-change-desk-hook new-desk old-desk)
  (gnome-set-workspace-hint! new-desk))

(define (gnome-window-property-hook win property newval oldval)
  (case property
    ((desk) (gnome-set-win-workspace-hint! win newval))))


(define (announce-gnome-hint-support)
  (X-property-set! 'root-window _WIN_SUPPORTING_WM_CHECK 
                   (vector (window-id 'root-window)) "CARDINAL" 32)
  (gnome-set-protocols! gnome-supported-protocols))

  
(define (unannounce-gnome-hint-support)
  (X-property-delete! 'root-window "_WIN_SUPPORTING_WM_CHECK")
  (X-property-delete! 'root-window "_WIN_PROTOCOLS"))



(define-public (enable-gnome-hints)
  (announce-gnome-hint-support)
  (map (lambda (win)
	 (gnome-update-hints win)
	 (gnome-update-win-workspace win)
	 (gnome-set-win-workspace! win (window-desk win)))
       (list-all-windows))
  ;; hack!!!
  (gnome-init-workspace-params 4)
  (set! client-window-id-list (map window-id (list-all-windows)))
  (gnome-set-client-list! client-window-id-list)
  (add-hook! X-PropertyNotify-hook gnome-X-PropertyNotify-hook)
  (add-hook! X-root-PropertyNotify-hook gnome-X-root-PropertyNotify-hook)
  (add-hook! X-MapRequest-hook gnome-X-MapRequest-hook)
  (add-hook! after-new-window-hook gnome-new-window-hook)
  (add-hook! X-DestroyNotify-hook gnome-destroy-notify-hook)
  (add-hook! change-desk-hook gnome-change-desk-hook)
;  (add-hook! window-property-hook gnome-window-property-hook)
  (add-hook! shutdown-hook (lambda args  (disable-gnome-hints))))


(define-public (disable-gnome-hints)
  (unannounce-gnome-hint-support)
  (set! client-window-id-list '())
  (remove-hook! X-PropertyNotify-hook gnome-X-PropertyNotify-hook)
  (remove-hook! X-root-PropertyNotify-hook gnome-X-root-PropertyNotify-hook)
  (remove-hook! X-MapRequest-hook gnome-X-MapRequest-hook)
  (remove-hook! after-new-window-hook gnome-new-window-hook)
  (remove-hook! X-DestroyNotify-hook gnome-destroy-notify-hook)
  (remove-hook! change-desk-hook gnome-change-desk-hook)
;  (remove-hook! window-property-hook gnome-window-property-hook)
  (remove-hook! shutdown-hook disable-gnome-hints))














