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



(define-module (app scwm gnome-hints)
  :use-module (app scwm base)
  :use-module (app scwm winops))




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
(define _WIN_EXPANDED_SIZE       (string->X-atom "_WIN_EXPANDED_SIZE"))

;;;; Layers
(define _WIN_LAYER               (string->X-atom "_WIN_LAYER"))

;;;; List of managed client windows
(define _WIN_CLIENT_LIST         (string->X-atom "_WIN_CLIENT_LIST"))

;;;; Announce hint support
(define _WIN_SUPPORTING_WM_CHECK (string->X-atom "_WIN_SUPPORTING_WM_CHECK"))

;;;; Undocumented, appears to be support for large scrollable virtual desktops
(define _WIN_AREA                (string->X-atom "_WIN_AREA"))
(define _WIN_AREA_COUNT          (string->X-atom "_WIN_AREA_COUNT"))

;;;; Undocumented and unsupported
;; (define _WIN_APP_STATE           (string->X-atom "_WIN_APP_STATE"))
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

(define WIN_STATE_STICKY            1) ;; /* everyone knows sticky */
(define WIN_STATE_MINIMIZED         2) ;; /* ??? */ - ignore this, there are iconify hints already
(define WIN_STATE_MAXIMIZED_VERT    4) ;; /* window in maximized V state */
(define WIN_STATE_MAXIMIZED_HORIZ   8) ;; /* window in maximized H state */
(define WIN_STATE_HIDDEN           16) ;; /* not on taskbar but window visible */
(define WIN_STATE_SHADED           32) ;; /* shaded (NeXT style) */
(define WIN_STATE_HID_WORKSPACE    64) ;; /* not on current desktop */
(define WIN_STATE_HID_TRANSIENT   128) ;; /* owner of transient is hidden */
(define WIN_STATE_FIXED_POSITION  256) ;; /* window is fixed in position even */
(define WIN_STATE_ARRANGE_IGNORE  512) ;;  /* ignore for auto arranging */


(define WIN_STATE_ALL (+ WIN_STATE_STICKY 
			 WIN_STATE_MINIMIZED
			 WIN_STATE_MAXIMIZED_VERT 
			 WIN_STATE_MAXIMIZED_HORIZ 
			 WIN_STATE_HIDDEN 
			 WIN_STATE_SHADED 
			 WIN_STATE_HID_WORKSPACE 
			 WIN_STATE_HID_TRANSIENT 
			 WIN_STATE_FIXED_POSITION
			 WIN_STATE_ARRANGE_IGNORE))

(define gnome-supported-protocols 
  (vector _WIN_CLIENT_LIST 
	  _WIN_WORKSPACE _WIN_WORKSPACE_COUNT _WIN_WORKSPACE_NAMES 
	  _WIN_STATE _WIN_HINTS _WIN_EXPANDED_SIZE 
	  _WIN_LAYER 
	  _WIN_AREA_COUNT _WIN_AREA))


(define client-window-id-list '())

;;;; Helper procedures (maybe some should be commonized w/ KDE stuff)

(define (string-append/null-separated . strings)
  (apply string-append (map (lambda (x) (string-append x (string #\nul))) strings)))

(define (X-property-value win prop)
    (let ((pval (X-property-get win prop)))
      (and pval (car pval))))

(define (X-root-property-value prop)
  (X-property-value 'root-window prop))

(define (X-property-numeric-value win prop)
    (let ((pval (X-property-get win prop)))
      (and pval (vector-ref (car pval) 0))))

(define (X-root-property-numeric-value prop)
  (X-property-numeric-value 'root-window prop))

(define (nonzero? x)
  (not (eqv? 0 x)))


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

(define (gnome-set-area! x y)
  (X-property-set! 'root-window _WIN_AREA
                   (list->vector 
                    (map (lambda (vp ds) (round/ vp ds))
                         (list x y) (display-size)))
                   "CARDINAL" 32))

(define (gnome-set-area-count! w h)
  (X-property-set! 'root-window _WIN_AREA_COUNT (vector w h) "CARDINAL" 32))

(define (gnome-set-state! win)
  (let* ((cur-state (or (X-property-numeric-value win _WIN_STATE) 0))
         (or-mask (logior 
                   (if (sticky? win) WIN_STATE_STICKY 0)
                   (if (maximized? win) WIN_STATE_MAXIMIZED_VERT 0)
                   (if (maximized? win) WIN_STATE_MAXIMIZED_HORIZ 0)
                   (if (window-shaded? win) WIN_STATE_SHADED 0)
                   (if (object-property win 'arrange-skip) 
                       WIN_STATE_ARRANGE_IGNORE 0)))
         (and-mask (logand 
                    #x7fffffff
                    (if (sticky? win) #x7fffffff (lognot WIN_STATE_STICKY))
                    (if (maximized? win) 
                        #x7fffffff (lognot WIN_STATE_MAXIMIZED_VERT))
                    (if (maximized? win) 
                        #x7fffffff (lognot WIN_STATE_MAXIMIZED_HORIZ))
                    (if (window-shaded? win) 
                        #x7fffffff (lognot WIN_STATE_SHADED))
                    (if (object-property win 'arrange-skip) 
                        #x7fffffff (lognot WIN_STATE_ARRANGE_IGNORE)))))
    (X-property-set! win _WIN_STATE 
                     (vector (logior or-mask 
                                     (logand and-mask cur-state)))
                     "CARDINAL" 32)))
  

(define (gnome-set-layer! win)
  (if (kept-on-top? win)
      (let ((old-layer (X-property-numeric-value win _WIN_LAYER)))
        (if (not (and old-layer (> old-layer 4)))
            (X-property-set! win _WIN_LAYER (vector 6) "CARDINAL" 32)))
      (X-property-set! win _WIN_LAYER (vector 4) "CARDINAL" 32)))



(define (gnome-set-client-list! clist)
  (X-property-set! 'root-window _WIN_CLIENT_LIST
                   (list->vector clist) "CARDINAL" 32))

(define (gnome-set-protocols! proto)
  (X-property-set! 'root-window _WIN_PROTOCOLS 
                   proto "ATOM" 32))

(define (gnome-init-workspace-params num-desks)
  (gnome-set-workspace! (current-desk))
  (gnome-set-workspace-count! num-desks)
  (gnome-set-workspace-names! (map (lambda (x) (number->string (+ 1 x))) 
                                   (iota num-desks))))


(define (gnome-init-area-params)
  (apply gnome-set-area-count! (desk-size))
  (apply gnome-set-area! (viewport-position)))

;;;; update assorted state in response to hints

(define (gnome-update-workspace-from-property)
  (let* ((prop (X-root-property-numeric-value _WIN_WORKSPACE)))
    (if (not (eqv? (current-desk) prop))
        (set-current-desk! prop))))

(define (gnome-update-workspace-from-client-message data)
  (let* ((new-desk (vector-ref data 0)))
    (if (not (eqv? (current-desk) new-desk))
        (set-current-desk! new-desk))))

(define (gnome-update-win-workspace-from-property win)
  (let ((prop (X-property-numeric-value win _WIN_WORKSPACE)))
    (if prop
	(move-window-to-desk prop win))))


(define (gnome-update-win-workspace-from-client-message win data)
  (let ((new-desk (vecttor-ref data 0)))
    (if prop
	(move-window-to-desk new-desk win))))


(define (gnome-update-area-from-client-message data)
  (let* ((new-x (vector-ref data 0))
	 (new-y (vector-ref data 1))
	 (new-pos (map * (display-size) (list new-x new-y))))
    (if (not (equal? (viewport-position) new-pos))
        (apply set-viewport-position! new-pos))))


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



(define (gnome-update-state win mask new-state)
  (if (nonzero? (logand mask WIN_STATE_STICKY)) 
      (if (nonzero? (logand new-state WIN_STATE_STICKY)) 
	  (if (not (sticky? win)) (stick win)) 
	  (if (sticky? win) (unstick win))))

  ;; ignore WIN_STATE_MINIMIZED  - apparently deprecated


  ;; for now kludge separate horizontal and vertical maximization
  
  (if (or (nonzero? (logand mask WIN_STATE_MAXIMIZED_VERT))
	  (nonzero? (logand mask WIN_STATE_MAXIMIZED_HORIZ)))
      
      (if (or (nonzero? (logand mask new-state WIN_STATE_MAXIMIZED_VERT))
	      (nonzero? (logand mask new-state  WIN_STATE_MAXIMIZED_HORIZ)))
	  (if (not (maximized? win))
	      (maximize 
	       (if (nonzero? (logand mask new-state WIN_STATE_MAXIMIZED_HORIZ)) (%x 100) 0)
	       (if (nonzero? (logand mask new-state WIN_STATE_MAXIMIZED_VERT)) (%y 100) 0)
	       win))
	  (if (maximized? win)
	      (unmaximize win))))

  ;; ignore WIN_STATE_HIDDEN - it is unused and duplicates a WIN_HINTS bit

  (if (nonzero? (logand mask WIN_STATE_SHADED))
      (if (nonzero? (logand new-state WIN_STATE_SHADED))
	  (if (not (window-shaded? win)) (window-shade win))
	  (if (window-shaded? win) (window-unshade win))))

  ;; ignore WIN_STATE_HID_WORKSPACE  (poorly specified)
  ;; ignore WIN_STATE_HID_TRANSIENT  (poorly specified)
  ;; ignore WIN_STATE_FIXED_POSITION (no way to implement right now)

  (if (nonzero? (logand mask WIN_STATE_ARRANGE_IGNORE))
      (set-object-property! win 'arrange-skip 
                            (nonzero? 
                             (logand new-state WIN_STATE_ARRANGE_IGNORE)))))

  
(define (gnome-update-state-from-property win)
  (let ((prop (X-property-numeric-value win _WIN_HINTS)))
    (cond 
     (prop
      (gnome-update-state win WIN_STATE_ALL prop)))))
      
(define (gnome-update-state-from-client-message win msg)
  (let ((mask (vector-ref msg 0))
	(new-state (vector-ref msg 1)))
      (gnome-update-state win mask new-state)))



(define (gnome-update-layer win new-layer)
  (if (> new-layer 4)
      (if (not (kept-on-top? win))
	  (keep-on-top win))
      (if (kept-on-top? win)
	  (un-keep-on-top win))))

(define (gnome-update-layer-from-property win)
  (let ((new-layer (X-property-numeric-value win _WIN_LAYER)))
    (if new-layer  
	(gnome-update-layer win new-layer))))

(define (gnome-update-layer-from-client-message win msg)
  (let ((new-layer (vector-ref msg 0)))
    (gnome-update-layer win new-layer)))



(define (gnome-init-window-from-props win)
  (gnome-update-hints win)
  (gnome-update-win-workspace-from-property win)
  (gnome-set-win-workspace! win (window-desk win))
  (gnome-update-state-from-property win)
  (gnome-set-state! win)
  (gnome-update-layer-from-property win)
  (gnome-set-layer! win))

(define (gnome-X-root-PropertyNotify-hook prop deleted?)
;;  (cond
;;   ((eqv? prop _WIN_WORKSPACE)
;;    (gnome-update-workspace))))
  ())


(define (gnome-X-PropertyNotify-hook prop win)
  (cond
   ((eqv? prop _WIN_HINTS)
    (gnome-update-hints win))))

(define (gnome-X-MapRequest-hook win)
  (gnome-init-window-from-props win))

(define (gnome-new-window-hook win)
  (set! client-window-id-list (append! client-window-id-list (list (window-id win))))
  (gnome-set-client-list! client-window-id-list))

(define (gnome-window-close-hook win)
  (set! client-window-id-list 
        (delq! (window-id win) client-window-id-list))
  (gnome-set-client-list! client-window-id-list))

(define (gnome-change-desk-hook new-desk old-desk)
  (gnome-set-workspace! new-desk))

(define (gnome-desk-size-change-hook width height)
  (gnome-set-area-count! width height))

(define (gnome-viewport-position-change-hook x y)
  (gnome-set-area! x y))

(define (gnome-window-property-change-hook win property newval oldval)
  (case property
    ((sticky maximized shaded arrange-skip) (gnome-set-state! win))
    ((desk) (gnome-set-win-workspace! win newval))
    ((on-top) (gnome-set-layer! win)))
  (write (list 'property-change: win property newval oldval))
  (newline))


(define (gnome-client-message-hook win type format data)
;;  (display "Client message: ")
;;  (newline)
;;  (write win)
;;  (newline)
;;  (write type)(display " ")(write (X-atom->string type))
;;  (newline)
;;  (write format)
;;  (newline)
;;  (write data)
;;  (newline)
  (cond
   ((eqv? type _WIN_STATE) 
    (gnome-update-state-from-client-message win data))
   ((eqv? type _WIN_LAYER) 
    (gnome-update-layer-from-client-message win data))
   ((eqv? type _WIN_WORKSPACE)
    (if (eq? win 'root-window)
	(gnome-update-workspace-from-client-message data)
	(gnome-update-win-workspace-from-client-message win workspace)))
   ((eqv? type _WIN_AREA)
    (if (eq? win 'root-window)
	(gnome-update-area-from-client-message data)))))


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
	 (gnome-init-window-from-props win))
       (list-all-windows))
  ;; hack!!!
  (gnome-init-workspace-params 4)
  (gnome-init-area-params)
  (set! client-window-id-list (map window-id (list-all-windows)))
  (gnome-set-client-list! client-window-id-list)
  (add-hook! X-PropertyNotify-hook gnome-X-PropertyNotify-hook)
  (add-hook! X-root-PropertyNotify-hook gnome-X-root-PropertyNotify-hook)
  (add-hook! X-MapRequest-hook gnome-X-MapRequest-hook)
  (add-hook! after-new-window-hook gnome-new-window-hook)
  (add-hook! window-close-hook gnome-window-close-hook)
  (add-hook! change-desk-hook gnome-change-desk-hook)
  (add-hook! desk-size-change-hook gnome-desk-size-change-hook)
  (add-hook! viewport-position-change-hook gnome-viewport-position-change-hook)
  (add-hook! client-message-hook gnome-client-message-hook)
  (add-hook! window-property-change-hook gnome-window-property-change-hook)
;  (add-hook! window-property-hook gnome-window-property-hook)
  (add-hook! shutdown-hook (lambda args  (disable-gnome-hints))))


(define-public (disable-gnome-hints)
  (unannounce-gnome-hint-support)
  (set! client-window-id-list '())
  (remove-hook! X-PropertyNotify-hook gnome-X-PropertyNotify-hook)
  (remove-hook! X-root-PropertyNotify-hook gnome-X-root-PropertyNotify-hook)
  (remove-hook! X-MapRequest-hook gnome-X-MapRequest-hook)
  (remove-hook! after-new-window-hook gnome-new-window-hook)
  (remove-hook! window-close-hook gnome-window-close-hook)
  (remove-hook! change-desk-hook gnome-change-desk-hook)
  (remove-hook! desk-size-change-hook gnome-desk-size-change-hook)
  (remove-hook! viewport-position-change-hook gnome-viewport-position-change-hook)
  (remove-hook! client-message-hook gnome-client-message-hook)
  (remove-hook! window-property-change-hook gnome-window-property-change-hook)
;  (remove-hook! window-property-hook gnome-window-property-hook)
  (remove-hook! shutdown-hook disable-gnome-hints))


;;;; Enable by default
(enable-gnome-hints)

