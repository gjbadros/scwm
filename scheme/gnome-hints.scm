;; $Id$
;;;; Copyright (C) 1999, 2000 Maciej Stachowiak and Greg J. Badros
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
  :use-module (app scwm optargs)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  :use-module (app scwm message-window)
  :use-module (app scwm animation)
  :use-module (app scwm style)
  :use-module (app scwm placement)
  :use-module (app scwm basic-styles)
  :use-module (app scwm style)
  :use-module (app scwm winops))



(define-scwm-group gnome "Gnome")

;;;; See the file gnome-libs-*/libgnomeui/gnome-winhints.h

;;; user variables.
;;; set them in ~/.scwmrc, then call (enable-gnome-hints)

(define-scwm-option *debug-gnome-hints* #f
  "Set true to enable debugging messages for the gnome-hints module."
  #:type 'boolean
  #:group 'gnome)
;;(set! *debug-gnome-hints* #t)

(define-scwm-option *gnome-shade-animated* #f
  "Should the shading be animated or not?"
  #:type 'boolean
  #:group 'gnome)

;;; Hint properties

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
                    (map (lambda (vp ds) (scwm-round/ vp ds))
                         (list x y) (display-size)))
                   "CARDINAL" 32))

(define (gnome-set-area-count! w h)
  (X-property-set! 'root-window _WIN_AREA_COUNT (vector w h) "CARDINAL" 32))

(define (gnome-set-state! win)
  (let* ((cur-state (or (X-property-numeric-value win _WIN_STATE) 0))
         (or-mask (logior
                   (if (sticky-window? win) WIN_STATE_STICKY 0)
                   (if (maximized? win) WIN_STATE_MAXIMIZED_VERT 0)
                   (if (maximized? win) WIN_STATE_MAXIMIZED_HORIZ 0)
                   (if (shaded-window? win) WIN_STATE_SHADED 0)
                   (if (object-property win 'arrange-skip)
                       WIN_STATE_ARRANGE_IGNORE 0)))
         (and-mask (logand 
                    WIN_STATE_ALL
                    (if (sticky-window? win) WIN_STATE_ALL (lognot WIN_STATE_STICKY))
                    (if (maximized? win) 
                        WIN_STATE_ALL (lognot WIN_STATE_MAXIMIZED_VERT))
                    (if (maximized? win) 
                        WIN_STATE_ALL (lognot WIN_STATE_MAXIMIZED_HORIZ))
                    (if (shaded-window? win) 
                        WIN_STATE_ALL (lognot WIN_STATE_SHADED))
                    (if (object-property win 'arrange-skip) 
                        WIN_STATE_ALL (lognot WIN_STATE_ARRANGE_IGNORE)))))
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
;;  (display "state = ")
;;  (write-line new-state)
  (and win
       (if (nonzero? (logand mask WIN_STATE_STICKY))
	   (if (nonzero? (logand new-state WIN_STATE_STICKY))
	       (if (not (sticky-window? win)) (stick-window win))
	       (if (sticky-window? win) (unstick-window win)))))

  ;; ignore WIN_STATE_MINIMIZED  - apparently deprecated

  ;; gmc's desktop_icons come up with maximize and minimize bits set
  ;; why?  for now, just do not maximize if WIN_STATE_MINIMIZED --09/29/99 gjb

  (if (not (nonzero? (logand mask WIN_STATE_MINIMIZED)))
      (begin
	;; for now kludge separate horizontal and vertical maximization
	
	(if (or (nonzero? (logand mask WIN_STATE_MAXIMIZED_VERT))
		(nonzero? (logand mask WIN_STATE_MAXIMIZED_HORIZ)))
	    
	    (if (or (nonzero? (logand mask new-state WIN_STATE_MAXIMIZED_VERT))
		    (nonzero? (logand mask new-state WIN_STATE_MAXIMIZED_HORIZ)))
		(if (not (maximized? win))
		    (begin
;;		      (write-line "maximizing")
		      (maximize
		       (if (nonzero? (logand mask new-state WIN_STATE_MAXIMIZED_HORIZ)) (%x 100) 0)
		 (if (nonzero? (logand mask new-state WIN_STATE_MAXIMIZED_VERT)) (%y 100) 0)
		 win)))
		(if (maximized? win)
		    (begin
;;		      (write-line "unmaximizing")
		      (unmaximize win)))))
	))

  ;; ignore WIN_STATE_HIDDEN - it is unused and duplicates a WIN_HINTS bit

  (if (nonzero? (logand mask WIN_STATE_SHADED))
      (if (nonzero? (logand new-state WIN_STATE_SHADED))
	  (if (not (shaded-window? win))
	      (if (equal? (window-class win) "Panel")
		  (write-line "FIXME: *not* window-shading the panel")
		  (if *gnome-shade-animated*
		      (animated-window-shade win) (shade-window win))))
	  (if (shaded-window? win)
              (if *gnome-shade-animated*
                  (animated-window-unshade win) (unshade-window win)))))

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
  (and win
       (if (> new-layer 4)
	   (if (not (kept-on-top? win))
	       (keep-on-top win))
	   (if (kept-on-top? win)
	       (un-keep-on-top win)))))

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
  '())


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

(define (gnome-viewport-position-change-hook x y dx dy)
  (gnome-set-area! x y))

(define (gnome-window-property-change-hook win property newval oldval)
  (cond
   (*debug-gnome-hints*
    (write (list 'window 'property 'change: win property newval oldval))
    (newline)))
  (case property
    ((sticky maximized shaded arrange-skip) (gnome-set-state! win))
    ((desk) (gnome-set-win-workspace! win newval))
    ((on-top) (gnome-set-layer! win))))


(define (gnome-client-message-hook win type format data)
  (cond
   (*debug-gnome-hints*
    (write (list 'Client 'message: win (X-atom->string type) format data))
    (newline)))
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

(define-public (gnome-desktop-press n)
  "Send a button-press N to the gnome desktop manager."
  (send-button n bpress_win_id 'desk-press))

(define-public (gnome-desktop-click n)
  "Send a button-press N to the gnome desktop manager."
  (send-button n bpress_win_id 'desk-click))

(define*-public (gnome-desktop-menu)
  "Pop-up the gnome desktop menu."
  (interactive)
  (gnome-desktop-press 3))

(define*-public (gnome-desktop-press-1)
  "Send a button-press 1 to the gnome desktop manager."
  (interactive)
  (gnome-desktop-press 1))

(define*-public (gnome-desktop-press-2)
  "Send a button-press 1 to the gnome desktop manager."
  (interactive)
  (gnome-desktop-press 2))

(define*-public (gnome-desktop-press-3)
  "Send a button-press 1 to the gnome desktop manager."
  (interactive)
  (gnome-desktop-press 3))

(define*-public (gnome-desktop-click-1)
  "Send a button-press 1 and button-release 1 to the gnome desktop manager."
  (interactive)
  (gnome-desktop-press 1))

(define*-public (gnome-desktop-click-2)
  "Send a button-press 1 and button-release 2 to the gnome desktop manager."
  (interactive)
  (gnome-desktop-press 2))

(define*-public (gnome-desktop-click-3)
  "Send a button-press 1 and button-release 3 to the gnome desktop manager."
  (interactive)
  (gnome-desktop-press 3))

(define-public bpress_win #f)
(define-public bpress_win_id #f)

(define (announce-gnome-hint-support)
  (set! bpress_win (make-message-window ""))
  (set! bpress_win_id (message-window-id bpress_win))
  (X-property-set! 'root-window _WIN_SUPPORTING_WM_CHECK
                   (vector bpress_win_id) "CARDINAL" 32)
  (X-property-set! 'root-window "_WIN_DESKTOP_BUTTON_PROXY" 
		   (vector bpress_win_id) "CARDINAL" 32)
  (X-property-set! bpress_win_id "_WIN_SUPPORTING_WM_CHECK"
                   (vector bpress_win_id) "CARDINAL" 32)
  (X-property-set! bpress_win_id "_WIN_DESKTOP_BUTTON_PROXY" 
		   (vector bpress_win_id) "CARDINAL" 32)

  (gnome-set-protocols! gnome-supported-protocols))

(define (unannounce-gnome-hint-support)
  (X-property-delete! 'root-window "_WIN_SUPPORTING_WM_CHECK")
  (X-property-delete! 'root-window "_WIN_PROTOCOLS")
  (X-property-delete! 'root-window "_WIN_DESKTOP_BUTTON_PROXY")
  (set! bpress_win_id #f)
  (set! bpress_win #f))

(define-public (enable-gnome-hints)
  "Enable support for GNOME hints.
GNOME hint support allows for better integration with the GNOME
desktop environment, including support for the GNOME panel and 
pager applet. See also `disable-gnome-hints'."
  (announce-gnome-hint-support)
  (map (lambda (win)
	 (gnome-init-window-from-props win))
       (list-all-windows))
  (gnome-init-workspace-params *gnome-desktop-number*)
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
  (add-hook! shutdown-hook (lambda args  (disable-gnome-hints))))

#!
(define (client-message-debug win atom format vector)
  (display "Got client message on ")
  (display win)
  (newline))

(add-hook! client-message-hook client-message-debug)
!#


(define-public (disable-gnome-hints)
  "Disable support for GNOME hints.
GNOME hint support allows for better integration with the GNOME
desktop environment, including support for the GNOME panel and 
pager applet. Reverses the effect of `enable-gnome-hints'."
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
  (remove-hook! shutdown-hook disable-gnome-hints))


;;; Do not enable by default - let the user reset some parameters first
;;; (enable-gnome-hints)

(define-public gnome-sm
  (->bool (X-property-get 'root-window "GNOME_SM_PROXY")))

(define-public (gnome-install-window-styles)
  "Initialize various window styles for GNOME support.
This handles desktop icons, splash screens, and the panel."
;;; gmc desktop icons
  (window-style "desktop_icon" 
		#:use-style desk-icon 
		#:placement-proc use-placement-hint)
  
  (window-style "panel"
		#:use-style desk-widget #:border-width 0 #:no-titlebar #t)
  
  (for-each (lambda (resource)
	      (window-style resource
			    #:use-style desk-widget #:no-titlebar #t
			    #:placement-proc place-at-center))
	    (list 
	     "gnome-splash"
	     "guname"))
  
  (window-style "wm-properties-capplet"
		#:use-style desk-widget #:no-titlebar #t
		#:transient-placement-proc (at-point-placement #:offset '(60 45))))


;; Hint properties
(define-scwm-option *gnome-desktop-number* 4
  "The number of desktops to show in the GNOME pager.
Used in `enable-gnome-hints'."
  #:type 'integer
  #:group 'gnome
  #:range '(1 . 10)
  #:setter (lambda (desks)
	     (set! *gnome-desktop-number* desks)
	     (gnome-init-workspace-params *gnome-desktop-number*))
  #:favorites '(1 2 3 4 5))
