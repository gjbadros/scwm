;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros
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


;;; SRL:FIXME::I do not claim that all the properties that we might like to
;;; save are saved, but this code is already useful.  This code should save
;;; enough state to get a window back to the same size, position, and
;;; shaded/iconified/sticky state as when it was saved.  Icon position and
;;; keep-on-top status are not saved.  Everything else is not saved.  In
;;; particular focus style, hints, other actions configuration items, all
;;; decoration state, and anything else I forgot are NOT saved.  This code is
;;; currently broken in many, many ways.  This is mostly because the needed
;;; C primitives are missing or malfunctioning.  This code works for position
;;; of non-iconified window, size of non-iconified window, iconified state, and
;;; sticky state.  All other state is usually lost or causes strange effects.
;;; The code attempts to store shaded state but the shading stuff is broken
;;; in several ways.  The window-configuration needs to have the icon position
;;; added to it and probably keep on top status.

;;; SRL:FIXME::Should save which desk the window is on.

(define-module (app scwm window-configuration)
  :use-module (app scwm wininfo)
  :use-module (app scwm optargs)
  :use-module (app scwm animation)
  :use-module (app scwm listops)
  :use-module (app scwm base))

;;; window configuration (use win-cfg-* functions to access subparts)
;;;   window
;;;   id
;;;   position (either desktop or viewport relative)
;;;   frame size
;;;   iconified?
;;;   sticky-window?
;;;   icon-sticky?
;;;   shaded?

(define*-public (window-configuration #&optional (win (get-window)))
  "Return a list containing the state of WIN.
Use the win-cfg-* functions to access parts of the configuration.
Elements of the configuration may move at any time and the type of
a configuration may change at any time."
  (with-window 
   win
   (list
    win                 ; 0
    (window-id)         ; 1
    (window-position)   ; 2
    (window-frame-size) ; 3
    (iconified-window?) ; 4
    (sticky-window?)    ; 5
    (icon-sticky?)      ; 6
    (shaded-window?)    ; 7
    )))

(define-public (win-cfg-window cfg)
  "Return the window from which CFG was recorded."
  (list-ref cfg 0))

(define-public (win-cfg-window-id cfg)
  "Return the window id from which CFG was recorded."
  (list-ref cfg 1))

(define-public (win-cfg-window-position cfg)
  "Return the window position from CFG.
This is relative to the desktop unless win-cfg-window-sticky? is
true in which case it is relative to the viewport."
  (list-ref cfg 2))

(define-public (win-cfg-window-frame-size cfg)
  "Return the window frame size from CFG."
  (list-ref cfg 3))

(define-public (win-cfg-iconified-window? cfg)
  "Return the whether the window was iconified in CFG."
  (list-ref cfg 4))

(define-public (win-cfg-sticky-window? cfg)
  "Return the whether the window was sticky in CFG."
  (list-ref cfg 5))

(define-public (win-cfg-icon-sticky? cfg)
  "Return the whether the window's icon was sticky in CFG."
  (list-ref cfg 6))

(define-public (win-cfg-shaded-window? cfg)
  "Return the whether the window was shaded in CFG."
  (list-ref cfg 7))

(define-public (window-configuration? wcfg)
  "Return #t if WCFG is a window configuration."
  (and (pair? wcfg)
       (window? (car wcfg))
       (pair? (cdr wcfg))
       (number? (cadr wcfg))
       (= (length wcfg) 8)))

;; (define c (window-configuration))

(define*-public (window-configuration->xform-to-it win cfg)
  "Create a transformation for going from the current state of win to CFG.
CFG should be a window configuration object.  See also `animate-windows'."
  (let* ((icon? (iconified-window? win))
         (sticky? (if icon? (icon-sticky? win) (sticky-window? win)))
         (pos (window-viewport-position win))
	 (size (window-frame-size win))
	 (startX (car pos))
	 (startY (cadr pos))
	 (startW (car size))
	 (startH (cadr size))
	 (endPos (win-cfg-window-position cfg))
	 (endSize (win-cfg-window-frame-size cfg))
	 (endX (if sticky? (car  endPos) (vx->vpx (car  endPos))))
	 (endY (if sticky? (cadr endPos) (vy->vpy (cadr endPos))))
	 (endW (car endSize))
	 (endH (cadr endSize))
	 )
    (list win (not icon?)
	  (cons startW startH)
	  (cons endW endH)
	  (cons startX startY)
	  (cons endX endY)
	  (cons #t #t))))

(define*-public (apply-window-configurations win-configs)
  "Apply the window configurations in WIN-CONFIGS.
WIN-CONFIGS should be a list of elements of the form
(list window window-configuration)."
  ;; get iconify state of window to match configuration
  (map
   (lambda (win-configuration)
     (let ((win (list-ref win-configuration 0))
           (configuration (list-ref win-configuration 1)))
       (if (not (eq? (iconified-window? win) (win-cfg-iconified-window? configuration)))
           (if (win-cfg-iconified-window? configuration)
               (iconify-window win)
               (deiconify-window win)))))
   win-configs)
  ;; get shaded state of window to match configuration
  (map
   (lambda (win-configuration)
     (let ((win (list-ref win-configuration 0))
           (configuration (list-ref win-configuration 1)))
       (if (not (eq? (shaded-window? win) (win-cfg-shaded-window? configuration)))
           (if (win-cfg-shaded-window? configuration)
               (shade-window win)
               (unshade-window win)))))
   win-configs)
  ;; get stickiness of window to match configuration
  (map
   (lambda (win-configuration)
     (let ((win (list-ref win-configuration 0))
           (configuration (list-ref win-configuration 1)))
       (if (not (eq? (sticky-window? win) (win-cfg-sticky-window? configuration)))
           (if (win-cfg-sticky-window? configuration)
               (stick-window win)
               (unstick-window win)))
       (if (not (eq? (icon-sticky? win) (win-cfg-icon-sticky? configuration)))
           (if (win-cfg-icon-sticky? configuration)
               (stick-icon win)
               (unstick-icon win)))))
   win-configs)
  ;; get position of window in other iconification state to match configuration
  ;;(map
  ;; (lambda (win-configuration)
  ;;   (let ((win (list-ref win-configuration 0))
  ;;         (configuration (list-ref win-configuration 1)))
  ;;     (if (iconified-window? win)
  ;;         (let* ((wc-win-pos (win-cfg-window-position configuration))
  ;;                (wc-win-x (car wc-win-pos))
  ;;                (wc-win-y (cadr wc-win-pos)))
  ;;           (if (not (equal? (window-position win) wc-win-pos))
  ;;               (if (sticky-window? win)
  ;;                   ;; SRL:FIXME::This doesn't work because move-window on iconified-window
  ;;                   ;;  changes position of icon not window when expanded.  No function to
  ;;                   ;;  use here.  Needs C fix.
  ;;                   (move-window-viewport-position wc-win-x wc-win-y win)
  ;;                   (move-window                   wc-win-x wc-win-y win))))
  ;;         ; SRL:FIXME::move icon to correct position if necessary here.  Need an
  ;;         ;   icon move function to do this.
  ;;         )))
  ;; win-configs)
  ;; get position/size of window to match configuration
  (animate-windows
   (map 
    (lambda (win-configuration)
     (let ((win (list-ref win-configuration 0))
           (configuration (list-ref win-configuration 1)))
        (window-configuration->xform-to-it win configuration)))
    ;; SRL:FIXME:: Have to remove iconified windows here since animate-windows
    ;;   doesn't handle them properly.
    (filter
     (lambda (win-configuration)
       (let ((win (list-ref win-configuration 0)))
         (not (iconified-window? win))))
     win-configs))))

;; GJB:FIXME:: make animation optional.
(define*-public (copy-window-configuration configuration #&optional (win (get-window)))
  "Apply a saved state CONFIGURATION to window WIN."
  (apply-window-configurations (list (list win configuration))))

;; (copy-window-configuration c)

(define*-public (restore-window-configuration global-configuration 
					     #&optional (win (get-window)))
  "Restore the state of WIN from GLOBAL-CONFIGURATION."
  (let ((c (assoc win global-configuration)))
    (if c (begin (copy-window-configuration c win) #t) #f)))

;; SRL:FIXME::What about Iconified?, Sticky?, ...
;; Fails for sticky windows unless this happens to be called while we
;;   are in the upper left corner of the virtual desktop
(define-public (global-window-configuration)
  "Return an object abstracting all of the current windows' states."
  (map window-configuration (list-stacking-order)))

;; (define gc (global-window-configuration))
;; (define win (get-window))
;; (define cfg (window-configuration (get-window)))
;; (window-configuration->xform-to-it cfg)

(define-public (restore-global-window-configuration global-configuration)
  "Restore the states of all windows from GLOBAL-CONFIGURATION."
  (apply-window-configurations
   ;; SRL:FIXME:: Ugh. O(n^2) for n windows
   (filter-map 
    (lambda (w) 
      (let ((cfg (assoc w global-configuration)))
	(if cfg
	    (list w cfg)
	    #f)))
    (list-stacking-order)))
  (restack-windows (map car global-configuration)))

(define*-public (push-window-configuration #&optional (win (get-window)))
  "Save the configuration of WIN on its stack of previous configurations."
  (interactive)
  (set-window-property! 
   win 'window-configuration-stack
   (cons 
    (window-configuration win)
    (or (window-property win 'window-configuration-stack) '()))))

				      
(define*-public (pop-window-configuration #&optional (win (get-window)))
  "Restore the last configuration of WIN that was saved on its stack of previous configurations."
  (interactive)
  (let ((config (window-property win 'window-configuration-stack)))
    (if (and config (not (null? config)))
	(begin
	  (copy-window-configuration (car config) win)
	  (set-window-property! 
	   win
	   'window-configuration-stack 
	   (cdr config))))))

(define-public window-configuration-menu
  (menu
   (list
    (menu-title "Window configuration")
    menu-separator
    (menuitem "&Push" #:action push-window-configuration)
    (menuitem "P&op" #:action pop-window-configuration))))

;; (push-window-configuration)
;; (pop-window-configuration)
;; (popup-menu window-configuration-menu)
