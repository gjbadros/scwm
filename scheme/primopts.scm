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

(define-module (app scwm primopts)
  :use-module (app scwm optargs)
  :use-module (app scwm time-convert)
  :use-module (app scwm base)
  :use-module (app scwm defoption)
  :use-module (app scwm style)
  )

;; (set! scwm-option-variables '())
;; scwm-option-variables
;; (map (lambda (v) (scwm-option-name (eval v))) scwm-option-variables)

(define-scwm-group virtual "Virtual")

;; (popup-option-menu *desk-width*)
(define-scwm-option *desk-width* (car (desk-size))
  "The virtual desktop width, in units of physical screen size."
  #:type 'integer
  #:group 'virtual
  #:range '(1 . 10)
  #:favorites '(1 2 3 4 5)
  #:setter (lambda (x) (set-desk-size! x (cadr (desk-size))))
  #:getter (lambda () (car (desk-size))))

;; (popup-option-menu *desk-height*)
(define-scwm-option *desk-height* (cadr (desk-size))
  "The virtual desktop height, in units of physical screen size."
  #:type 'integer
  #:group 'virtual
  #:range '(1 . 10)
  #:favorites '(1 2 3 4 5)
  #:setter (lambda (y) (set-desk-size! (car (desk-size)) y))
  #:getter (lambda () (cadr (desk-size))))

;; (popup-option-menu *edge-x-scroll*)
(define-scwm-option *edge-x-scroll* (edge-x-scroll)
  "The virtual scrolling in the horizontal direction."
  #:type 'percent
  #:group 'virtual
  #:favorites '(100 50 20 5 1 0)  ;; could extend this to have preferred print formats
  #:setter (lambda (x) (set-edge-x-scroll! (%x x)))
  #:getter (lambda () (pix->%x (edge-x-scroll)))
  )

;; (popup-option-menu '*edge-y-scroll*)
(define-scwm-option *edge-y-scroll* (edge-y-scroll)
  "The virtual scrolling in the vertical direction."
  #:type 'percent
  #:group 'virtual
  #:favorites '(100 50 20 5 1 0)  ;; could extend this to have preferred print formats
  #:setter (lambda (y) (set-edge-y-scroll! (%y y)))
  #:getter (lambda () (pix->%y (edge-y-scroll)))
  )

(define-scwm-option *edge-scroll-delay* (edge-scroll-delay)
  "The edge scroll delay in milliseconds.
See `set-edge-scroll-delay!'. "
  #:type 'integer
  #:range '(0 . 5000)
  #:group 'virtual
  #:favorites '(0 250 500 1000)
  #:setter set-edge-scroll-delay!
  #:getter edge-scroll-delay
  )

(define-scwm-option *edge-move-threshold* (edge-move-threshold)
  "The edge move threshold in pixels.
See `set-edge-move-threshold!'. "
  #:type 'integer
  #:range '(0 . 200)
  #:group 'virtual
  #:favorites '(0 2 5 10 25)
  #:setter set-edge-move-threshold!
  #:getter edge-move-threshold
  )

(define-scwm-group focus "Focus")

(define-scwm-option *default-focus-style* 'mouse
  "The default focus style for windows.
See `set-window-focus!'."
  #:type 'enum
  #:group 'focus
  #:favorites '(click mouse sloppy)  ;; none is an option for windows, but stupid as a default
  #:setter (lambda (v) (window-style "*" #:focus v) (set! *default-focus-style* v)))

(define-scwm-option *highlight-foreground* "white"
  "The foreground (text) color for the titlebar of the window with the keyboard focus."
  #:type 'color
  #:group 'face
  #:setter (lambda (v) (set-highlight-foreground! v))
  #:getter (lambda () (highlight-foreground)))

(define-scwm-option *highlight-background* "navy"
  "The background color for the titlebar of the window with the keyboard focus."
  #:type 'color
  #:group 'face
  #:setter (lambda (v) (set-highlight-background! v))
  #:getter (lambda () (highlight-background)))
