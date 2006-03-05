;;;; $Id$
;;;; gtk-about.scm
;;;; Copyright (C) 1999, 2000 Greg J. Badros
;;;;

(define-module (app scwm gtk-about)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  )

;; Largely borrowed from Galway, the Guile HTML editor --12/05/99 gjb
;; GJB:FIXME:: how do I force a startup width, but not height?
(define*-public (gtk-about title text #:optional (pixmap #f) (width 500) (height 400))
  (let* ((window  (gtk-window-new 'dialog))
	 (notebook(gtk-notebook-new))
	 (box     (gtk-vbox-new #f 1))
	 (hbox0   (gtk-hbox-new #f 1))
	 (button0 (gtk-button-new))
	 (button1 (gtk-button-new-with-label "Ok"))
	 (text-widget (gtk-text-new #f #f)))
    (if (string? pixmap) (set! pixmap (gtk-pixmap-new-search-scwm-path pixmap button0)))
    (gtk-signal-connect button1 "clicked"
			(lambda () (gtk-widget-destroy window)))
    (gtk-window-set-title window title)
    (gtk-container-add window box)
    (for-each (lambda (i) (gtk-box-pack-start box i)) (list notebook hbox0))
    (gtk-box-pack-start hbox0 button1 #t)
    (gtk-text-insert text-widget #f "blue" #f text -1)
    (if (gtk-pixmap? pixmap)
	(gtk-notebook-append-page notebook pixmap (gtk-label-new title)))
    (gtk-notebook-append-page notebook text-widget (gtk-label-new "more"))
    (gtk-widget-set-usize window width height)
    (gtk-widget-show-all window)))
  

(define-public (gtk-about-scwm)
  (gtk-about 
   "About Scwm"
   "Scwm:
The Scheme Constraints Window Manager
(C) 1999, 2000 Greg J. Badros and Maciej Stachowiak

See: http://scwm.mit.edu

Scwm is the Scheme Constraints Window Manager, a highly dynamic and
extensible window manager for the X Window System.  Scwm embeds Guile
Scheme as the configuration and extension language, and includes a
sophisticated constraint solver for permitting the user to interactively
specify constraints among top-level window sizes and positions.  Nearly
all decorations can be changed at run-time on a per-window basis.
Dynamic loading of C modules is supported.  Scwm is self-documenting and
provides A powerful protocol for interacting with the window manager
from other processes.

See also the scwm/THANKS file for a complete
list of contributors!."
   "scwm-logo.xpm"))
