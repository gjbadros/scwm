;;;; $Id$


(define b (gtk-button-new))
(define p (gtk-pixmap-new "test.xpm" b))

;; (use-scwm-modules (gtk gtk))
;; (use-scwm-modules (gtk gdk))
;; (use-scwm-modules preferences)
;; (use-scwm-modules )

(define-public (start-pixmap-button-window)
  (let* ((button (gtk-button-new))
	 (toplevel (gtk-window-new 'toplevel))
	 (hbox (gtk-hbox-new 0 0)))
    (gtk-window-set-title toplevel "PixmapButtonTest")
    (gtk-window-set-wmclass toplevel "PixmapButtonTest" "Scwm")
    (gtk-container-add toplevel hbox)
    (gtk-box-pack-start hbox button)
    (gtk-signal-connect button "clicked" (lambda () (display "clicked\n")))
    (gtk-signal-connect button "realize" (lambda () 
					   (let ((pixmap (gtk-pixmap-new "test.xpm" button)))
					     (gtk-widget-show pixmap)
					     (gtk-container-add button pixmap))))
    (gtk-widget-show button)
    (gtk-widget-show hbox)
    (gtk-widget-show toplevel)))

(start-pixmap-button-window)
