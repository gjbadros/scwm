;; with the current scwm from CVS, and guile-gtk-0.10 and gtk-1.0.5 from
;; ftp.gtk.org, typing the following to a scwmrepl results in a very
;; interesting window:
;; Maciej --08/02/98

(begin
  (use-modules (app scwm gtk) (gtk gtk))
  
  (define w (gtk-window-new 'toplevel))
  (define b (gtk-button-new-with-label "Name: "))
  (define e (gtk-entry-new))
  (gtk-signal-connect b "clicked" 
		      (lambda ()
			(display "Entry text is: ")
			(display (gtk-entry-get-text e))
			(newline)))
  (define hb (gtk-hbox-new 0 0))
  (gtk-container-add w hb)
  (gtk-box-pack-start hb b)
  (gtk-box-pack-end hb e)
  (gtk-widget-show e)
  (gtk-widget-show b)
  (gtk-widget-show hb)
  (gtk-widget-show w))

(gtk-widget-unmap w)
