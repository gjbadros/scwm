;; with the current scwm from CVS, and guile-gtk-0.10 and gtk-1.0.5 from
;; ftp.gtk.org, typing the following to a scwmrepl results in a very
;; interesting window:
;; Maciej --08/02/98

(begin
  (use-modules (gtk gtk))
  
  (define w (gtk-window-new 'toplevel))
  (define b (gtk-button-new-with-label "Name: "))
  (define e (gtk-entry-new))
  (gtk-container-add w b)
  (gtk-container-add w e)
  (gtk-widget-show e)
  (gtk-widget-show b)
  (gtk-widget-show w)
  (while (not (= 0 (gtk-events-pending))) (gtk-main-iteration))
  (while (not (= 0 (gtk-events-pending))) (gtk-main-iteration))
  (while (not (= 0 (gtk-events-pending))) (gtk-main-iteration)))

(gtk-widget-unmap w)

(define (handle-gtk-events)
  (while (not (= 0 (gtk-events-pending))) (gtk-main-iteration))
  (add-timer-hook! 10000 handle-gtk-events))

(define gtk-loop-timer (add-timer-hook! 10000 handle-gtk-events))
