;;;; $Id$
;;;; Copyright (C) 1999, 2000 Francesco Tapparo, Greg J. Badros
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
;;;; END OF LICENSE
;;;;
;;;;   25/12/1999       Francesco Tapparo
;;;;                    gtk-message use a label widget to calculate 
;;;;                    automatically the dimensions (trick stolen from
;;;;                    xmessage)
;;;;                    message is a #:key value; removed xsize and ysize
;;;;                    the close button is more small


(define-module (app scwm gtk-message)
  :use-module (app scwm base)
  :use-module (app scwm optargs)
  :use-module (gtk gtk))

(define* (quit-toolbar win #:optional (close-message "close"))
  (let* ((bar (gtk-alignment-new 0.5 0.5 0 0))
	 (quit-button (gtk-button-new-with-label close-message)))
    (gtk-signal-connect quit-button "clicked" (lambda ()
						(gtk-widget-destroy  win)))
    (gtk-container-add bar quit-button)
    bar))


;; (gtk-message "Hello world" #:close-message "Ok" #:title "Alert")
(define*-public (gtk-message message #:key (close-message "close") (title "message"))
  "It display a message on the display, using a gtk window
MESSAGE is a string representing the message to be displayed on the screen."
  (let* ((msgwin (gtk-window-new 'dialog))
	 (msg (gtk-label-new message))
	 (box (gtk-vbox-new #f 1)))
    (gtk-window-set-title msgwin title)
    (gtk-container-add msgwin box)
    (gtk-container-add box msg)
    (gtk-hseparator-new)
    (gtk-container-add box (quit-toolbar msgwin close-message))
    (gtk-widget-show-all msgwin)))
