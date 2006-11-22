;;;; $Id$
;;;; Copyright (C) 1999 Michael Schmitz <mschmitz@iname.com>
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


;; This module serves setting up menus from directories that represent
;; either Gnome or KDE menus. It contains the following functions of
;; interest:
;;
;; (make-menu-gnome-system #:optional path title)
;;     returns the Gnome System menus (the one you get as top-level
;;     directory in the panel)
;;
;; (make-menu-another-level #:optional path title)
;;     returns the AnotherLevel menus
;;
;; (make-menu-gnome-user #:optional
;;			       (path "/usr/share/applnk")
;;			       (title "KDE Menus"))
;; (make-menu-from-structured-directory path title))



;;(get-name-of-menuitem '("Desktop" (key-name . "Desktop")))
;;(get-name-of-menuitem '((key-name . "URL Handlers") (key-file . "url-properties.desktop")))
;;(get-name-of-menuitem '((key-file . "url-properties.desktop")))
;;(get-name-of-menuitem '((key-f . "url-properties.desktop")))

;;(sort-alphabetically '(("Peripherals" (key-name . "Peripherals")) (key-item ((key-name . "URL Handlers") (key-file . "url-properties.desktop"))) (key-item ((key-name . "Menu editor") (key-file . "gmenu.desktop")))))

;;(sort-according-to-order-spec '(key-order ("gnomecc.desktop" "Desktop" "Multimedia" "Imlib_config.desktop")) '(("Desktop" (key-name . "Desktop")) ("Multimedia" (key-name . "Multimedia")) (key-item ((key-name . "GNOME Control Center") (key-file . "gnomecc.desktop"))) (key-item ((key-name . "Imlib Configuration Options") (key-file . "Imlib_config.desktop")))))

;;(readdir-rec "/usr/share/gnome/apps")
;;(popup-menu (make-menu-gnome-system))
;;(popup-menu (make-menu-gnome-user))
;;(popup-menu (make-menu-another-level))
;;(popup-menu (make-menu-kde))
;;(popup-menu (make-menu-from-structured-directory (string-append (getenv "HOME") "/.gnome/apps-redhat")))
;;(popup (make-menu-from-list-of-structured-directories
;;	     (list "/usr/share/gnome/apps" (string-append (getenv "HOME") "/.gnome/apps-redhat") "/usr/share/applnk")
;;	     "TEST"
;;	     (list "System Menus" "AnotherLevel Menus" "KDE Menus")))
