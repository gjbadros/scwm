;;;; $Id$
;;;; Copyright (C) 1999 Greg J. Badros
;;;; Modified from ScwmButtons.scm, (C) 1998 Maciej Stachowiak
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



(define-module (app scwm ui-constraints-buttons)
  :use-module (app scwm gtk)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (app scwm ui-constraints)
  :use-module (app scwm ui-constraints-classes)
  :use-module (app scwm optargs))

;; (load "/scratch/gjb/scwm/scheme/ui-constraints-buttons.scm")
;; (set-current-module the-root-module)
;; (start-ui-constraints-buttons)


;; example usage: 
;; (window-style (resource-match?? "ScwmUIConstraintsButtons") #:use-style desk-widget-on-top-no-titlebar)
;; (use-modules (app scwm ui-constraints-buttons))
;; (start-ui-constraints-buttons)
;; other testing:
;; (use-modules (app scwm ui-constraints))
;; (draw-all-constraints)

(define tooltips (gtk-tooltips-new))

(define-public (start-ui-constraints-buttons)
  (let* ((ui-constraint-classes global-constraint-class-list)
	 (cn-ui-ctrs (map ui-constraint-class-ui-ctr ui-constraint-classes))
	 (cn-names (map ui-constraint-class-name ui-constraint-classes))
	 (cn-buttons (map (lambda (n) (gtk-button-new)) cn-names))  ;; was gtk-button-new-with-label
;;	 (cn-buttons (map gtk-button-new-with-label cn-names))
	 (cn-pixmaps (map (lambda (c b) (gtk-pixmap-new-search-scwm-path (ui-constraint-class-pixmap-name c) b))
			  ui-constraint-classes cn-buttons))
	 (toplevel (gtk-window-new 'toplevel))
	 (hbox (gtk-hbutton-box-new)))
    (for-each (lambda (b tip)
		(gtk-tooltips-set-tip tooltips b tip ""))
	      cn-buttons cn-names)
    (gtk-button-box-set-spacing hbox 0)
    (gtk-button-box-set-child-ipadding hbox 0 0)
    (gtk-button-box-set-child-size hbox 32 32)
    (gtk-window-set-title toplevel "ScwmUIConstraintsButtons")
    (gtk-window-set-wmclass toplevel "ScwmUIConstraintsButtons" "Scwm")
    (gtk-container-add toplevel hbox)
    (gtk-container-border-width toplevel 0)
    (for-each (lambda (b) (gtk-box-pack-start hbox b)) cn-buttons)
    (for-each (lambda (b p l) 
		(if p
		    (gtk-container-add b p)
		    (gtk-container-add b (gtk-label-new l))))
	      cn-buttons cn-pixmaps cn-names)
    (for-each (lambda (b d)
		(gtk-signal-connect b "clicked"
				    (lambda ()
				      (enable-ui-constraint (make-ui-constraint-interactively d)))))
	      cn-buttons ui-constraint-classes)
    (for-each (lambda (p) (if p (gtk-widget-show p))) cn-pixmaps)
    (for-each gtk-widget-show cn-buttons)
    (gtk-widget-show hbox)
    (gtk-widget-show toplevel)
    ;; return the close procedure
    (lambda ()
      (if (not (gtk-widget-destroyed toplevel))
	  (gtk-widget-hide toplevel)
	  (gtk-widget-destroy toplevel)))))

(define-public (close-ui-constraints-buttons sdb)
  (sdb))
