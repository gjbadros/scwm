;; $Id$
;; Example of using FvwmButtons module
;; By Greg J. Badros, 2-Feb-1999

(register-fvwm2-module-config 
 "FvwmButtons"
 "PixmapPath /uns/share/include/X11/pixmaps:/scratch/gjb/scwm-icons/mini-icons"
 "*WorkmanButtonsRows 1"
 "*WorkmanButtonsFore black"
 "*WorkmanButtonsBack grey76"
 "*WorkmanButtons(Icon mini-wm-pause.xpm, Action \"exec workman -s pause\")"
 "*WorkmanButtons(Icon mini-wm-play.xpm, Action \"exec workman -s play\")"
 "*WorkmanButtons(Icon mini-wm-next.xpm, Action \"exec workman -s fwd\")"
 "*WorkmanButtons(Icon mini-wm-prev.xpm, Action \"exec workman -s back\")"
 "*WorkmanButtons(Icon mini-wm-stop.xpm, Action \"exec workman -s stop\")"
 "*WorkmanButtons(Icon mini-wm-eject.xpm, Action \"exec workman -s eject\")")

(define workman-buttons (run-fvwm2-module "FvwmButtons" '("WorkmanButtons")))
