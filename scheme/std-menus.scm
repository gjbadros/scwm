;;; File: <std-menus.scm - 1998-05-27 Wed 12:08:16 EDT sds@mute.eaglets.com>
;;;; 	Copyright (C) 1998 Sam Steingold and Maciej Stachowiak

;;;	$Id$

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



(define-module (app scwm std-menus)
  :use-module (app scwm base)
  :use-module (app scwm flux)
  :use-module (app scwm optargs))



;;; --------------------------------------
;;; The screen saver and screen lock menus
;;; --------------------------------------
(define-public screensaver-modes
  '("ant" "ball" "bat" "blot" "bouboule" "bounce" "braid" "bug" "bubble"
    "cartoon" "clock" "coral" "crystal" "daisy" "dclock" "deco" "demon"
    "dilemma" "drift" "eyes" "fadeplot" "flag" "flame" "forest" "galaxy"
    "grav" "helix" "hop" "hyper" "ico" "ifs" "image" "julia" "kaleid"
    "laser" "life" "life1d" "life3d" "lightning" "lisa" "lissie" "loop"
    "mandelbrot" "marquee" "maze" "mountain" "munch" "nose" "pacman"
    "penrose" "petal" "puzzle" "pyro" "qix" "roll" "rotor" "shape"
    "sierpinski" "slip" "sphere" "spiral" "spline" "star" "strange" "swarm"
    "swirl" "triangle" "tube" "turtle" "vines" "voters" "wator" "wire"
    "world" "worm"))

(define-public xlock-options
  "-nice -19 +mousemotion +timeelapsed -lockdelay 600 -timeout 30")
(define (run-xlock mode lock)	; returns a lambda!
  (exe (string-append "xlock " xlock-options " -mode " mode
                      (if lock "" " -nolock"))))

;;; to use this, add the following to the menu of your choice:
;;;   (menuitem "Screensaver" #:action (make-xlock-menu #f))
;;; or
;;;   (menuitem "Lock Screen" #:action (make-xlock-menu #t))
(define*-public (make-xlock-menu #&optional (lock? #f))
  (menu (append!
	 (list (menuitem "Lock Screen" #f) menu-title menu-separator
	       (menuitem "Random!" #:action (run-xlock "random" lock?))
	       (menuitem "Blank" #:action (run-xlock "blank" lock?))
	       (menuitem "Bomb" #:action (run-xlock "bomb" lock?)))
	 (fold-menu-list
	  (map (lambda (str) (menuitem str #:action (run-xlock str lock?)))
	       screensaver-modes)))))

;;; to use this, add the following to the menu of your choice:
;;; (menuitem "telnet" #:action (menu-hosts '("host1" "host2" ...)))
(define*-public (make-hosts-menu host-list #&optional (user USER))
  (menu (fold-menu-list
         (map (lambda (hh)
               (menuitem hh #:action
                         (run-in-xterm
                          (string-append "-T telnet:" hh " -n telnet "
                                         "-e telnet -E -l " user " " hh))))
             host-list))))

