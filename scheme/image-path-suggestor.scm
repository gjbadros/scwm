;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm image-path-suggestor))

(define (suggest-image-path-for name)
  (let ((found-if-0 (system (string-append "locate '*/" name "'"))))
    (if (= 0 found-if-0)
	(display (string-append "Perhaps try adding above path to image-load-path\n")))))

(define (handle-image-not-found name) 
  (display (string-append "Could not find image: " name "\n"))
  (suggest-image-path-for name))

(define-public (enable-image-path-suggestor)
  "Turn on the image path suggestor.
This can substantially slow down loading of a .scwmrc if there
are lots of missing images."
  (add-hook! image-not-found-hook handle-image-not-found))

