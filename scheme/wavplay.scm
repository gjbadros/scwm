;;; $Id$
;;; (C) 1999 Greg J. Badros

(define-module (app scwm wavplay)
  :use-module (app scwm esdsound)
  :use-module (app scwm defoption))

(define-scwm-option *sounds-dir* (or (getenv "MEDIA") 
				     (string-append 
				      (or (getenv "SCWMDIR") (scwm-path-prefix))
				      "/Media/"))
  "Directory containing sounds to be played by `wavplay' procedure."
  #:type 'directory
  #:group 'file-locations)

(define-scwm-option *external-wav-player* "wavplay"
  "A program to exec that takes a single argument, a WAV sound file, and will play it.
This is only used if the Scwm esdsound module is not built."
  #:type 'command
  #:group 'system)

(if (scwm-module-loaded? '(app scwm esdsound))
    (define-public (wavplay filename)
      (sound-play (sound-load (string-append *sounds-dir* filename))))
    (define-public (wavplay filename)
      (system (string-append "wavplay " *sounds-dir* filename "&"))))
