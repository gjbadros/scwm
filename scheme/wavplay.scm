;;; $Id$
;;; Copyright (C) 1999, 2000 Greg J. Badros

(define-module (app scwm wavplay)
  :use-module (app scwm esdsound)
  :use-module (app scwm defoption))

;;(wavplay "scwm-startup.wav")

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

;; (define filename "scwm-startup.wav")
(if (and (defined? 'sound-play) (defined? 'sound-load))
    (define-public (wavplay filename)
      (let ((snd (sound-load (string-append *sounds-dir* filename))))
	(and snd (sound-play snd))))
    (define-public (wavplay filename)
      (system (string-append *external-wav-player*
			     " " *sounds-dir* filename "&"))))
