;; $Id$
;; Jeff W. Nichols
;;
;; This module defines a globally available message window.  This window
;; is created once (hopefully at startup) and can be used by anyone.  
;; Currently it is used by the move-with-msgwin and resize-with-msgwin
;; modules.
;;
;;

(define-module (app scwm move-resize-message-window))

(define-public move-resize-message-window (make-message-window ""))

