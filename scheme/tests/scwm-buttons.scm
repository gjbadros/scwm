;; $Id$

;; Start a mini-button bar using ScwmButtons 
(define btns 
  (run-ScwmButtons 
   (list 
    (button-item "mini-term.xpm" #:action "xterm" #:tooltip "Spawn an XTerm") 
    (button-item "mini-edit.xpm" #:action "xemacs" #:tooltip "Xemacs Editor") 
    (button-item "mini-calc.xpm" #:action "xcalc" #:tooltip "Calculator") 
    (button-item "mini-xmcd.xpm" #:action "xmcd" #:tooltip "CD Player") 
    (button-item "mini-audiovol.xpm" #:action "timidity -ig -Os" 
                 #:tooltip "Midi Player") 
    (button-item "mini-xv.xpm" #:action "xv" #:tooltip "Image Viewer") 
    (button-item "mini-gv.xpm" #:action "gv" #:tooltip "Postscript Viewer") 
    (button-item "mini-nscape.xpm" #:action "/usr/netscape/netscape" 
                 #:tooltip "Netscape Web Browser") 
    ) #:name "MiniButtons")) 
 
;; Start biff stuff 
(define guibiff (run-ScwmBiff "mini-mail.xpm" "mini-newmail.xpm" 
                              #:action "privtool" 
                              #:parent btns "Mail Notifier")) 
 
(define biff (simplebiff #:activate-proc (lambda () (activate-ScwmBiff guibiff)) 
                         #:deactivate-proc (lambda () (deactivate-ScwmBiff guibi 
ff)))) 
 
(btns 'add-space) 
