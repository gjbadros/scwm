(use-scwm-modules fvwm-module)

(register-fvwm2-module-config
 "FvwmPager"
 "*FvwmPagerBack lightsteelblue"
 "*FvwmPagerFore black"
 "*FvwmPagerHilight moccasin"
 "*FvwmPagerFont none"
 "*FvwmPagerDeskTopScale 32"
 "*FvwmPagerGeometry 156x58-440-0"
 "*FvwmPagerLabel 0 Bureau"
 "*FvwmPagerSmallFont 5x8"
 )

(register-fvwm2-module-config
 "FvwmButtons"
 "*FvwmButtonsFore Black"
 "*FvwmButtonsBack Lightsteelblue"
 "*FvwmButtonsFont -adobe-helvetica-medium-r-*-*-8-*-*-*-*-50-iso8859-1"
 "*FvwmButtonsGeometry -0-0"
 "*FvwmButtonsRows 1"
 "*FvwmButtons(4x1, Swallow FvwmPager 'Module FvwmPager 0 0')"
 "*FvwmButtons(1x1, Title xterm, \
                  Icon bouton-xterm.xpm, \
                  Action 'Exec \"XTerm\" xterm-')"
 "*FvwmButtons(1x1, Title emacs, \
                  Icon lemacs.xpm, \
                  Action 'Exec \"Emacs\" emacs-serv')"
 "*FvwmButtons(1x1, Title netscape, \
                  Icon mag_glass.xpm, \
                  Action 'Exec \"Netscape\" netscape-server')"
 "*FvwmButtons(1x1, Title ncftp, \
                  Icon disk.xpm, \
                  Action 'Exec \"XTerm\" ncftp-')"
 "*FvwmButtons(1x1, Title mutt, \
                  Icon mail2.xpm, \
                  Action 'Exec \"XTerm\" mutt- -- -geometry 80x56+0+0')"
 "*FvwmButtons(1x1, Title slrn, \
                  Icon dialog_box.xpm, \
                  Action 'Exec \"XTerm\" slrn-')"
 "*FvwmButtons(1x1, Title xlock, \
                  Icon xdbx.xpm, \
                  Action 'Exec \"XLock\" xlock-')"
 "*FvwmButtons(3x1, Swallow(NoHints) xload-button \
                   'Exec xload -title xload-button -nolabel \
                               -geometry 117x58-40-0')"
 "*FvwmButtons(1x1, Swallow(NoHints) xclock-button \
                   'Exec xclock -name xclock-button -padding 0 \
                                -bg lightsteelblue -fg black \
                                -geometry 39x58-0-0')"
 )

(define fvwm2-buttons (run-fvwm2-module "FvwmButtons"))
;; (define fvwm2-pager (run-fvwm2-module "FvwmPager" '("0" "0")))

