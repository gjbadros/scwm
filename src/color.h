/* $Id$ 
 * color.h
 */


#ifndef COLOR_H
#define COLOR_H

#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

extern long scm_tc16_scwm_color;

#define COLORP(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_color)
#define COLOR(X)  ((Pixel)SCM_CDR(X))

int print_color(SCM obj, SCM port, scm_print_state * pstate);
SCM load_color(SCM cname);
SCM set_hilight_colors(SCM fg, SCM bg);
SCM set_menu_colors(SCM fg, SCM bg, SCM stipple);
SCM color_p(SCM obj);

#endif /* COLOR_H */
