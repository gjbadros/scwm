/* $Id$ 
 * color.h
 */


#ifndef COLOR_H
#define COLOR_H

#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

extern long scm_tc16_scwm_color;

#define COLOR_P(X) (SCM_NIMP(X) && SCM_CAR(X) == (SCM)scm_tc16_scwm_color)
#define COLOR(X)  ((Pixel)SCM_CDR(X))
#define SAFE_COLOR(X) (COLOR_P((X))?COLOR((X)):0)

#define COLOR_OR_SYMBOL_P(x) (COLOR_P((x)) || gh_symbol_p((x)))

#define DYNAMIC_COLOR_P(X) (gh_symbol_p((X))? \
			    COLOR_P(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			    COLOR_P((X)))

#define DYNAMIC_SAFE_COLOR(X) (gh_symbol_p((X))? \
			       SAFE_COLOR(scm_symbol_binding(SCM_BOOL_F,(X))) : \
			       SAFE_COLOR((X)))

int print_color(SCM obj, SCM port, scm_print_state * pstate);
SCM load_color(SCM cname);
SCM set_hilight_colors(SCM fg, SCM bg);
SCM set_menu_colors(SCM fg, SCM bg, SCM stipple);
SCM color_p(SCM obj);

#endif /* COLOR_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
